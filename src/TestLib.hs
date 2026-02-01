module TestLib (
    TestTree,
    Test,
    test,
    testLazy,
    TestName,
    getTestTree,
    displayTestResultsConsole,
    TestRunnerOpts (..),
    runTests,
    assertEqual,
    assert,
    assertFailure,
    golden,
)
where

import Control.DeepSeq (NFData)
import Control.DeepSeq qualified as DeepSeq
import Control.Exception (SomeException, evaluate)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadThrow, try)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bitraversable
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Traversable
import Data.Tree
import System.Console.ANSI
import System.Directory
import System.FilePath

data TestTree m input where
    TestTree :: TestName -> TestCase m input output -> [TestTree m output] -> TestTree m input

data TestCase m input output where
    TestCase :: (NFData output) => (input -> Test m output) -> TestCase m input output
    TestCaseLazy :: (input -> Test m output) -> TestCase m input output

newtype Test m a
    = Test
        ( ExceptT
            TestFailure
            ( WriterT
                [TestLogItem]
                (ReaderT TestRunnerOpts m)
            )
            a
        )
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadError TestFailure
        , MonadWriter [TestLogItem]
        , MonadReader TestRunnerOpts
        )

-- | See `testLazy` for avoiding the `NFData` constraint.
test :: (NFData output) => Text -> (input -> Test m output) -> [TestTree m output] -> TestTree m input
test n f = TestTree (TestName n) $ TestCase f

{- | This is `test` without the `NFData` constraint.
It doesn't force the output before completion, which means that reported timings may be less accurate.
-}
testLazy :: Text -> (input -> Test m output) -> [TestTree m output] -> TestTree m input
testLazy n f = TestTree (TestName n) $ TestCaseLazy f

data TestResult = TestResult
    { name :: TestName
    , logs :: [TestLogItem]
    , result :: Either TestFailure (NominalDiffTime, [TestResult])
    }

data TestLogItem
    = LogRegeneratedGolden

data TestFailure
    = ExceptionFailure SomeException
    | AssertionFailure Text
    | GoldenMissing
    | GoldenFailure {expected :: Text, actual :: Text}

newtype TestName = TestName Text

getTestTree :: TestTree m r -> Tree Text
getTestTree (TestTree (TestName name) _ ts) = Node name $ map getTestTree ts

displayTestResultsConsole :: Maybe Int -> TestResult -> TL.Text
displayTestResultsConsole terminalWidth testResult =
    displayResult 0 testResult <> TL.pack (setSGRCode [Reset])
  where
    displayResult indent =
        (TL.replicate (fromIntegral indent) "  " <>) . \case
            TestResult{name = TestName name, logs, result} ->
                case result of
                    Right (dt, children) ->
                        TL.fromStrict (header Green '✓' name indent (Just dt) <> displayLogs)
                            <> TL.concat (map (displayResult (indent + 1)) children)
                    Left e ->
                        TL.fromStrict $
                            header Red '✗' name indent Nothing
                                <> displayLogs
                                <> setColour Vivid Red
                                <> indentAllLines indent case e of
                                    ExceptionFailure ex -> "Exception: " <> T.show ex
                                    AssertionFailure t -> "Assertion failed: " <> T.stripEnd t
                                    GoldenMissing -> "Golden file missing"
                                    GoldenFailure{expected, actual} ->
                                        "Expected:\n" <> T.stripEnd expected <> "\nActual:\n" <> T.stripEnd actual
              where
                displayLogs =
                    setColour Dull Magenta
                        <> indentAllLines
                            indent
                            ( flip foldMap logs \case
                                LogRegeneratedGolden -> "Created golden file"
                            )
                        <> setColour Dull Magenta
    header colour icon name indent time =
        setColour Vivid colour
            <> T.singleton icon
            <> " "
            <> setColour Dull White
            <> name
            <> maybe
                mempty
                ( \t@(showTime -> tt) ->
                    T.replicate
                        ( fromIntegral $
                            maybe
                                3
                                (\n -> n - (2 * indent + T.length name + T.length tt + 4))
                                terminalWidth
                        )
                        " "
                        <> setColour Dull Blue
                        <> tt
                        <> " "
                        <> T.singleton (timeBarFunction t)
                )
                time
            <> "\n"
    paddedAllLines p = T.unlines . map (p <>) . T.lines
    indentAllLines indent = paddedAllLines $ T.replicate (indent * 2) " "
    timeBarFunction t
        | t < 0.01 = ' '
        | t < 0.03 = '▁'
        | t < 0.1 = '▂'
        | t < 0.3 = '▃'
        | t < 1 = '▄'
        | t < 3 = '▅'
        | t < 10 = '▆'
        | t < 30 = '▇'
        | otherwise = '█'
    showTime (nominalDiffTimeToSeconds -> MkFixed duration) =
        -- SI prefixes, and always exactly 2 decimal places, or 3 if there's no prefix
        T.show res
            <> T.singleton '.'
            <> T.take (if isNothing unit then 3 else 2) (T.show frac <> "000")
            <> foldMap T.singleton unit
            <> T.singleton 's'
      where
        (frac, res, unit) = case duration of
            0 -> (0, 0, Nothing)
            d -> go (0 :: Int) 0 d
        go = \case
            4 -> (,,Nothing)
            iterations -> \carried n ->
                case n `divMod` 1000 of
                    (0, r) ->
                        ( carried
                        , r
                        , Just case iterations of
                            3 -> 'm'
                            2 -> 'μ'
                            1 -> 'n'
                            _ -> 'p'
                        )
                    (d, r) -> go (succ iterations) r d
    sgr = T.pack . setSGRCode
    setColour d c = sgr [SetColor Foreground d c]

data TestRunnerOpts = TestRunnerOpts
    { regenerateGoldenFiles :: Bool
    }

runTests :: (MonadIO m, MonadCatch m) => TestRunnerOpts -> a -> TestTree m a -> m TestResult
runTests opts r0 (TestTree name tc ts) =
    let Test t = Control.Monad.Catch.try $ runTest tc
     in runReaderT (runWriterT (runExceptT t)) opts
            >>= fmap (\(result, logs) -> TestResult{name, logs, result}) . flip bitraverse pure \case
                Left e ->
                    pure $ Left e
                Right (Left e) ->
                    pure $ Left $ ExceptionFailure e
                Right (Right (r, dt)) -> do
                    rs <- for ts $ runTests opts r
                    let childTimes = sum $ map (either (const 0) fst . (.result)) rs
                    pure $ Right (dt + childTimes, rs)
  where
    runTest = \case
        TestCase f -> timed (liftIO . evaluate . DeepSeq.force) $ f r0
        TestCaseLazy f -> timed pure $ f r0
    timed f x = do
        t0 <- liftIO getCurrentTime
        r <- x
        rf <- f r
        t1 <- liftIO getCurrentTime
        pure (rf, diffUTCTime t1 t0)

assertEqual :: (Eq p, Monad m) => p -> p -> Test m ()
assertEqual expected actual = assert "not equal" (expected == actual)
assert :: (Monad m) => Text -> Bool -> Test m ()
assert s b = if b then pure () else assertFailure s
assertFailure :: (Monad m) => Text -> Test m a
assertFailure = throwError . AssertionFailure
golden :: (MonadIO m, MonadFail m) => FilePath -> Text -> Test m ()
golden file actual = do
    TestRunnerOpts{..} <- ask
    exists <- liftIO $ doesFileExist file
    if exists
        then do
            expected <- liftIO $ T.readFile file
            if expected == actual then pure () else throwError $ GoldenFailure{expected, actual}
        else do
            if regenerateGoldenFiles
                then
                    let parents = dropWhile null $ scanl (</>) "" $ splitDirectories $ takeDirectory file
                     in tell [LogRegeneratedGolden] >> liftIO do
                            for_ parents \dir -> do
                                parentExists <- liftIO $ doesDirectoryExist dir
                                when (not parentExists) $ createDirectory dir
                            T.writeFile file actual
                else
                    throwError GoldenMissing
