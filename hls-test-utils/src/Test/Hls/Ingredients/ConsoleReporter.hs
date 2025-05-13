{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImplicitParams           #-}
module Test.Hls.Ingredients.ConsoleReporter (consoleTestReporter) where

import           Control.Monad                          (join, unless, void,
                                                         when, forM_)
import           Control.Monad.Trans.Reader             (Reader, ask, runReader)
import           Data.Char
import           Prelude                                hiding (EQ, fail)
import           Test.Tasty.Options
import           Test.Tasty.Providers.ConsoleFormat
import           Text.Printf
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.State              (evalStateT, get,
                                                         modify, put)
import qualified Data.IntMap                            as IntMap
import qualified Data.IntSet                            as IntSet
import           Data.Monoid                            (Any (..))
import           Data.Proxy
import           System.Console.ANSI
import           System.IO
import           Test.Tasty
import           Test.Tasty.Ingredients.ConsoleReporter hiding (buildTestOutput,
                                                         consoleTestReporter)
import           Test.Tasty.Providers
import           Test.Tasty.Runners                     hiding
                                                        (consoleTestReporter)

import Debug.Trace
import Data.IntMap (IntMap)
import Data.Functor ((<&>))

type Level = Int

consoleTestReporterOptions :: [OptionDescription]
consoleTestReporterOptions =
  [ Option (Proxy :: Proxy Quiet)
  , Option (Proxy :: Proxy HideSuccesses)
  , Option (Proxy :: Proxy MinDurationToReport)
  , Option (Proxy :: Proxy UseColor)
  , Option (Proxy :: Proxy AnsiTricks)
  ]

-- | A simple console UI with a hook to postprocess results,
-- depending on their names and external conditions
-- (e. g., its previous outcome, stored in a file).
-- Names are listed in reverse order:
-- from test's own name to a name of the outermost test group.
--
-- @since 1.4.2
consoleTestReporter :: Ingredient
consoleTestReporter = TestReporter consoleTestReporterOptions $
  \opts tree -> Just $ \smap -> do

  let
    whenColor = lookupOption opts
    Quiet quiet = lookupOption opts
    HideSuccesses hideSuccesses = lookupOption opts
    NumThreads numThreads = lookupOption opts
    AnsiTricks ansiTricks = lookupOption opts

  if quiet
    then do
      b <- statusMapResult numThreads smap
      return $ \_time -> return b
    else

      do
      isTerm <- hSupportsANSI stdout
      isTermColor <- hSupportsANSIColor stdout

      (\k -> if isTerm
        -- When killing with Ctrl+C 'showCursor' can fail
        -- to restore terminal cursor if not flushed explicitly
        then (do hideCursor; k) `finally` (do showCursor; hFlush stdout)
        else k) $ do

          hSetBuffering stdout LineBuffering

          let
            ?colors = useColor whenColor isTermColor

          let
            -- 'buildTestOutput' is a pure function and cannot query 'hSupportsANSI' itself.
            -- We also would rather not pass @isTerm@ as an extra argument,
            -- since it's a breaking change, thus resorting to tweaking @opts@.
            opts' = changeOption (\(AnsiTricks x) -> AnsiTricks (x && isTerm)) opts
            toutput = buildTestOutput opts' tree

          case () of { _
            | hideSuccesses && isTerm && ansiTricks ->
                consoleOutputHidingSuccesses toutput smap
            | hideSuccesses ->
                streamOutputHidingSuccesses toutput smap
            | otherwise -> consoleOutput toutput smap
          }

          return $ \time -> do
            stats <- computeStatistics smap
            let
              testNames = foldTestTree (trivialFold { foldSingle = \ _ name _ -> [name] }) opts tree
            when (statFailures stats /= 0) $ do
              fails <- summariseTestFailures testNames smap
              printFails opts fails
            printStatistics stats time
            return $ statFailures stats == 0

printFails :: OptionSet -> [(TestName, FailureReason)] -> IO ()
printFails opts testFails = do
  hSetBuffering stdout LineBuffering
  let
    whenColor = lookupOption opts
  isTermColor <- hSupportsANSIColor stdout
  let
    ?colors = useColor whenColor isTermColor
  forM_ testFails $ \ (name, reason) -> do
    infoOk name
    infoOk " ... "
    infoFail "FAIL"

summariseTestFailures :: [TestName] -> StatusMap -> IO [(TestName, FailureReason)]
summariseTestFailures names sMap =
  getApp $ foldMap (\(var, name) -> Ap $
      getResultFromTVar var <&>
       (\r -> case resultOutcome r of
        Success -> []
        Failure reason ->
          [(name, reason)]
      )
    ) (zip (fmap snd $ IntMap.toAscList sMap) names)

-- {{{
consoleOutput :: (?colors :: Bool) => TestOutput -> StatusMap -> IO ()
consoleOutput toutput smap =
  getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      ( Traversal $ do
          printName :: IO ()
          r <- getResult
          printResult r
      , Any True)
    foldHeading _name printHeading (printBody, Any nonempty) =
      ( Traversal $ do
          when nonempty $ do
            printHeading :: IO ()
            getTraversal printBody
      , Any nonempty
      )

consoleOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO ()
consoleOutputHidingSuccesses toutput smap =
  void . getApp $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      Ap $ do
          printName :: IO ()
          r <- getResult
          if resultSuccessful r
            then do clearThisLine; return $ Any False
            else do printResult r :: IO (); return $ Any True

    foldHeading _name printHeading printBody =
      Ap $ do
        printHeading :: IO ()
        Any failed <- getApp printBody
        unless failed clearAboveLine
        return $ Any failed

    clearAboveLine = do cursorUpLine 1; clearThisLine
    clearThisLine = do clearLine; setCursorColumn 0

streamOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO ()
streamOutputHidingSuccesses toutput smap =
  void . flip evalStateT [] . getApp $
    foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      Ap $ do
          r <- liftIO $ getResult
          if resultSuccessful r
            then return $ Any False
            else do
              stack <- get
              put []

              liftIO $ do
                sequence_ $ reverse stack
                printName :: IO ()
                printResult r :: IO ()

              return $ Any True

    foldHeading _name printHeading printBody =
      Ap $ do
        modify (printHeading :)
        Any failed <- getApp printBody
        unless failed $
          modify $ \stack ->
            case stack of
              _:rest -> rest
              []     -> [] -- shouldn't happen anyway
        return $ Any failed


-- | Wait until
--
-- * all tests have finished successfully, and return 'True', or
--
-- * at least one test has failed, and return 'False'
statusMapResult
  :: Int -- ^ lookahead
  -> StatusMap
  -> IO Bool
statusMapResult lookahead0 smap
  | IntMap.null smap = return True
  | otherwise =
      join . atomically $
        IntMap.foldrWithKey f finish smap mempty lookahead0
  where
    f :: Int
      -> TVar Status
      -> (IntSet.IntSet -> Int -> STM (IO Bool))
      -> (IntSet.IntSet -> Int -> STM (IO Bool))
    -- ok_tests is a set of tests that completed successfully
    -- lookahead is the number of unfinished tests that we are allowed to
    -- look at
    f key tvar k ok_tests lookahead
      | lookahead <= 0 =
          -- We looked at too many unfinished tests.
          next_iter ok_tests
      | otherwise = do
          this_status <- readTVar tvar
          case this_status of
            Done r ->
              if resultSuccessful r
                then k (IntSet.insert key ok_tests) lookahead
                else return $ return False
            _ -> k ok_tests (lookahead-1)

    -- next_iter is called when we end the current iteration,
    -- either because we reached the end of the test tree
    -- or because we exhausted the lookahead
    next_iter :: IntSet.IntSet -> STM (IO Bool)
    next_iter ok_tests =
      -- If we made no progress at all, wait until at least some tests
      -- complete.
      -- Otherwise, reduce the set of tests we are looking at.
      if IntSet.null ok_tests
        then retry
        else return $ statusMapResult lookahead0 (IntMap.withoutKeys smap ok_tests)

    finish :: IntSet.IntSet -> Int -> STM (IO Bool)
    finish ok_tests _ = next_iter ok_tests


-- | Build the 'TestOutput' for a 'TestTree' and 'OptionSet'. The @colors@
-- ImplicitParam controls whether the output is colored.
--
--
-- This was initially copy-pasted from
-- https://github.com/UnkindPartition/tasty/tree/7d61d9887df5180abf69d6427655d3e71331217d/core/Test/Tasty/Ingredients/ConsoleReporter.hs
buildTestOutput :: (?colors :: Bool) => OptionSet -> TestTree -> TestOutput
buildTestOutput opts tree =
  let
    MinDurationToReport{minDurationMicros} = lookupOption opts

    runSingleTest
      :: (IsTest t, ?colors :: Bool)
      => OptionSet -> TestName -> t -> Ap (Reader Level) TestOutput
    runSingleTest _opts name _test = Ap $ do
      level <- ask

      let
        testNamePadded = printf "%s%s%s"
          (indent level)
          name
          (" ... ")

        printTestName = do
          withoutLineWrap $ putStr testNamePadded
          hFlush stdout

        printTestProgress progress
          -- We cannot display progress properly if a terminal
          -- does not support manipulations with cursor position.
          | not getAnsiTricks = pure ()

          | progress == emptyProgress = pure ()

          | otherwise = do
              let
                msg = case (cleanupProgressText $ progressText progress, 100 * progressPercent progress) of
                        ("",  pct) -> printf "%.0f%% " pct
                        (txt, 0.0) -> printf "%s" txt
                        (txt, pct) -> printf "%s: %.0f%% " txt pct
              putChar '\r'
              -- A new progress message may be shorter than the previous one
              -- so we must clean whole line and print anew.
              clearLine
              withoutLineWrap $ do
                putStr testNamePadded
                infoOk msg
              hFlush stdout

        printTestResult result = do
          rDesc <- formatMessage $ resultDescription result

          -- use an appropriate printing function
          let
            printFn =
              case resultOutcome result of
                Success               -> ok
                Failure TestDepFailed -> skipped
                _                     -> fail
            time = resultTime result

          withoutLineWrap $ do
            when getAnsiTricks $ do
              putChar '\r'
              clearLine
              putStr testNamePadded

          printFn (resultShortDescription result)
          when (floor (time * 1e6) >= minDurationMicros) $
            printFn (printf " (%.2fs)" time)
          printFn "\n"

          when (not $ null rDesc) $
            (if resultSuccessful result then infoOk else infoFail) $
              printf "%s%s\n" (indent $ level + 1) (formatDesc (level+1) rDesc)
          case resultDetailsPrinter result of
            ResultDetailsPrinter action -> action level withConsoleFormat

      return $ PrintTest name printTestName printTestProgress printTestResult

    runGroup :: OptionSet -> TestName -> [Ap (Reader Level) TestOutput] -> Ap (Reader Level) TestOutput
    runGroup _opts name grp = Ap $ do
      level <- ask
      let
        printHeading = withoutLineWrap $ printf "%s%s\n" (indent level) name
        printBody = runReader (getApp (mconcat grp)) (level + 1)
      return $ PrintHeading name printHeading printBody

  in
    flip runReader 0 $ getApp $
      foldTestTree
        trivialFold
          { foldSingle = runSingleTest
          , foldGroup = runGroup
          }
          opts tree
  where
    AnsiTricks{getAnsiTricks} = lookupOption opts
    -- We must ensure these lines don't wrap, otherwise the wrong
    -- line will be cleared later or the test tree printing will
    -- itself wrap.
    withoutLineWrap :: IO () -> IO ()
#if MIN_VERSION_ansi_terminal(1,1,2)
    withoutLineWrap m | getAnsiTricks =
      bracket_ disableLineWrap enableLineWrap m
#endif
    withoutLineWrap m = m

-- | Make sure the progress text does not contain any newlines or line feeds,
-- lest our ANSI magic breaks. Since the progress text is expected to be short,
-- we simply drop anything after a newline.
cleanupProgressText :: String -> String
cleanupProgressText = map (\c -> if isSpace c then ' ' else c)
                    . takeWhile (\c -> c /= '\n' && c /= '\r' && c /= '\t')


-- (Potentially) colorful output
ok, fail, skipped, infoOk, infoFail :: (?colors :: Bool) => String -> IO ()
fail     = output failFormat
ok       = output okFormat
skipped  = output skippedFormat
-- Just default foreground color for 'infoOk'; do not apply 'infoOkFormat',
-- because terminal's background could be white itself. See #298.
infoOk   = putStr
infoFail = output infoFailFormat

output
  :: (?colors :: Bool)
  => ConsoleFormat
  -> String
  -> IO ()
output format = withConsoleFormat format . putStr

indentSize :: Int
indentSize = 2

indent :: Int -> String
indent n = replicate (indentSize * n) ' '

-- handle multi-line result descriptions properly
formatDesc
  :: Int -- indent
  -> String
  -> String
formatDesc n desc =
  let
    -- remove all trailing linebreaks
    chomped = reverse . dropWhile (== '\n') . reverse $ desc

    multiline = '\n' `elem` chomped

    -- we add a leading linebreak to the description, to start it on a new
    -- line and add an indentation
    paddedDesc = flip concatMap chomped $ \c ->
      if c == '\n'
        then c : indent n
        else [c]
  in
    if multiline
      then paddedDesc
      else chomped

--------------------------------------------------
-- Various utilities
--------------------------------------------------
-- {{{
getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar var =
  atomically $ do
    status <- readTVar var
    case status of
      Done r -> return r
      _ -> retry
