module Main (main) where

import Data.Either (partitionEithers)
import qualified System.Console.MultiArg as MA
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified Data.ByteString as BS
import qualified System.Directory as D
import qualified Control.Concurrent as C
import qualified Control.Exception as Ex
import qualified System.Process as P
import qualified Paths_rewrite as Pr
import qualified Data.Version as Ver

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ "[options] FILE PROGRAM [program options]"
  , "opens given FILE and feeds it to standard input of PROGRAM"
  , "and writes PROGRAM output back to FILE."
  , ""
  , "Aborts if PROGRAM exits with non-zero exit status."
  , ""
  , "Options:"
  , "  -b, --backup SUFFIX Back up FILE to file with given SUFFIX"
  , "                      before writing new file"
  , ""
  , "  -h, --help          Show help and exit"
  , "  --version           Show version and exit"
  ]

version :: String -> String
version pn = unlines
  [ pn ++ " " ++ Ver.showVersion Pr.version ]

type Backup = String
type ProgramOpt = String
type ProgName = String
type InputFile = String

errExit :: String -> IO a
errExit msg = do
  pn <- MA.getProgName
  IO.hPutStrLn IO.stderr $ pn ++ ": error: " ++ msg
  Exit.exitFailure

-- | Parses command line arguments; returns whether to do backup and
-- positional arguments
parseArgs :: IO (Maybe Backup, InputFile, ProgName, [ProgramOpt])
parseArgs = do
  let opts = [ MA.OptSpec ["backup"] "b" (MA.OneArg (return . Left)) ]
  as <- MA.simpleHelpVersion help version
                             opts MA.StopOptions (return . Right)
  let (baks, os) = partitionEithers as
  bak <- case baks of
    [] -> return Nothing
    x:[] -> if null x
            then errExit "empty backup suffix given"
            else return $ Just x
    _ -> errExit "multiple backup suffixes given"
  (input, name, progOpts) <- case os of
    [] -> errExit "no input file or program name given"
    _:[] -> errExit "no program name given"
    x:y:xs -> return (x, y, xs)
  return (bak, input, name, progOpts)


doBackup :: InputFile -> Backup -> IO ()
doBackup inf bak = D.copyFile inf (inf ++ "." ++ bak)


runProgram
  :: Maybe Backup

  -> InputFile
  -- ^ Name of input file

  -> ProgName

  -> [ProgramOpt]

  -> IO ()
runProgram mayBak inFile pn opts = do
  input <- BS.readFile inFile
  (code, out) <- readProcess pn opts input
  _ <- case code of
    Exit.ExitSuccess -> return ()
    Exit.ExitFailure bad ->
      errExit $ "program " ++ pn ++ " exited with code "
                ++ show bad
  _ <- case mayBak of
    Nothing -> return ()
    Just bak -> doBackup inFile bak
  BS.writeFile inFile out


-- | Given the name of a program, the options, and the data to feed to
-- the program's standard input, runs the program in a subprocess.
-- Returns the exit code from the program and the text printed to the
-- program's standard output.  Any output the subprocess prints to its
-- standard error is printed to rewrite's standard error.
--
-- Previous versions of rewrite simply used 'readProcessWithExitCode'
-- from "System.Process.ByteString" from the process-extras package.
-- However, this would result in bad behavior if the subprocess
-- failed.  For example, if you ran
--
-- > penny-fit -f amex clear
--
-- then you would get
--
-- > penny-fit: user error (clear: you must provide a postings file.)
--
-- and the program would exit with an error.  However, if you ran
--
-- > rewrite main.pny penny-fit -f amex clear
--
-- you would get
--
-- > rewrite: fd:5: hPutBuf: resource vanished (Broken pipe)
--
-- which is not a helpful error message.  This function ensures that
-- the standard error from the subprocess is always printed, which
-- results in much more useful error messages.  Now the same command
-- yields:
--
-- > penny-fit: user error (clear: you must provide a postings file.)
-- > rewrite: fd:9: hPutBuf: resource vanished (Broken pipe)
--
-- which still has some gobbledygook about hPutBuf, but at least now
-- there is also the underlying program's error message.  (The second
-- line could be stripped out, but I don't see the point in this and
-- sometimes such errors might be helpful.)
--
-- All intermediate results are buffered in memory, so rewrite may not
-- be suitable for dealing with huge intermediate results that are
-- hard to buffer in available memory.

readProcess
  :: ProgName
  -> [ProgramOpt]
  -> BS.ByteString
  -> IO (Exit.ExitCode, BS.ByteString)
readProcess pn os bs = do
  let cp = P.CreateProcess
        { P.cmdspec = P.RawCommand pn os
        , P.cwd = Nothing
        , P.env = Nothing
        , P.std_in = P.CreatePipe
        , P.std_out = P.CreatePipe
        , P.std_err = P.CreatePipe
        , P.close_fds = False
        , P.create_group = False }
  (Just i, Just o, Just e, h) <- P.createProcess cp
  mWriteBS <- forkThreadMVar (BS.hPut i bs)
  mReadOut <- forkThreadMVar (BS.hGetContents o)
  mReadErr <- forkThreadMVar (BS.hGetContents e)
  mCode <- forkThreadMVar (P.waitForProcess h)
  strErr <- throwErr mReadErr
  BS.hPutStr IO.stderr strErr
  c <- throwErr mCode
  _ <- throwErr mWriteBS
  out <- throwErr mReadOut
  return (c, out)


-- | Fetches a value stored in an MVar. Throws if the value is an
-- exception; otherwise, returns the value.
throwErr :: C.MVar (Either Ex.SomeException a) -> IO a
throwErr m = do
  m' <- C.readMVar m
  case m' of
    Left e -> Ex.throwIO e
    Right g -> return g


-- | Forks a new thread, and supplies the result of the computation
-- (or any resulting exception) in an MVar. To wait until the thread
-- exits, get the MVar's value.
forkThreadMVar
  :: IO a
  -> IO (C.MVar (Either Ex.SomeException a))
forkThreadMVar i = do
  m <- C.newEmptyMVar
  _ <- C.forkFinally i $ \e -> C.putMVar m e
  return m


main :: IO ()
main = do
  (mayBak, inf, pn, opts) <- parseArgs
  runProgram mayBak inf pn opts

