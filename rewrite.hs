module Main (main) where

import Data.Either (partitionEithers)
import qualified System.Console.MultiArg as MA
import qualified System.IO as IO
import qualified System.Exit as Exit
import System.Process.ByteString (readProcessWithExitCode)
import qualified Data.ByteString as BS
import qualified System.Directory as D

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ "[options] FILE PROGRAM [program options]"
  , "opens given FILE and feeds it to standard input of PROGRAM"
  , "and writes PROGRAM output back to FILE."
  , ""
  , "Aborts if PROGRAM exits with non-zero exit status."
  , ""
  , "Options:"
  , "  -h, --help          Show help and exit"
  , "  -b, --backup SUFFIX Back up FILE to file with given SUFFIX"
  , "                      before writing new file"
  ]

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
  let opts = [ MA.OptSpec ["backup"] "b" (MA.OneArg Left) ]
  as <- MA.simpleWithHelp help MA.StopOptions opts (return . Right)
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
  (code, out, err) <- readProcessWithExitCode pn opts input
  BS.hPutStr IO.stderr err
  _ <- case code of
    Exit.ExitSuccess -> return ()
    Exit.ExitFailure bad ->
      errExit $ "program " ++ pn ++ " exited with code "
                ++ show bad
  _ <- case mayBak of
    Nothing -> return ()
    Just bak -> doBackup inFile bak
  BS.writeFile inFile out


main :: IO ()
main = do
  (mayBak, inf, pn, opts) <- parseArgs
  runProgram mayBak inf pn opts
