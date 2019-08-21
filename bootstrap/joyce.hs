-- Copyright (c) 2019 Mark Friedenbach <mark@friedenbach.org>
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Joy.Options

import Options.Applicative
import Data.Semigroup ((<>))

-- joyce compiler options

-- Most of the compiler options are hidden away in the CompilerOpts
-- type in the Joy library, which is shared by both joyce and runjoy.
-- The options we handle here have to do with using the compiler
-- subsystem to generate output files of various types.
data JoyceOpts = JoyceOpts
  { keepJoyCoreSource :: !Bool
  , keepModuleSpec    :: !Bool
  , keepHaskellSource :: !Bool
  , keepObjectCode    :: !Bool
  , targetStage       :: !TargetStage
  , outputOpts        :: !OutputOpts
  , compilerOpts      :: !CompilerOpts
  } deriving (Show)

parseJoyceOpts :: Parser JoyceOpts
parseJoyceOpts = JoyceOpts
  <$> switch
    ( long "keep-joycore-files"
   <> long "keep-joycore-file"
   <> help "Retain intermediate .joycore Joyscript Core source files." )
  <*> switch
    ( long "keep-joyspec-files"
   <> long "keep-joyspec-file"
   <> help "Retain intermediate .joyspec interface specification files.")
  <*> switch
    ( long "keep-hs-files"
   <> long "keep-hs-file"
   <> help "Retain intermediate .hs Haskell source files." )
  <*> switch
    ( long "keep-o-files"
   <> long "keep-o-file"
   <> help "Retain intermediate .o compiled object files." )
  <*> parseTargetStage
  <*> parseOutputOpts
  <*> parseCompilerOpts

-- As with compiler for other languages, the user can specify
-- compilation to stop after certain stages of the compilation
-- pipeline.  Most commonly used is "-c" to compile source files to
-- object code without linking, but other target can be useful for
-- debugging purposes.
data TargetStage = LinkExecutable -- (default)
                 | MachineCode
                 | HaskellSource
                 | MacroExpansion
  deriving (Show) -- TODO: maybe remove?

optMachineCode :: Parser TargetStage
optMachineCode = flag' MachineCode
   ( long "compile"
  <> short 'c'
  <> help "Compile the source files, but do not link to create an executable.  The output is in the form of architecture-dependent object file produced by the Haskell compiler, and an architecture-independent module interface specification for each input file specified." )

optHaskellSource :: Parser TargetStage
optHaskellSource = flag' HaskellSource
   ( long "haskell"
  <> short 'S'
  <> help "Stop after the stage of Haskell source-to-source compilation; do not generate object code.  The output is in the form of a module interface specification file and Haskell source code suitable for compilation with recent versions of GHC, for each input file specified." )

optMacroExpansion :: Parser TargetStage
optMacroExpansion = flag' MacroExpansion
   ( long "preprocess"
  <> short 'E'
  <> help "Stop after the macro expansion stage; do not run the compiler proper.  The output is in the form of preprocessed Joyscript source code." )

parseTargetStage :: Parser TargetStage
parseTargetStage = optMachineCode <|> optHaskellSource <|> optMacroExpansion <|> pure LinkExecutable

-- By default compiler output is sent to files with names based on the
-- input file name, with the suffix replaced based on output type.
-- The user can override this, or even specify output to go to stdout.
data OutputOpts = DefaultOutput
                | DisplayToStdOut
                | FileOutput FilePath
  deriving (Show) -- FIXME: maybe remove?

optDisplayToStdOut :: Parser OutputOpts
optDisplayToStdOut = flag' DisplayToStdOut
   ( long "to-stdout"
  <> help "Send non-binary output to stdout rather than a file.  Must be combined with -E or -S." )

optFileOutput = FileOutput <$> strOption
   ( long "output"
  <> short 'o'
  <> help "Send output to the file specified." )

parseOutputOpts :: Parser OutputOpts
parseOutputOpts = optDisplayToStdOut <|> optFileOutput <|> pure DefaultOutput

main :: IO ()
main = engine =<< execParser opts
  where
    opts = info (parseJoyceOpts <**> helper)
       ( fullDesc
      <> progDesc "Compile, assemble, and link a Joyscript program."
      <> header "joyce - the (Joy)script (c)ompiler and run-time (e)nvironment" )

engine :: JoyceOpts -> IO ()
engine opts = putStrLn $ "Options: " ++ (show opts)

-- End of File
