-- Copyright (c) 2019 Mark Friedenbach <mark@friedenbach.org>
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data RunJoyOpts = RunJoyOpts
  { path :: String }

parseOpts :: Parser RunJoyOpts
parseOpts = RunJoyOpts
  <$> argument str
     ( metavar "PATH" )

main :: IO ()
main = engine =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
       ( fullDesc
      <> progDesc "Interprets/runs the specified Joyscript program from source, without first having to compile it.  The sole argument provided is the path to the main module of the program."
      <> header "runjoy - run a (Joy)script a program directly from source" )

engine :: RunJoyOpts -> IO ()
engine (RunJoyOpts path) = putStrLn $ "Running Joyscript program at path " ++ (show path)

-- End of File
