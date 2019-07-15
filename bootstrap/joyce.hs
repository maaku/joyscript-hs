-- Copyright (c) 2019 Mark Friedenbach <mark@friedenbach.org>
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data JoyceOpts = JoyceOpts
  { compile :: Bool }

parseOpts :: Parser JoyceOpts
parseOpts = JoyceOpts
  <$> switch
     ( long "compile"
    <> short 'c'
    <> help "Generate object file, but do not link." )

main :: IO ()
main = engine =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
       ( fullDesc
      <> progDesc "Compile, assemble, and link a Joyscript program."
      <> header "joyce - the (Joy)script (c)ompiler and run-time (e)nvironment" )

engine :: JoyceOpts -> IO ()
engine (JoyceOpts c) = putStrLn $ "Running with --compile=" ++ (show c)

-- End of File
