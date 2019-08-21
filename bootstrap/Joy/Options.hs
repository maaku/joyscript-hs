-- Copyright (c) 2019 Mark Friedenbach <mark@friedenbach.org>
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0.  If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Joy.Options (
  PreludeOpts,
  CompilerOpts,
  parseCompilerOpts,
) where

import Options.Applicative
import Data.Semigroup ((<>))

data PreludeOpts = NoPrelude
                 | StdPrelude
                 | FilePrelude FilePath
  deriving (Show)

optNoPrelude :: Parser PreludeOpts
optNoPrelude = flag' NoPrelude
    ( long "XNoImplicitPrelude"
   <> help "Don't implicitly import Prelude." )

optFilePrelude :: Parser PreludeOpts
optFilePrelude = FilePrelude <$> strOption
    ( long "prelude"
   <> metavar "PRELUDE"
   <> help "Path to a file to be imported instead of the the standard Prelude." )

parsePreludeOpts :: Parser PreludeOpts
parsePreludeOpts = optNoPrelude <|> optFilePrelude <|> pure StdPrelude

data CompilerOpts = CompilerOpts
  { prelude :: !PreludeOpts
  } deriving (Show)

parseCompilerOpts :: Parser CompilerOpts
parseCompilerOpts = CompilerOpts <$> parsePreludeOpts

-- End of File
