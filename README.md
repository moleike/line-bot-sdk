# line-bot-sdk [![Build Status](https://travis-ci.org/moleike/line-bot-sdk.svg?branch=master)](https://travis-ci.org/moleike/line-bot-sdk)

Servant library for building LINE chatbots. 

Features:

* Servant combinator `LineReqBody` for validation of request signatures using the channel secret. This is required to distinguish legitimate requests sent by LINE from malicious requests

* Bindings for (most) of the Messaging APIs

## Installation

### From Hackage

`line-bot-sdk` is available on [Hackage](https://hackage.haskell.org). Using the [`cabal-install`][cabal] tool:

```bash
cabal update
cabal install line-bot-sdk
```

### From source

Building from source can be done using [stack][stack] or [cabal][cabal]:

```bash
git clone github.com/moleike/line-bot-sdk.git
cd line-bot-sdk
stack install # Alternatively, `cabal install`
```

[cabal]: https://www.haskell.org/cabal
[stack]: https://docs.haskellstack.org/en/stable/README

## Documentation

The documentation for the latest release is available on [Hackage][hackage]. 

[hackage]: http://hackage.haskell.org/package/line-bot-sdk "Hackage"

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.String (fromString)
import Line.Bot.Client (Line, getProfile, runLine)
import Line.Bot.Types (Profile)
import System.Environment (getEnv)

profile :: Line Profile
profile = getProfile "U4af4980629..."

main = do
  token <- fromString <$> getEnv "CHANNEL_TOKEN"
  result <- runLine profile token
  case result of
    Left err -> print err
    Right profile -> print profile
```

See the
[examples/](https://github.com/moleike/line-bot-sdk/tree/master/examples) directory for more comprehensive examples.

## Contribute

Please report bugs via the
[github issue tracker](https://github.com/moleike/line-bot-sdk/issues).

## Acknowledgments

Thanks to the authors of [servant-github](https://hackage.haskell.org/package/servant-github), for inspiration.

