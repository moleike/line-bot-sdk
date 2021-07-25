# line-bot-sdk [![Build Actions](https://github.com/moleike/line-bot-sdk/workflows/build/badge.svg)](https://github.com/moleike/line-bot-sdk/actions)


Servant library for building LINE chatbots. 

Features:

* Servant combinator `LineReqBody` for validation of request signatures using the channel secret. This is required to distinguish legitimate requests sent by LINE from malicious requests

* Bindings for (most of) the Messaging APIs

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

See the official API documentation for more information.

- English: https://developers.line.biz/en/docs/messaging-api/overview/
- Japanese: https://developers.line.biz/ja/docs/messaging-api/overview/

## Usage

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.String        (fromString)
import           Line.Bot.Client
import           Line.Bot.Types
import           System.Environment (getEnv)

getProfiles :: Id Room -> Line [Profile]
getProfiles roomId = do
  userIds <- getRoomMemberUserIds roomId
  sequence $ getRoomMemberProfile roomId <$> userIds

main = do
  token  <- fromString <$> getEnv "CHANNEL_TOKEN"
  result <- runLine (getProfiles "U4af4980629...") token
  case result of
    Left err      -> print err
    Right profile -> print profile
```

See the
[examples/](https://github.com/moleike/line-bot-sdk/tree/master/examples) directory for more comprehensive examples.

## Contribute

Please report bugs via the
[github issue tracker](https://github.com/moleike/line-bot-sdk/issues).

## Acknowledgments

Thanks to the authors of [servant-github](https://hackage.haskell.org/package/servant-github), for inspiration.

