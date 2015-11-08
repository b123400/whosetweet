#How to build

##Server side

- Copy config/settings.yml.sample to config/settings.yml and fill in your api keys
- Install [Haskell](https://www.haskell.org)
- Install [stack](https://www.stackage.org)
- run `stack build` in the project folder
- To start server: `stack exec whosetweet`

##Client side

Files are in the static folder.

To build:

- Install [elm](http://elm-lang.org)
- run `elm package install`
- run `elm make index.elm --output index.js`