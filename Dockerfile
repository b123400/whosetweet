FROM haskell:8.0

RUN mkdir -p /var/lib/whosetweet
WORKDIR /var/lib/whosetweet

COPY ./stack.yaml /var/lib/whosetweet

RUN stack setup

COPY ./whosetweet.cabal /var/lib/whosetweet
RUN stack install --only-dependencies

COPY . /var/lib/whosetweet

RUN stack build

CMD stack exec whosetweet
