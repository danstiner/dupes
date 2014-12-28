FROM buildpack-deps

RUN apt-get update && apt-get install -y haskell-platform

RUN cabal update
RUN cabal install -j hdevtools

ENV PATH /.cabal/bin:$PATH
ENV LANG C.UTF-8

# App specific steps
RUN apt-get install -y libleveldb-dev
