#! /bin/sh

cabal sandbox init

cabal sandbox add-source deps/snaplet-postgresql-simple

cabal sandbox add-source deps/servant-snap/deps/snap/deps/io-streams
cabal sandbox add-source deps/servant-snap/deps/snap/deps/io-streams-haproxy
cabal sandbox add-source deps/servant-snap/deps/snap/deps/snap-core
cabal sandbox add-source deps/servant-snap/deps/snap/deps/snap-server
--cabal sandbox add-source deps/servant-snap/deps/snap/deps/xmlhtml
cabal sandbox add-source deps/servant-snap/deps/snap/deps/heist
cabal sandbox add-source deps/servant-snap/deps/snap
cabal sandbox add-source deps/snap-loader-dynamic
cabal sandbox add-source deps/snap-loader-static

cabal sandbox add-source deps/servant-snap/deps/servant/servant
cabal sandbox add-source deps/servant-snap/deps/servant/servant-docs
cabal sandbox add-source deps/servant-snap/deps/servant/servant-client
cabal sandbox add-source deps/servant-snap/deps/servant/servant-blaze
cabal sandbox add-source deps/servant-snap/deps/servant/servant-foreign
cabal sandbox add-source deps/servant-snap/deps/servant/servant-js
cabal sandbox add-source deps/servant-snap/deps/servant/servant-lucid
cabal sandbox add-source deps/servant-snap/deps/servant/servant-mock
cabal sandbox add-source deps/servant-snap/deps/servant/servant-server

cabal sandbox add-source deps/servant-snap/

cabal sandbox add-source deps/servant-matlab/

cabal sandbox add-source deps/yaml-ghcjs/
cabal sandbox add-source deps/groundhog/groundhog
cabal sandbox add-source deps/groundhog/groundhog-postgresql
cabal sandbox add-source deps/groundhog/groundhog-th

cabal sandbox add-source deps/reflex-dom
cabal sandbox add-source deps/reflex-dom-contrib

cabal sandbox add-source tagging-common



(cd tagging-server && cabal sandbox init --sandbox=../.cabal-sandbox)
(cd tagging-common && cabal sandbox init --sandbox=../.cabal-sandbox)
(cd tagging-client && cabal sandbox init --sandbox=../.cabal-sandbox)
