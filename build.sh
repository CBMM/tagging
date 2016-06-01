cd tagging-client && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure --ghcjs && cabal build" && cp -r tagging-client/dist/build/*/*.jsexe ../app/static/media/js/ && cd ..

cd tagging-server && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure && cabal build"


cp tagging-server/dist/build/tagging-server/tagging-server app/



