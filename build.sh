mkdir -p app/static/media/js

cd tagging-client && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure --ghcjs && cabal build" && cp -r dist/build/*/*.jsexe ../app/static/media/js/ && cd ..

cd homealone && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure --ghcjs && cabal build" && cp -r dist/build/*/*.jsexe ../app/static/media/js/ && cd ..

cd tagging-server && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure && cabal build" && cp dist/build/tagging-server/tagging-server ../app/ && cd ..

echo "Finished"
