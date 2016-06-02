mkdir -p app/static/media/js

echo "Building tagging-client"
cd tagging-client && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure --ghcjs && cabal build" && cp -r dist/build/*/*.jsexe ../app/static/media/js/ && cd ..
echo "Done with tagging-client"

echo "Building homealone"
cd homealone && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure --ghcjs && cabal build" && cp -r dist/build/*/*.jsexe ../app/static/media/js/ && cd ..
echo "Done with homealone"

echo "Building tagging-server"
cd tagging-server && ../deps/reflex-platform/work-on ./tr.nix ./. --run "cabal configure && cabal build" && cp dist/build/tagging-server/tagging-server ../app/ && cd ..
echo "Done with tagging-server"

echo "Finished"
