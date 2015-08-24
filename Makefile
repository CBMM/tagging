LIB=$(wildcard collabplot/src/*.hs)
SERVER=tagging-server/dist/build/tagging-server/tagging-server
SERVER_SRC=$(wildcard tagging_server/src/*.hs)
CLIENT=tagging-server/static/all.js
CLIENT_SRC=$(wildcard tagging-client/src/*.hs)

all: $(LIB) $(SERVER) $(CLIENT)

$(SERVER): $(LIB) tagging-server/tagging-server.cabal $(SERVER_SRC)
	(cd tagging-server && cabal build)

$(CLIENT): $(LIB) tagging-client/tagging-client.cabal $(CLIENT_SRC)
	(cd tagging-client && cabal configure --ghcjs && cabal install --ghcjs)
	cp -r tagging-client/.cabal-sandbox/bin/*.jsexe tagging-server/static/media/js/
