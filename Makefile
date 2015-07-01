LIB=$(wildcard collabplot/src/*.hs)
SERVER=tagging_server/dist/build/server/server
CLIENT=tagging_server/static/all.js
CLIENT_SRC=$(wildcard tagging_client/src/*.hs)

all: $(LIB) $(SERVER) $(CLIENT)

$(SERVER): $(LIB) tagging_server/server.cabal tagging_server/src/*.hs
	(cd tagging_server && cabal build)

$(CLIENT): $(LIB) tagging_client/client.cabal $(CLIENT_SRC)
	(cd tagging_client && cabal configure --ghcjs && cabal build)
	cp tagging_client/dist/build/tagging_client/tagging_client.jsexe/*.js tagging_server/static/
