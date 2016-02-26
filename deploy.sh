KEY=$1
URL=$2

scp -r -i $KEY tagging-server/static $URL:tagging-server/
scp -i $KEY tagging-server/dist/build/tagging-server/tagging-server $URL:tagging-server/
scp -r -i $KEY tagging-server/snaplets $URL:tagging-server/
