#!/bin/bash
PASS = $1

sudo -u postgres createuser tagging
sudo -u postgres createdb tagging --owner=tagging
sudo -u postgres psql -c "alter user tagging with password '$PASS';"
