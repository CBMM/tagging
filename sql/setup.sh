PASS=$1
LINUXUSER=$2
ROOTUSER=$3  #On Ubuntu, use postgres. On osx, the user with postgres admin access

if [ $# -ne 3 ]; then
   echo "Please call setup.sh with password argument, main linux, and root linux user only"; exit;
fi

PGUSER=tagging
DB=tagging

read -p "About to reset the tagging database. Sure? (y/n)" yn
case $yn in
    [Nn]* ) echo "Ok, let's not yet. Did nothing."; exit;;
    [Yy]* )

        #USERPG=/home/$LINUXUSER/.pgpass;
        #ROOTPG=/root/.pgpass;
        #echo "Looking at $USERPG"
        #echo
        #sudo sed -i '/^localhost:5432:tagging/d' $USERPG
        #sudo echo >> $USERPG;
        #sudo echo "localhost:5432:tagging:tagging:$PASS" >> $USERPG;
        #sudo sed -i '/^localhost:5432:tagging/d' $ROOTPG
        #sudo echo >> $ROOTPG;
        #sudo echo "localhost:5432:tagging:tagging:$PASS" >> $ROOTPG;

        sudo -u $ROOTUSER dropdb $DB;
        sudo -u $ROOTUSER dropuser $PGUSER;
        sudo -u $ROOTUSER createuser $PGUSER;
        sudo -u $ROOTUSER createdb $DB;
        sudo -u $ROOTUSER psql -U $ROOTUSER -d $DB -c "ALTER ROLE $PGUSER WITH PASSWORD '$PASS';";
        sudo -u $ROOTUSER psql -U $ROOTUSER -d $DB -c "CREATE EXTENSION \"uuid-ossp\";";
        sudo -u $ROOTUSER psql -U $ROOTUSER -d $DB -c "GRANT ALL PRIVILEGES ON DATABASE $DB TO $PGUSER";
        ../tagging-server/dist/build/fixtures/fixtures ../tagging-server/snaplets/groundhog-postgresql/devel.cfg;;

    * ) echo "Didn't catch that. Try again, please answer y or n"
esac
