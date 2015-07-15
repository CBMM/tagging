PASS=$1
LINUXUSER=$2

if [ $# -ne 2 ]; then
   echo "Please call setup.sh with password argument and linux user only"; exit;
fi

PGUSER=tagging
DB=tagging

read -p "About to reset the tagging database. Sure? (y/n)" yn
case $yn in
    [Nn]* ) echo "Ok, let's not yet. Did nothing."; exit;;
    [Yy]* )

        USERPG=/home/$LINUXUSER/.pgpass;
        ROOTPG=/root/.pgpass;
        echo "Looking at $USERPG"
        echo
        sudo sed -i '/^localhost:5432:tagging/d' $USERPG
        sudo echo >> $USERPG;
        sudo echo "localhost:5432:tagging:tagging:$PASS" >> $USERPG;
        sudo sed -i '/^localhost:5432:tagging/d' $ROOTPG
        sudo echo >> $ROOTPG;
        sudo echo "localhost:5432:tagging:tagging:$PASS" >> $ROOTPG;

        sudo -u postgres dropdb $DB;
        sudo -u postgres dropuser $PGUSER;
        sudo -u postgres createuser $PGUSER;
        sudo -u postgres psql -c "ALTER ROLE $PGUSER WITH PASSWORD '$PASS';";
        sudo -u postgres createdb $DB;
        sudo -u postgres psql -d $DB -c "CREATE EXTENSION \"uuid-ossp\";";
        sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE $DB TO $PGUSER";;
    * ) echo "Didn't catch that. Try again, please answer y or n"
esac
