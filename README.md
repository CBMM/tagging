# Tagging

A general-purpose web-based psychophysics experiment platform

## Installation

*Tagging* is a little heavy to install, and we don't yet provide binaries. First-time installation may take a while, and running a *Tagging* server requires some experience with system administration. The instructions on this page should be enough to build and start a server - links are for background reading.

### Prerequisites:

 - [PostgreSQL](https://postgresql.org) database
   Create a database for *Tagging*. You will specify the database user, name and password later in a *Tagging* config file.

   (osx hint: [with homebrew](http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/) )
   
   (ubuntu hint: `sudo apt-get update && sudo apt-get install postgresql postgresql-contrib`)

 - [git](https://git-scm.com)

### Building from source

We use [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules), the [nix package manager](https://nixos.org/nix/), and [reflex-platform](https://github.com/reflex-frp/reflex-platform) to manage dependencies and building. These are all bundled with the *Tagging* code. Run the following in any directory where you keep source code.

```bash
git clone https://github.com/CBMM/tagging
cd tagging
git submodule update --init --recursive
```

If this is your first time installing, use *reflex-platform* to install nix:

```bash
./deps/reflex-platform/installNix.sh
```

To build the server and frontends:

```bash
./build.sh
```

`build.sh` will try to compile both the server and the frontend web experiments against a common definition of the *Tagging* API. You may want to comment out any lines in that script for frontends that you're not interested in, or that fail to build for whatever reason. A typical *Tagging* site only requires `tagging-server`, `tagging-client` and a single other frontend of your choice, to work.

`build.sh` copies the server binary and frontend javascript to the `app` directory. Frontend Haskell code will have been converted to javascript and saved in `app/static/media/js`

## Running the server

Copy `app/devel.cfg.example`  to `app/devel.cfg` and set the user, database and pasasword fields according to your Postgres setup. **Do not keep `devel.cfg` under version control!**

Then we create empty files to log activity to and launch the server on port 8000.

```bash
cd path/to/tagging/app
mkdir log
touch log/access.log
touch log/error.log
sudo ./tagging-server -p 8000
```

If all is working, go to `http://localhost:8000` and you will see the login screen.

By default, the first account created will have admin privileges (the ability to create users, modify any database rows and set and revoke researcher/admin creds on other users). We run the server locally (on port 8000) first in order to set up the admin account before anyone on the public internet can create an account.

To run the server on the public internet, the easiest way is to configure your machine to allow incoming traffic on port 80, and then run `sudo tagging-server -p 80`.
