# Tagging

A general-purpose web-based psychophysics experiment platform

## Installation

*Tagging* is a little heavy to install, and we don't yet provide binaries. First-time installation may take a while, and running a *Tagging* server requires some experience with system administration. The instructions on this page should be enough to build and start a server - links are for background reading.

### Prerequisites:

 - [PostgreSQL](https://postgresql.org) database
   Create a database for *Tagging*. You will specify the database user, name and password later in a *Tagging* config file.

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
../deps/reflex-platform/installNix.sh
```

To build the server and frontends, TODO: finish 
