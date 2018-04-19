An experimental embedded domain specific language for describing instruction set
architectures semantics.

# Features

* Extracting static data dependencies of single instructions and blocks (list of instructions).

# Getting started

Archer is implemented in [Idris](https://www.idris-lang.org/), thus the Idris language itself
and the related tools are required. The latest development version of Idris may be
installed from the git repo using the Haskell `stack` tool:

```
git clone git://github.com/idris-lang/Idris-dev.git
cd Idris-dev
stack setup && stack build && stack install
```

These commands will install the Haskell compiler (GHC), build Idris from source
and place the executable in `/home/<username>/.local/bin` (in Ubuntu, the path may differ
in your system.). Building Idris may require about 20 minutes, actual time will depend
on the hardware.
