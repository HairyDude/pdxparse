# pdxparse
A parser for scripts used in Paradox Development Studios games, written in Haskell.

## Building

This package uses the standard Cabal build system.

First install the [https://www.haskell.org/platform/](Haskell Platform).

Then open a teminal and type:

    $ cabal update
    $ cabal install --only-dependencies
    $ runghc Setup.hs configure
    $ runghc Setup.hs build
    $ runghc Setup.hs install

## Usage

If you got EU4 from Steam, `pdxparse` should be able to find it automatically
as long as your steamapps folder is in the default location.  If it's somewhere
else, you'll need to edit `settings.yml` to point to it.

Currently there is no command line processing; it just processes everything it
finds and puts the results in the directory `output`. There is, however, a
clause in Main.hs that restricts the parser to only attempt to process certain
files. This is to make the program finish sooner while testing. If you want to
process as much as possible, delete everything between the comment saying
"DELETE ME" and the line saying "$ scripts".

