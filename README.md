# pdxparse
A parser for scripts used in Paradox Development Studios games, written in Haskell.

## Building

The easiest way to get it running is to use
[Stack](http://docs.haskellstack.org/en/stable/README.html). Install it, then
`cd` to the directory where you cloned `pdxparse` and type:

    $ stack install --install-ghc

This will automatically install the compiler and all dependencies. (If you
already have GHC 7.10.3 installed, you can probably omit `--install-ghc`.)

You may also be able to just use `cabal-install` if you have it:

    $ cabal install --prefix=/path/to/install

## Usage

`pdxparse` should be run from the command line. It will create a directory
`output` in the current directory. Its structure is the same as that of the EU4
directory.

If you got EU4 from Steam, `pdxparse` should be able to find it automatically
as long as your steamapps folder is in the default location.  If it's somewhere
else, you'll need to edit `settings.yml` to point to it.

Currently there is no command line processing; it just processes everything it
finds and puts the results in the directory `output`. There is, however, a
clause in Main.hs that restricts the parser to only attempt to process certain
files. This is to make the program finish sooner while testing. If you want to
process only certain files, uncomment those lines, edit the list to include
only the files you want, and rebuild.

