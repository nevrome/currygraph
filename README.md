# currygraph

currygraph is a small command line tool for specific path searches on spatial graphs.
At the moment it's solely developed for my own needs, though I may extend it in the future for more general applications.

It is written in the functional logic programming language [curry](https://curry-lang.org/).

### Setup

Install one of the curry compilers [PAKCS](https://www.curry-lang.org/pakcs/), [KiCS2](https://www.curry-lang.org/kics2/), or [Curry2G](https://www.curry-lang.org/curry2go/) (currygraph was only tested with KiCS2, but it should also work with the others). 

All of these compilers come with the [Curry Package Manager (CPM)](https://www.curry-lang.org/tools/cpm/). You should therefore be able to install currygraph by running

```
cypm install
```

in the root directory of this repository after installing the respective compiler.

This will compile the main module and install the currygraph executable in `$HOME/.cpm/bin`.

Run `currygraph -h` to access the command line help.
