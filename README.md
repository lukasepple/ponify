# ponify – ponypipe in haskell
This is a implementation of [ponypipe](http://github.com/maandree/ponypipe) in Haskell.

It can ponify text, e. g. it converts "anybody" to "anypony" etc., and deponify it.

## Usage

* `ponify --ponify < INFILE > OUTFILE` ponifies INFILE and outputs that into OUTFILE
* `ponify --deponify < INFILE > OUTFILE` the other direction

with `--rules` you can specify a custom [rules](./rules)-file.

## Credits
The idea is clearly [manandree](http://github.com/maandree)'s and I wanted to implement it in Haskell for fun. I also used his incredible awesome rules.
