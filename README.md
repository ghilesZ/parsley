# Parsley [![Build Status](https://travis-ci.com/ghilesZ/parsley.svg?branch=master)](https://travis-ci.com/ghilesZ/parsley)

Parsley provides conversions between OCaml's numeric types (float,
int32, int64, native) while indicating if a loss of precision occured
during the conversion. It uses the
[Result](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Result.html)
module of OCaml to do that as in the following example:

#### Documentation
You can build locally the documentation by typing `make doc` or consult the online [documentation](https://ghilesz.github.io/parsley/parsley/Parsley/).

### How?
Parsley parses the string using arbitrary precision integers and
rationals (using the Zarith library) and compares the result it
obtains with the one obtained by OCaml's standard of_string utilities
(int_of_string, float_of_string ...)

### PPX
You can use Parsley's functionnalities on your OCaml source code using
the ppx that goes with the library. It generates pretty warning messages
using OCaml's warning styles as in the following examples.

##### todo example

## Dependencies
- Zarith: https://github.com/ocaml/Zarith

## Compatibility:
- OCaml >= 4.08

## Licence:
This Library is distributed under the terms of the MIT license.  See
the LICENSE.md file for details.
