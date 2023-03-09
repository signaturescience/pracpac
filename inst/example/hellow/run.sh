#!/bin/bash

## get tranlsation language from first argument
## default to :fr
TOLANG="${1:-:fr}"
HELLO=$(Rscript /pre.R)

trans "$HELLO" "$TOLANG"
