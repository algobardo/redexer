#!/bin/sh

set -e

TARGET=src/main
FLAGS=" -use-ocamlfind -pkgs sha,batteries"
OCAMLBUILD=ocamlbuild

ocb()
{
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)  ocb -clean;;
    native) ocb $TARGET.native;;
    byte)   ocb $TARGET.byte;;
    dbyte)   ocb $TARGET.d.byte;;
    all)    ocb $TARGET.native $TARGET.byte $TARGET.d.byte;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi