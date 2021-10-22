#!/bin/sh

basicTest() {
  fileName=$1

  idris2 --no-color -p contrib -p xml -c "$fileName" -x main

  rm -rf build
}
