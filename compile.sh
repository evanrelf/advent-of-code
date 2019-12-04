#!/bin/sh

source=$1
binary=$(echo "$source" | sed "s/.hs//")
object=$(echo "$source" | sed "s/.hs/.o/")
interface=$(echo "$source" | sed "s/.hs/.hi/")

ghc -O3 "$source" -o "$binary" && rm -f "$object" "$interface"
