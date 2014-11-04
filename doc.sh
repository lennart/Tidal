#!/bin/sh

set -o errexit
set -o nounset

if [ -d ./haddock ]; then
  rm -R ./haddock
fi

mkdir -p ./haddock/md

haddock --hoogle -o haddock Sound.Tidal.Pattern --hide=Sound.Tidal.Time --hide=Sound.Tidal.Utils

sed -n  -f haddock2md.sed ./haddock/main.txt | awk -f ./splitmd.awk