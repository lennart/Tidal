#!/usr/bin/env bash
set -e

# This script installs tidal.
# It will get all its dependencies and put
# files into ~/tidal and a start script on the
# Desktop.
#
# This script has been tested with Ubuntu 13.10
# and Debian.

# prepare system
mkdir -p ~/tidal
cd ~/tidal

# get system architecture
arch=$(uname -m)

if [ "${arch}" = "armv7l" ]; then
# pin ghc to raspian testing to make ghci work
    echo "Package: *\nPin: release a=stable\nPin-Priority: 503\n\nPackage: *\nPin: release a=testing\nPin-Priority: 502\n\nPackage: ghc\nPin: release a=testing\nPin-Priority: 505\n" | sudo tee /etc/apt/preferences.d/99tidal

    echo "# add stretch repos for working ghci\ndeb http://mirrordirector.raspbian.org/raspbian/ stretch main contrib non-free rpi\n" | sudo tee /etc/apt/sources.list.d/tidal.list

    # update sources to persist changes
    sudo apt-get update

    sudo apt-get -y install build-essential libsndfile1-dev libsamplerate0-dev \
         liblo-dev libjack-jackd2-dev qjackctl jackd git \
         ghc/testing zlib1g-dev cabal-install \
         emacs23 haskell-mode
else
# standard linux dependency installation
    sudo apt-get -y install build-essential libsndfile1-dev libsamplerate0-dev \
         liblo-dev libjack-jackd2-dev qjackctl jackd git \
         ghc zlib1g-dev cabal-install \
         emacs24 haskell-mode
fi
# install Dirt
if [ -d "Dirt" ]; then
	cd Dirt
	if [ ! -d ".git" ]; then
		>&2 echo "no git repository for 'Dirt' ... don't know what to do"
		exit 1
	fi
	git pull
else
	git clone https://github.com/tidalcycles/Dirt.git
	cd Dirt
fi
make clean; make

# actually install tidal
cabal update
cabal install cabal
cabal install tidal

# configure Emacs
mkdir -p ~/tidal/emacs
rm -f ~/tidal/emacs/tidal.el
wget -O ~/tidal/emacs/tidal.el https://raw.githubusercontent.com/tidalcycles/Tidal/master/tidal.el
touch ~/.emacs
if [ `grep "(add-to-list 'load-path \"~/tidal/emacs\")" ~/.emacs | wc -l` -ne 1 ]; then
	echo "(add-to-list 'load-path \"~/tidal/emacs\")" >> ~/.emacs
fi
if [ `grep "(require 'tidal)" ~/.emacs | wc -l` -ne 1 ]; then
	echo "(require 'tidal)" >> ~/.emacs
fi
sudo adduser $USER audio

# put starter on th desktop
cd ~/Desktop
rm -f start-tidal
wget http://yaxu.org/tmp/start-tidal
chmod u+x start-tidal
