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
    echo "Package: *\nPin: release a=stable\nPin-Priority: 503\n\nPackage: *\nPin: release a=testing\nPin-Priority: 502\n\nPackage: ghc cabal-install\nPin: release a=testing\nPin-Priority: 505\n" | sudo tee /etc/apt/preferences.d/99tidal

    echo "# add stretch repos for working ghci\ndeb http://mirrordirector.raspbian.org/raspbian/ stretch main contrib non-free rpi\n" | sudo tee /etc/apt/sources.list.d/tidal.list

    # increase swap space
    sudo cp /etc/dphys-swapfile /home/pi/dphys-swapfile.bak
    echo "CONF_SWAPSIZE=1024" | sudo tee /etc/dphys-swapfile
    sudo service dphys-swapfile restart

    # update sources to persist changes
    sudo apt-get update

    sudo apt-get -y install build-essential libsndfile1-dev libsamplerate0-dev \
         liblo-dev libjack-jackd2-dev qjackctl jackd git \
         ghc/testing zlib1g-dev cabal-install/testing \
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
	git clone --recursive https://github.com/tidalcycles/Dirt.git
	cd Dirt
fi
make clean; make

# actually install tidal
cabal update
cabal install cabal
cabal install tidal

if [ "${arch}" = "armv7l" ]; then
    # reset swap size
    sudo mv /home/pi/dphys-swapfile.bak /etc/dphys-swapfile
    sudo service dphys-swapfile restart
fi

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

# setup audio hw access
sudo adduser $USER audio

if [ "${arch}" = "armv7l" ]; then
    echo '<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd"><busconfig><policy user="pi"><allow own="org.freedesktop.ReserveDevice1.Audio0" /><allow own="org.freedesktop.ReserveDevice1.Audio1" /></policy></busconfig>' | sudo tee "/etc/dbus-1/system.d/tidal.conf"
    echo 'options snd-usb-audio index=-2' | sudo tee "/etc/modprobe.d/tidal.conf"

    # make sure a bash_profile exists
    touch "$HOME/.bash_profile"
    # allow DBUS to work without X11
    echo 'export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket' | tee -a '$HOME/.bash_profile'
    echo "By default, tidal will use the raspberry pi on-board soundcard"
    echo "This will result in poor audio quality. Consider using a usb soundcard to improve playback quality!"
    echo "To make tidal use your usb soundcard, comment the onboard soundcard module in /etc/modules-load.d/modules.conf like this:"
    echo
    echo "# snd-bcm2835"
    echo
    echo "And change the index option for snd-usb-audio in /etc/modprobe.d/tidal.conf like this:"
    echo
    echo "options snd-usb-audio index=0"
    echo
    echo "Reboot the pi and make sure your usb soundcard is connected and speakers/headphones are plugged into the it."
    echo "WARNING: the above procedure will effectively TURN OFF the onboard soundcard!"
fi


# put starter on th desktop
cd ~/Desktop
rm -f start-tidal
wget http://yaxu.org/tmp/start-tidal
chmod u+x start-tidal
