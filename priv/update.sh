#!/usr/bin/env bash
set -eu

# Linux, x86_64
ARCH=linux.x86_64
rm -rf $ARCH
mkdir $ARCH
pushd $ARCH
TARBALL=shellcheck-stable.$ARCH.tar.xz
wget https://github.com/koalaman/shellcheck/releases/download/stable/$TARBALL
tar zxvf $TARBALL
rm -f $TARBALL
mv shellcheck-stable/* .
rm -rf shellcheck-stable
popd

# macOS, x86_64
ARCH=darwin.x86_64
rm -rf $ARCH
mkdir $ARCH
pushd $ARCH
TARBALL=shellcheck-stable.$ARCH.tar.xz
wget https://github.com/koalaman/shellcheck/releases/download/stable/$TARBALL
tar zxvf $TARBALL
rm -f $TARBALL
mv shellcheck-stable/* .
rm -rf shellcheck-stable
popd

# Windows, x86
ARCH=windows.x86
rm -rf $ARCH
mkdir $ARCH
pushd $ARCH
ZIPFILE=shellcheck-stable.zip
wget https://github.com/koalaman/shellcheck/releases/download/stable/$ZIPFILE
unzip $ZIPFILE
rm -f $ZIPFILE
popd
