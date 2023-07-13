#!/usr/bin/env bash
set -eu

vsn=$(cat VERSION)

update_for_arch() {
  local arch=$1
  local file_type=$2
  local compressed=$3

  echo "Going for $arch:"
  rm -rf "$arch"
  mkdir "$arch"
  pushd "$arch" > /dev/null || true

  local remote
  remote=https://github.com/koalaman/shellcheck/releases/download/"$vsn"/"$compressed"
  echo "  * downloading $remote"
  wget --quiet "$remote"

  echo "  * expanding archive"
  case "$file_type" in
    "tar.xz")
      tar zxf "$compressed"
    ;;
    "zip")
      unzip -qq "$compressed"
    ;;
  esac

  echo "  * removing build artifacts"
  rm -f "$compressed"
  if [ -d shellcheck-"$vsn" ]; then
    mv shellcheck-"$vsn"/* .
    rm -rf shellcheck-"$vsn"
  fi
  popd > /dev/null || true
  local size
  size=$(du -sh "$arch" | awk '{print $1}')
  echo "  * size: $size"
  echo "... done for $arch!"
}

# Linux, x86_64
update_for_arch "linux.x86_64" "tar.xz" "shellcheck-$vsn.linux.x86_64.tar.xz"
echo

# macOS, x86_64
update_for_arch "darwin.x86_64" "tar.xz" "shellcheck-$vsn.darwin.x86_64.tar.xz"
echo

# Windows, x86
update_for_arch "windows.x86" "zip" "shellcheck-$vsn.zip"
