#!/usr/bin/env bash
# Installed using devcontainer-config from https://github.com/dominique-unruh/devcontainer-config.git, dir features/apt, content-hash 917b0f2543d31a0a57fa5c57f212b4b59d18eea411f44d9c9b0ee35f6abf1787
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive

packages="${PACKAGES:-}"

apt-get update

if [ -n "$packages" ]; then
  # shellcheck disable=SC2086
  apt-get install --no-install-recommends -y $packages
fi
