#!/usr/bin/env bash
# Installed using devcontainer-config from <no remote>, dir features/sbt, content-hash 3eac6bfa60b5b685c70cb37201250b9263acfbad19f028fb209756693cb866b7
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install --no-install-recommends -y apt-transport-https curl gnupg ca-certificates

# Official scala-sbt apt repo + signing key, per
# https://www.scala-sbt.org/download.html
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" > /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" \
  | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg

apt-get update
apt-get install --no-install-recommends -y sbt
