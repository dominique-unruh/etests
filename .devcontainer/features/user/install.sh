#!/usr/bin/env bash
# Installed using devcontainer-config from <no remote>, dir features/user, content-hash cb8f9ab4e3e71a7df7953bb98117dd9a5e699f02d34ed80acdd7f5e2143af9e3
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive

username="${USERNAME:-dev}"

apt-get update
apt-get install --no-install-recommends -y sudo

# ubuntu:24.04 ships a built-in "ubuntu" user at uid 1000. Left in place, our
# own useradd below lands on 1001+, so devcontainer's updateRemoteUserUID
# (which remaps remoteUser to the host user's uid, usually 1000, but no-ops
# on a uid collision) silently fails to align it. Free uid/gid 1000 first.
if [ "$username" != "ubuntu" ] && id -u ubuntu >/dev/null 2>&1; then
  userdel -r ubuntu 2>/dev/null || userdel ubuntu || true
  getent group ubuntu >/dev/null 2>&1 && groupdel ubuntu || true
fi

if ! id -u "$username" >/dev/null 2>&1; then
  useradd --create-home --shell /bin/bash "$username"
fi

echo "${username} ALL=(ALL) NOPASSWD:ALL" > "/etc/sudoers.d/${username}"
chmod 0440 "/etc/sudoers.d/${username}"
