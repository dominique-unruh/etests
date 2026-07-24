#!/usr/bin/env bash
set -euo pipefail
export VAGRANT_DISABLE_STRICT_DEPENDENCY_ENFORCEMENT=1

case "${1:-}" in
  provision)
    vagrant up --provision
    ;;
  rebuild)
    vagrant destroy -f
    vagrant up --provision
    ;;
  restart)
    vagrant reload --provision
    ;;
  *)
    vagrant up
    vagrant ssh -c "$@"
    ;;
esac
