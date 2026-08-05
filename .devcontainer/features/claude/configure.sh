#!/usr/bin/env bash
set -euo pipefail

# Claude Code's state (~/.claude, ~/.claude.json) lives in the container's
# writable layer by default, so a rebuild wipes it. Symlink it into the
# bind-mounted workspace instead, under .devcontainer/persistent/ (survives
# rebuilds since it's on the host, not the container fs). postCreateCommand's
# cwd is the workspace folder per the devcontainer spec, so $PWD here is it.
persist_base="${PWD}/.devcontainer/persistent"
persist_root="${persist_base}/claude"
mkdir -p "$persist_root"
[ -f "${persist_base}/.gitignore" ] || echo '*' > "${persist_base}/.gitignore"

persist_dir() {
  local target="$1" store="$2"
  [ -L "$target" ] && return
  if [ -e "$store" ]; then
    rm -rf "$target"
  elif [ -e "$target" ]; then
    mv "$target" "$store"
  else
    mkdir -p "$store"
  fi
  ln -s "$store" "$target"
}

persist_file() {
  local target="$1" store="$2"
  [ -L "$target" ] && return
  if [ -e "$store" ]; then
    rm -f "$target"
  elif [ -e "$target" ]; then
    mv "$target" "$store"
  fi
  ln -s "$store" "$target"
}

persist_dir "${HOME}/.claude" "${persist_root}/home"
persist_file "${HOME}/.claude.json" "${persist_root}/claude.json"

dangerous="$(cat /usr/local/share/devcontainer-config/claude/dangerous-permissions 2>/dev/null || echo false)"
if [ "$dangerous" = "true" ]; then
  mkdir -p "${HOME}/.claude"
  settings="${HOME}/.claude/settings.json"
  [ -f "$settings" ] || echo '{}' > "$settings"
  tmp="$(mktemp)"
  jq '.permissions.defaultMode = "bypassPermissions"' "$settings" > "$tmp"
  mv "$tmp" "$settings"
fi
