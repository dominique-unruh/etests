#!/usr/bin/env bash
set -euo pipefail
export PATH="${HOME}/.local/bin:/usr/local/bin:${PATH}"

# Marketplace repo is public; force https instead of ssh so the clone
# works even without an SSH key/agent available inside the container.
git config --global url."https://github.com/".insteadOf "git@github.com:"

claude plugin marketplace add JuliusBrussee/caveman
claude plugin install caveman@caveman

# The plugin's SessionStart hook recomputes the active mode from scratch on
# every session (env var -> repo-local .caveman.json -> this user config ->
# hardcoded "full" fallback) and overwrites .caveman-active accordingly, so
# writing that flag file directly here is pointless — it'd just get
# clobbered at the next session start. Write the actual config source
# instead (see caveman-config.js's getDefaultMode/getConfigPath).
mode="$(cat /usr/local/share/devcontainer-config/caveman/mode 2>/dev/null || echo ultra)"
config_dir="${XDG_CONFIG_HOME:-${HOME}/.config}/caveman"
mkdir -p "$config_dir"
printf '{"defaultMode":"%s"}\n' "$mode" > "${config_dir}/config.json"

# Statusline script lives under a content-hashed cache dir (varies per plugin
# version) — locate it instead of hardcoding, and merge into settings.json
# instead of overwriting it (other features may already have written keys
# there).
statusline_script="$(find "${HOME}/.claude/plugins/cache/caveman" -name caveman-statusline.sh -print -quit 2>/dev/null || true)"
if [ -n "$statusline_script" ]; then
  settings="${HOME}/.claude/settings.json"
  [ -f "$settings" ] || echo '{}' > "$settings"
  tmp="$(mktemp)"
  jq --arg cmd "bash \"${statusline_script}\"" \
    '.statusLine = {type: "command", command: $cmd}' \
    "$settings" > "$tmp"
  mv "$tmp" "$settings"
fi
