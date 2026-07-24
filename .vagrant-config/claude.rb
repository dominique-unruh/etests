def claude_code(config)
  $claude_code_seen ||= {}
  return if $claude_code_seen[config.object_id]
  $claude_code_seen[config.object_id] = true

  config.vm.provision "shell",
    name: "claude-code",
    privileged: false,
    inline: <<~'SH'
      set -euo pipefail
      curl -fsSL https://claude.ai/install.sh | bash
      "${HOME}/.local/bin/claude" --version
      echo "Authenticate on first use: run 'claude' (browser login) or set ANTHROPIC_API_KEY."
    SH
end
