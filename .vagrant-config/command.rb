# Installed using vagrant-config.py from <no remote>, file snippets/command.rb, revision <no revision>
def command(config, name, content)
  $command_seen ||= {}
  key = [config.object_id, name]
  if $command_seen.key?(key)
    if $command_seen[key] != content
      raise "command #{name.inspect} defined twice with different content"
    end
    return
  end
  $command_seen[key] = content

  # Pass name/content through env so no shell escaping of `content` is needed.
  config.vm.provision "shell",
    name: "command-#{name}",
    privileged: false,
    env: { "CMD_NAME" => name, "CMD_CONTENT" => content },
    inline: <<~'SH'
      set -euo pipefail
      mkdir -p "${HOME}/.local/bin"
      printf '%s' "$CMD_CONTENT" > "${HOME}/.local/bin/${CMD_NAME}"
      chmod +x "${HOME}/.local/bin/${CMD_NAME}"
    SH
end