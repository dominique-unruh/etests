# Installed using vagrant-config.py from <no remote>, file snippets/caveman.rb, revision <no revision>
def caveman(config)
  $caveman_seen ||= {}
  return if $caveman_seen[config.object_id]
  $caveman_seen[config.object_id] = true

  # Requires Claude Code (see claude.rb).
  config.vm.provision "shell",
    name: "caveman",
    privileged: false,
    inline: <<~'SH'
      set -euo pipefail
      export PATH="${HOME}/.local/bin:${PATH}"
      claude plugin marketplace add JuliusBrussee/caveman
      claude plugin install caveman@caveman
    SH
end