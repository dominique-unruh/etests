def sbt(config)
  $sbt_seen ||= {}
  return if $sbt_seen[config.object_id]
  $sbt_seen[config.object_id] = true

  apt_install(config, "curl", "default-jdk")

  config.vm.provision "shell",
    name: "sbt",
    privileged: false,
    inline: <<~'SH'
      set -euo pipefail
      mkdir -p "${HOME}/.local/bin"
      curl -fsSL "https://github.com/coursier/launchers/raw/master/cs-$(uname -m)-pc-linux.gz" \
        | gunzip > "${HOME}/.local/bin/cs"
      chmod +x "${HOME}/.local/bin/cs"
      "${HOME}/.local/bin/cs" install sbt --install-dir "${HOME}/.local/bin"
    SH
end
