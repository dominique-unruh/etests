# Installed using vagrant-config.py from https://github.com/dominique-unruh/vagrant-config.git, file snippets/apt.rb, revision 9329859d53ff88601a3c85eed0d5929bf42341a8
# Updates apt and installs packages via apt-get. Defines apt_update(config), apt_install(config, *packages).
def apt_update(config)
  $apt_update_seen ||= {}
  return if $apt_update_seen[config.object_id]
  $apt_update_seen[config.object_id] = true

  config.vm.provision "shell",
    name: "apt-update",
    inline: <<~'SH'
      set -euo pipefail
      export DEBIAN_FRONTEND=noninteractive
      apt-get update
    SH
end

def apt_install(config, *packages)
  packages = packages.flatten
  $apt_install_seen ||= {}
  packages = packages.reject { |p| $apt_install_seen[[config.object_id, p]] }
  packages.each { |p| $apt_install_seen[[config.object_id, p]] = true }
  return if packages.empty?

  apt_update(config)

  config.vm.provision "shell",
    name: "apt-install-#{packages.join('-')}",
    inline: <<~SH
      set -euo pipefail
      export DEBIAN_FRONTEND=noninteractive
      apt-get install --no-install-recommends -y #{packages.join(' ')}
    SH
end
