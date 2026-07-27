# Installed using vagrant-config.py from <no remote>, file snippets/pip.rb, revision <no revision>
def pip_install(config, *packages)
  packages = packages.flatten
  $pip_install_seen ||= {}
  packages = packages.reject { |p| $pip_install_seen[[config.object_id, p]] }
  packages.each { |p| $pip_install_seen[[config.object_id, p]] = true }
  return if packages.empty?

  apt_install(config, "python3-pip")

  config.vm.provision "shell",
    name: "pip-install-#{packages.join('-')}",
    inline: <<~SH
      set -euo pipefail
      # Ubuntu marks the system Python as externally-managed (PEP 668);
      # --break-system-packages allows a system-wide pip install anyway.
      pip3 install --break-system-packages #{packages.join(' ')}
    SH
end