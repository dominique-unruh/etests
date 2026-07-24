# Installs scalapy dependencies (scalapy itself is just a JVM dependency and pulled by sbt when needed)
def scalapy(config)
  $scalapy_seen ||= {}
  return if $scalapy_seen[config.object_id]
  $scalapy_seen[config.object_id] = true

  # python3-dev ships libpython3.X.so that ScalaPy loads via JNA.
  apt_install(config, "python3-dev")

  config.vm.provision "shell",
    name: "scalapy",
    inline: <<~'SH'
      set -euo pipefail
      # e.g. "python3.13" — matches the installed interpreter.
      pyver="$(python3 -c 'import sys; print(f"python{sys.version_info.major}.{sys.version_info.minor}")')"

      # ScalaPy reads SCALAPY_PYTHON_LIBRARY to pick the lib; JNA needs its path.
      cat > /etc/profile.d/scalapy.sh <<EOF
      export SCALAPY_PYTHON_LIBRARY="${pyver}"
      EOF
      chmod +x /etc/profile.d/scalapy.sh
    SH
end
