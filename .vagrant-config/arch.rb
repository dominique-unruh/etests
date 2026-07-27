# Installed using vagrant-config.py from <no remote>, file snippets/arch.rb, revision <no revision>
def enforce_amd64(config)
  puts "[enforce_amd64] called, config.object_id=#{config.object_id}"

  $enforce_amd64_seen ||= {}
  if $enforce_amd64_seen[config.object_id]
    puts "[enforce_amd64] already seen this config, skipping"
    return
  end
  $enforce_amd64_seen[config.object_id] = true

  puts "[enforce_amd64] config.vm.box (before) = #{config.vm.box.inspect}"
  config.vm.box_architecture = "amd64"
  puts "[enforce_amd64] config.vm.box_architecture set to amd64"

  host_cpu = RbConfig::CONFIG["host_cpu"]
  host_os  = RbConfig::CONFIG["host_os"]
  host_amd64 = %w[x86_64 amd64 x64].include?(host_cpu)
  puts "[enforce_amd64] RbConfig host_cpu=#{host_cpu.inspect} host_os=#{host_os.inspect} host_amd64=#{host_amd64}"
  puts "[enforce_amd64] RUBY_PLATFORM=#{RUBY_PLATFORM.inspect}"
  puts "[enforce_amd64] ENV VAGRANT_DEFAULT_PROVIDER (before) = #{ENV['VAGRANT_DEFAULT_PROVIDER'].inspect}"

  if host_amd64
    puts "[enforce_amd64] host is amd64, no qemu/provider override needed, returning"
    return
  end

  # Host is not amd64 (e.g. Apple Silicon). Native providers can't run amd64,
  # so force the qemu provider with x86_64 emulation (needs vagrant-qemu plugin).
  puts "[enforce_amd64] host is NOT amd64, forcing qemu provider"
  ENV["VAGRANT_DEFAULT_PROVIDER"] ||= "qemu"
  puts "[enforce_amd64] ENV VAGRANT_DEFAULT_PROVIDER (after) = #{ENV['VAGRANT_DEFAULT_PROVIDER'].inspect}"

  has_qemu_plugin = Vagrant.has_plugin?("vagrant-qemu")
  puts "[enforce_amd64] Vagrant.has_plugin?('vagrant-qemu') = #{has_qemu_plugin}"

  unless has_qemu_plugin
    puts "[enforce_amd64] RAISING: vagrant-qemu plugin missing"
    raise "amd64 on #{host_cpu} host needs the vagrant-qemu plugin: vagrant plugin install vagrant-qemu"
  end

  puts "[enforce_amd64] configuring qemu provider block"
  config.vm.provider "qemu" do |qe|
    qe.arch        = "x86_64"
    qe.machine     = "q35"
    qe.cpu         = "max"
    qe.net_device  = "virtio-net-pci"
    puts "[enforce_amd64] qemu provider block: arch=#{qe.arch} machine=#{qe.machine} cpu=#{qe.cpu} net_device=#{qe.net_device}"
  end
  puts "[enforce_amd64] done"
end