# Installed using vagrant-config.py from https://github.com/dominique-unruh/vagrant-config.git, file snippets/arch.rb, revision 9329859d53ff88601a3c85eed0d5929bf42341a8
# Forces amd64 box/provider, using qemu emulation on non-amd64 hosts (e.g. Apple Silicon). Defines enforce_amd64(config).
def enforce_amd64(config)
  $enforce_amd64_seen ||= {}
  return if $enforce_amd64_seen[config.object_id]
  $enforce_amd64_seen[config.object_id] = true

  config.vm.box_architecture = "amd64"

  host_cpu = RbConfig::CONFIG["host_cpu"]
  host_amd64 = %w[x86_64 amd64 x64].include?(host_cpu)
  return if host_amd64

  # Host is not amd64 (e.g. Apple Silicon). Native providers can't run amd64,
  # so force the qemu provider with x86_64 emulation (needs vagrant-qemu plugin).
  ENV["VAGRANT_DEFAULT_PROVIDER"] ||= "qemu"

  unless Vagrant.has_plugin?("vagrant-qemu")
    raise "amd64 on #{host_cpu} host needs the vagrant-qemu plugin: vagrant plugin install vagrant-qemu"
  end

  config.vm.provider "qemu" do |qe|
    qe.arch        = "x86_64"
    qe.machine     = "q35"
    qe.cpu         = "max"
    qe.net_device  = "virtio-net-pci"
  end
end