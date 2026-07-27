puts "[Vagrantfile] start, __dir__=#{__dir__}"
puts "[Vagrantfile] Vagrant.VERSION=#{Vagrant::VERSION}"
puts "[Vagrantfile] ARGV=#{ARGV.inspect}"
puts "[Vagrantfile] ENV VAGRANT_CWD=#{ENV['VAGRANT_CWD'].inspect}"
puts "[Vagrantfile] ENV VAGRANT_VAGRANTFILE=#{ENV['VAGRANT_VAGRANTFILE'].inspect}"
puts "[Vagrantfile] ENV VAGRANT_DEFAULT_PROVIDER (start) = #{ENV['VAGRANT_DEFAULT_PROVIDER'].inspect}"
puts "[Vagrantfile] Dir.pwd=#{Dir.pwd}"

# Load configuration helpers
config_files = Dir[File.expand_path(".vagrant-config/*.rb", __dir__)].sort
puts "[Vagrantfile] loading config helpers: #{config_files.inspect}"
config_files.each do |f|
  puts "[Vagrantfile] require #{f}"
  require f
end
puts "[Vagrantfile] all config helpers loaded"

Vagrant.configure("2") do |config|
  puts "[Vagrantfile] inside Vagrant.configure block, config.object_id=#{config.object_id}"

  config.vm.box = "bento/ubuntu-26.04"
  puts "[Vagrantfile] config.vm.box set to #{config.vm.box.inspect}"

  puts "[Vagrantfile] calling enforce_amd64"
  enforce_amd64(config)
  puts "[Vagrantfile] returned from enforce_amd64, config.vm.box now #{config.vm.box.inspect}, box_architecture=#{config.vm.box_architecture.inspect}"

  config.vm.network "forwarded_port", guest: 9000, host: 9000
  puts "[Vagrantfile] forwarded_port 9000 configured"

  # Install Claude Code
  puts "[Vagrantfile] calling claude_code"
  claude_code(config)
  # Install the caveman Claude Code plugin
  puts "[Vagrantfile] calling caveman"
  caveman(config)
  # Install sbt
  puts "[Vagrantfile] calling sbt"
  sbt(config)
  # Install node.js (used to compile TypeScript code inside webapp etc.)
  puts "[Vagrantfile] calling apt_install nodejs npm"
  apt_install(config, "nodejs", "npm")
  # Install Docker
  puts "[Vagrantfile] configuring docker provisioner"
  config.vm.provision "docker"
  # Install Sympy
  puts "[Vagrantfile] calling pip_install sympy"
  pip_install(config, "sympy")
  # Configure ScalaPy to find the system libpython.
  puts "[Vagrantfile] calling scalapy"
  scalapy(config)

  # Configure utility commands
  puts "[Vagrantfile] registering command 'webapp'"
  command(config, "webapp", "cd /vagrant && sbt 'project webapp; run'")

  puts "[Vagrantfile] end of Vagrant.configure block, final config.vm.box=#{config.vm.box.inspect}"
end
puts "[Vagrantfile] end of file"
