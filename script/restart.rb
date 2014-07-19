#!/usr/bin/env ruby

require 'yaml'
require 'ftools'

pwd  = File.dirname(__FILE__)
prog = File.basename(__FILE__)
Dir.chdir(pwd + '/..')

YAML_FILE  = 'cfg/config.yml'
hash = YAML.load_file(YAML_FILE)

start_p = ( prog =~ /drop_acid|restart/)
stop_p  = ( prog =~ /come_down|restart/)

# Ensure that indices are in core.  (Not strictly necessary.)
# `script/load_indices.rb #{hash[:root_dir]}`

# Create JSON config for ocaml.
`script/yaml_2_json.rb #{YAML_FILE}`

# pkill
#   -o : kill the oldest
#   -f : use pattern to match against the full command line
#

# Ports may be passed in on cmd line.
# 
# NB: If ARGV not used, and the ports in config.yml have changed
#     whilst the daemons run, some daemons might not get terminated.
#
hash[:ports] = ARGV if ARGV.length > 0

# Stop/start processes running on the specified ports.
hash[:ports].each do |p|
  if stop_p
    s = 1
    #puts "terminating lsd (if any) on port: #{p}"
    `pkill -TERM -o -f 'lsd #{p}'`
    while !`pgrep -f 'lsd #{p}'`.empty?
      if s < 32
        sleep s
        s = s*2
      else
        puts " #{p} appears wedged, resorting to kill -9"
        `pkill -9 -o -f 'lsd #{p}'`
        sleep 2
      end
    end
  end

  if start_p
    #puts "forking lsd on port: #{p}"
    fork do
      STDIN.close; STDOUT.close; STDERR.close
      exec "bin/lsd #{p}"
    end
  end
end

# Fire off in-stock updater.

if start_p
  `bin/lsync`
end


# TODO: show usage (-h / --help).
