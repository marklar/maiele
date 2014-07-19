#!/usr/bin/env ruby

#
# - read YAML file.
# - create .js config file for ocaml
#

require 'yaml'
require 'rubygems'
require 'json/ext'

if !ARGV[0]
  puts 'USAGE: script/yaml_2_json.rb <yml_file_name>'
  exit
end

JSON_FILE  = 'cfg/.config.js'

def idx_subdirs(root_dir)
  inodes = Dir.new(root_dir).entries.reject {|s| s =~ /^\./ }
  inodes.select {|s| File.directory?(root_dir + '/' + s) }
end

def modify_config_hash(hash)
  hash[:indices] = []
  idx_subdirs(hash[:idx_root_dir]).each do |dir_name|
    hash[:indices] << {
      :dir  => dir_name,
      :name => dir_name
    }
  end
  hash.delete :sub_dirs
  hash[:idx_root_dir] += '/'
  hash
end

File.open(JSON_FILE, 'w') do |f|
  f << modify_config_hash(YAML.load_file(ARGV[0])).to_json
end
