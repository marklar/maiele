#!/usr/bin/env ruby

require 'yaml'
YAML_FILE  = 'config.yml'
hash = YAML.load_file(YAML_FILE)
`./yaml_2_json.rb #{YAML_FILE}`
puts `./searcher_test #{ARGV.join(' ')}`
# puts `./ids_test #{ARGV.join(' ')}`
# puts `./game_ids_test`
# `rm ./.config.js`
