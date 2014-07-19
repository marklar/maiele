#!/usr/bin/env ruby

require 'open-uri'
require 'net/http'
require 'rubygems'
require 'json'

# Usage: ./script/check_for_missing_tokens.rb [opt: index_name]

# This script will run through all of the tokens created by the dump_postings bin and check a locally running
# lsd at port 40404 to see if there are any discrepancies. Browse index is currently left out as unless it
# has rgs data it will fail to find any results.

ALL_INDEXES = ['games_xbox', 'games', 'accessories', 'games_xbox360', 'ce', 'games_with_platforms', 'games_ds',
               'tablets', 'games_psp', 'games_3ds', 'mp3s', 'games_ps2', 'games_gamecube', 'phones', 'games_ps3',
               'games_wii', 'laptops']#, browse'

def check_index(index)
  print "Checking #{index}:\n"
  results_file = File.open("idx/#{index}/postings.txt")
  search_strings = results_file.lines.inject([]) do |strings, line|
    strings << URI::encode(line.match(/^(\S+)\s*\d{9}/)[1]).gsub('&', "%26").gsub('?', "%3F")
  end
  uniques = search_strings.uniq.sort
  i = 0
  tokens = uniques.inject([]) do |bad_tokens, string|
    print "\r"
    print "(#{'%.2f' % (i.to_f / uniques.length.to_f * 100)}%) #{string}                           "
    i += 1
    results = JSON.parse(
      Net::HTTP.get(URI.parse("http://localhost:40404/maiele/results.js?query=#{string}&domains=#{index}")))
    if results["success"] == true
      search_results = results["results"][0]["results"]
      if search_results.empty?
        bad_tokens << string
      end
    else
      bad_tokens << string + " caused #{results["exception"]}"
    end

    bad_tokens
  end
  print "\r"
  print "(100.00%)                                            \n"
  if tokens.length > 0
    puts tokens.map{|e| e.gsub("%20", " ")}
    puts "\n"
  else
    puts "No errors found in #{index}\n\n"
  end
end

begin
  if ARGV.empty?
    for index in ALL_INDEXES
      check_index index
    end
  else
    index = ARGV
    check_index index
  end

end