#!/bin/env ruby

require 'yaml'

$yaml = YAML.load_file('cfg/config.yml')

# :: (Symbol, Int) -> String
def str(domain, num)
  ['script/build_one_idx.rb',
   $yaml[:idx_root_dir],
   domain,
   num,
   $yaml[:db_host],
   $yaml[:db_port],
   $yaml[:db_name],
   $yaml[:tag_db_name],
   $yaml[:db_user],
   $yaml[:db_pwd]
  ].map {|i| i.to_s }.join(' ')
end

domain2parts = {
  :games_with_platforms => 1,
  #----
  :games_psp      => 1,
  :games_ps2      => 1,
  :games_ps3      => 1,
  :games_xbox     => 1,
  :games_xbox360  => 1,
  :games_ds       => 1,
  :games_3ds      => 1,
  :games_gamecube => 1,
  :games_wii      => 1,
  #----
  :games          => 1,
  :ce             => 1,
  :tablets        => 1,
  :mp3s           => 1,
  :phones         => 1,
  :laptops        => 1,
  :accessories    => 1,
  :browse         => 5,
}

domains = ARGV.length > 0 ? ARGV.map {|s| s.to_sym } : domain2parts.keys

# automatically build games sub-domains when building games
if domains.include?(:games)
  games_domains = domain2parts.keys.select{|s| s.to_s =~ /^games/}
  domains << games_domains
  domains.flatten!.uniq!
end

domains.each do |d|
  puts "#{d}"
  n = domain2parts[d]
  system(str(d, n))
end
