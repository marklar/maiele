#!/bin/env ruby

# USAGE: see EOF

require 'fileutils'

$time_it_depth = 0
INDENT = '   '
def time_it(sym = nil)  # &block
  depth = $time_it_depth
  $time_it_depth += 1
  puts (INDENT * depth) + ">> #{sym}" if sym
  b4 = Time.now
  begin yield
  ensure
    $time_it_depth -= 1
    diff = Time.now-b4
    puts (INDENT * depth) +
      "<< #{sym ? sym.to_s+': ' : ''} #{diff * 1_000} ms | #{diff} sec | #{diff.to_f / 60} min"
    puts
  end
end

# Halt script after error, rather than attempting next steps.
def sys(str)
  system(str) || raise("ERROR: #{str}\n\n")
end

def do_it(dir_name, vertical, db_host, db_port, db_name, db_user, db_pwd)
  dump = 'product_codes.txt'

  time_it :dump_product_codes do
    sys("bin/dump_product_codes #{vertical} #{db_host} #{db_port} " + 
        "#{db_name} #{db_user} #{db_pwd} > #{dir_name}/#{dump}")
  end
  time_it :sort_product_codes_dump do
    sys "LC_ALL=C sort #{dir_name}/#{dump} > #{dir_name}/#{dump}.sort"
  end
  # makes both:
  #  + Str_tbl for codes
  #  + Int_ary for Glu IDs (one per code)
  time_it :make_product_codes_str_tbl do
    sys "bin/mk_product_codes_tbl #{dir_name} #{dump}.sort"
  end
  # add first-char index to Str_tbl to make Lexicon
  time_it :mk_lexicon_idx do
    sys "bin/mk_lexicon_idx #{dir_name} lex"
  end
  time_it :remove_dump_files do
    sys "rm -f #{dir_name}/#{dump} #{dir_name}/#{dump}.sort"
  end
end

begin
  (idx_root_dir, vertical, db_host, db_port, db_name, db_user, db_pwd) = ARGV
  if [idx_root_dir, vertical, db_host, db_port, db_name, db_user].any? {|s| s.nil? || s == '' }
    raise "Arg error"
  end
  db_port = db_port.to_i

  # Make Directory.
  dir_name = idx_root_dir + "/#{vertical}_product_codes"
  FileUtils.mkdir_p(dir_name, :mode => 0755)

  do_it(dir_name, vertical, db_host, db_port, db_name, db_user, db_pwd)
rescue Exception => e
  puts e
  puts <<USAGE
usage: build_product_codes.rb <idx_root_dir> <vertical> <db_host> <db_port> <db_name> <db_user> [<db_pwd>]
USAGE
end
