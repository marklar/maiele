#!/usr/bin/env ruby

# USAGE: see EOF

require 'fileutils'

RM_TXT_FILES = true
CONCAT_MTXS_P = true
DEBUG = !ENV['DEBUG'].nil?

$time_it_depth = 0
INDENT = '   '
def time_it(sym = nil)  # &block
  if DEBUG
    depth = $time_it_depth
    $time_it_depth += 1
    puts (INDENT * depth) + ">> #{sym}" if sym
    b4 = Time.now
  end
  begin yield
  ensure
    if DEBUG
      $time_it_depth -= 1
      diff = Time.now-b4
      puts (INDENT * depth) +
        "<< #{sym ? sym.to_s+': ' : ''} " +
        "#{diff * 1_000} ms | #{diff} sec | #{diff.to_f / 60} min"
      puts
    end
  end
end

# Halt script after error, rather than attempting next steps.
def sys(str)
  system(str) || raise("ERROR: #{str}\n\n")
end

# Pause, to allow you to look at artefacts (files, dirs, etc.)
def pause(str='')
  puts "pause: #{str}"
  STDIN.getc
end

DOMAINS_W_FAUX_RES = ['games_platforms']

#--- BUILD RESULTS TABLE ---#
def build_results_table(dir_name, domain_name, db_host, db_port, db_name, browse_in_stock_db_name,
                        db_user, db_pwd, limit)  # limit may be nil
  time_it :collect_info_from_DB do
    sys("bin/write_str_file #{domain_name} #{db_host} #{db_port} #{db_name} " +
        "#{db_user} #{db_pwd} #{limit} > #{dir_name}/results.txt")
  end
  time_it :sort_strings do
    sys "LC_ALL=C sort #{dir_name}/results.txt > #{dir_name}/results.txt.sort"
  end
  time_it :create_results_table do
    sys "bin/read_str_file #{dir_name} #{domain_name}"
    `rm #{dir_name}/results.txt*` if RM_TXT_FILES
  end
  time_it :normalize_pop_ranks do
    sys "bin/fix_pops #{dir_name}"
  end

  if DOMAINS_W_FAUX_RES.include? domain_name
    time_it :create_faux_records do
      sys "bin/create_faux #{dir_name}"
    end
    time_it :move_files do
      # rm (old) 'res'.
      %w(res.pop.data
         res.target_ids.data res.target_ids.offs
         res.text.data       res.text.offs
         res.title_only.data res.title_only.offs
         res.sort_val.data
      ).each {|fn| `rm #{dir_name}/#{fn}` }
      # move 'new.res' to 'res'.
      %w(res.pop.data
         res.target_ids.data res.target_ids.offs
         res.text.data       res.text.offs
         res.is_faux.data
      ).each {|fn| `mv #{dir_name}/new.#{fn} #{dir_name}/#{fn}` }
    end
  else
    time_it :rm_files_not_needed_because_not_creating_faux_records do
      %w(res.title_only.data res.title_only.offs res.sort_val.data).each do |fn|
        `rm #{dir_name}/#{fn}`
      end
    end
  end
  
  time_it :create_pop_index do
    sys "bin/make_pop_idx #{dir_name}"
  end
  time_it :make_target_ids_index do
    sys "bin/make_target_idx #{dir_name} res.target_ids"
  end
  
  if domain_name == 'browse'
    time_it :make_tag_table do
      sys "bin/tag_tbl_mkr #{dir_name} #{db_host} #{db_port} #{db_name} #{db_user} #{db_pwd}"
    end
    time_it :make_glu_table do
      sys "bin/glu_tbl_mkr #{dir_name} #{db_host} #{db_port} #{browse_in_stock_db_name} #{db_name} #{db_user} #{db_pwd}"
    end
  end

end


#--- BUILD REVERSE INDEX ---#
def build_reverse_index(dir_name, num_partitions)
  time_it :dump_postings do
    sys "bin/dump_postings #{dir_name} > #{dir_name}/postings.txt"
  end
  time_it :sort_postings do
    sys "LC_ALL=C sort #{dir_name}/postings.txt > #{dir_name}/postings.txt.sort"
  end
  time_it :create_complete_only_matrix do
    sys "bin/postings_file_rdr #{dir_name} postings.txt.sort"
  end
  time_it :rm_files do
    `rm #{dir_name}/postings.txt*` if RM_TXT_FILES
  end
  # for each char, the ID of first lexeme.
  time_it :build_lexicon_index do
    sys "bin/mk_lexicon_idx #{dir_name}"  # 'lex'
  end
  
  time_it :partition_matrix do    # creates by-value partitions
    sys "bin/partition_mtx #{dir_name} #{num_partitions}"
  end
  # pause 'after partition'

  time_it :denormalize_matrices do
    # Creates LOTS of denormalized, by-key partitions.
    # JoCaml minions all work on a single partition at a time,
    # each denormalizing one shard.
    # They create their temp output in root dir.
    # The final result of each iteration of loop below goes in "big/" subdir.
    num_minions = 10  # independent of number of partitions.  relates to number of cores.
    (0...num_partitions).each do |num|
      time_it "partition: #{num}".to_sym do
        sys "bin/jo_denormalize_mtx #{dir_name} #{num} #{num_minions}"
        `rm -f #{dir_name}/mtx.#{num}.*`
      end
    end
  end
  # pause 'after denormalize *and* rm mtx.<i>.*'

  time_it :rm_files do
    `mv #{dir_name}/big/* #{dir_name}/.`
    `rmdir #{dir_name}/big`
    `rm #{dir_name}/mtx.ids.*`
  end
  # pause 'after mv denormed from big to main *and* rm mtx.idx.*'

  if CONCAT_MTXS_P
    time_it :concat_matrices do
      sys "bin/concat_mtxs #{dir_name}"
    end

    time_it :mv_files do
      # rm partition matrices
      (0...num_partitions).each do |i|
        `rm -f #{dir_name}/mtx.#{i}.*`
      end
      # mv cat.* files into final position
      ['offs', 'data'].each do |o_d|
        suffix = 'ids.' + o_d
        `mv #{dir_name}/cat.mtx.#{suffix} #{dir_name}/mtx.0.#{suffix}`
      end
    end
    
    time_it :add_pop_postings do
      sys "bin/add_pop_postings #{dir_name}"
    end
  end
end

def build_product_codes(dir_name, vertical, db_host, db_port, db_name, db_user, db_pwd)
  dump = 'product_codes.txt'

  time_it :dump_product_codes do
    sys("bin/dump_product_codes #{dir_name} #{vertical} #{db_host} #{db_port} " + 
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
    sys "bin/mk_lexicon_idx #{dir_name} code.lex"
  end
  time_it :remove_dump_files do
    sys "rm -f #{dir_name}/#{dump} #{dir_name}/#{dump}.sort"
  end
  time_it :mk_glu_2_codes do
    sys "bin/mk_glu_2_codes #{dir_name}"
  end
  time_it :remove_min_max_glu_id_file do
      sys "rm -f #{dir_name}/min_max_glu_id"
    end
end



$domains = %w(games ce tablets mp3s phones accessories laptops browse
  games_with_platforms
  games_psp games_ps2 games_ps3
  games_xbox games_xbox360
  games_ds games_3ds games_gamecube games_wii
)

begin
  (idx_root_dir, domain_name, num_parts_str,
   db_host, db_port, db_name, tag_db_name, db_user, db_pwd, limit_str) = ARGV
  unless $domains.include? domain_name
    raise "Invalid domain name: '#{domain_name}'."
  end
  if [db_host, db_port, db_name, db_user].any? {|s| s.nil? || s == '' }
    raise "Arg error"
  end
  db_port = db_port.to_i

  # Create JSON config for ocaml.
  system('script/yaml_2_json.rb cfg/config.yml')

  dir_name = idx_root_dir + '/' + domain_name
  FileUtils.mkdir_p(dir_name, :mode => 0755)

  # -- results --
  build_results_table(dir_name, domain_name, db_host, db_port,
                      (domain_name == 'browse' ? tag_db_name : db_name), db_name, db_user, db_pwd, limit_str && limit_str.to_i)

  # -- reverse index --
  build_reverse_index(dir_name, num_parts_str.to_i)

  # -- product codes --
  if %w(games tablets mp3s phones accessories).include?(domain_name)
    time_it :build_product_codes do
      build_product_codes(dir_name, domain_name, db_host, db_port, db_name, db_user, db_pwd)
    end
  end
  # Sorry.
rescue Exception => e
  puts e
  puts <<USAGE
usage: build_one_idx.rb <idx_root_dir> <domain_name> <num_partitions> \
<db_host> <db_port> <db_name> <tag_db_name> <db_user> [<db_pwd>]
       where <domain_name> :=
         | #{$domains.join("\n         | ")}
USAGE
end
