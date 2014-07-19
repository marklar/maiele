require 'rake/clean'

# 'rake clean'   : rms these files
obj_files = %w(src test).map do |dir|
  %w(o cmi cmx cmo).map {|ext| "#{dir}/*#{ext}" }
end.flatten
CLEAN.include(*obj_files)

# 'rake clobber' : blows *everything* away
CLOBBER.include('test/*_test', 'bin/*')

$c, $x, $g =
  ['ocamlopt -nodynlink -fno-PIC', '.cmxa', '.cmx']

$std_libs = "bigarray#{$x} str#{$x} unix#{$x}"

# 3rd-PARTY LIBS
#
# We're aggressive about including things here.
#   Pros:
#     + Makes build process simpler.
#     + Has only tiny impact on compiled-file sizes.
#   Cons:
#     - Makes running process larger?
#   
$lib_dir = '/usr/local/lib/ocaml/site-lib'
$other_libs = 
  # Not currently needed: equeue, netclient.  For being an HTTP *client*.
  # Netsys: adds some POSIX calls missing from Unix module (necessary for http).
  # Annexlib: for time-related functions.  (format_tm, gm_time, etc.)
  # ExtLib: ExtList provides tail-recursive funcs.
  #   http://ocaml-extlib.googlecode.com/svn/doc/apiref/index.html
  (
   # %w(pcre annexlib netsys netstring mysql http).map do |n|
   %w(pcre annexlib netsys netstring mysql http equeue netclient).map do |n|
     "-I #{$lib_dir}/#{n} #{n}#{$x}"
   end.join(' ') +
   " -cclib /usr/lib64/mysql/libmysqlclient.so    \
     -I #{$lib_dir}/extlib     extLib#{$x}        \
     -I #{$lib_dir}/json-wheel jsonwheel#{$x} "
   ).
  gsub(/\s+/, ' ')

# rules
rule '.cmi' => '.mli' do |t| sh "#{$c} #{$other_libs} -I ./src -c #{t.source}" end
rule $g     => '.ml'  do |t| sh "#{$c} #{$other_libs} -I ./src -c #{t.source}" end

# dependencies
def lib_task(name)
  name = "src/#{name}"
  if File.exists? "#{name}.mli"
    file "#{name}.cmi" => "#{name}.mli"
    file "#{name}#{$g}" => ["#{name}.ml", "#{name}.mli", "#{name}.cmi"]
    task :default => ["#{name}.cmi", "#{name}#{$g}"]
  else
    file "#{name}#{$g}" => ["#{name}.ml"]
    task :default => ["#{name}#{$g}"]
  end
end

def exe_task(target, deps, src=nil)
  src = 'src/' + (src || target)
  my_deps = (['util'] + deps).map {|n| "src/#{n}#{$g}" }
  deps_str = my_deps.join(' ')
  file target => ["#{src}.ml"] + my_deps do
    sh "#{$c} #{$std_libs} #{$other_libs} -I ./src #{deps_str} #{src}.ml -o bin/#{target}"
  end
  task :default => target
end

def test_task(target, deps)
  target = "test/#{target}_test"
  my_deps = (['util'] + deps).map {|n| "src/#{n}#{$g}" }
  deps_str = my_deps.join(' ')
  file target => ["#{target}.ml"] + my_deps do
    sh "#{$c} #{$std_libs} #{$other_libs} -I #{$lib_dir}/oUnit oUnit#{$x} #{deps_str} -I ./src #{target}.ml -o #{target}"
    sh "./#{target}"
  end
  task :default => target
end

lib_task 'ext_pcre'

# ORDER MATTERS
# str_sim levenshtein
daemon_libs = %w(
  diacritics
  prio_queue
  iter_fun 
  mmap
  int_ary ord_int_ary bool_ary char_ary
  int_ary_tbl str_tbl
  logger
  target_idx
  lexicon matrix
  lexer
  entities
  embolden
  tag tag_tbl
  cfg 
  result result_tbl
  str_query_tree id_query_tree
  in_stock
  store_in_stock
  domain
  searcher
  search_fun
  request data_service serial_controller
)


#-- Libs --#
#
# TODO: create dependencies of .cmi files
#

# necessary: set up dependencies
lib_task 'util'
daemon_libs.each {|n| lib_task(n) }

#-- Daemon --#

res_tbl_libs = %w(
  diacritics
  prio_queue
  iter_fun 
  mmap
  int_ary ord_int_ary bool_ary char_ary
  int_ary_tbl str_tbl
  logger
  target_idx
  lexicon matrix
  lexer
  entities
  embolden
  tag
  tag_tbl
  result
  result_tbl)

# exe_task 'multi_tags', res_tbl_libs

#-- deprecated --
# exe_task 'rgs_db', res_tbl_libs
# exe_task 'read_rgs', []

exe_task 'serial_test', daemon_libs
exe_task 'lexicon_test', %w(mmap char_ary iter_fun int_ary ord_int_ary idx_file tbl_mkr int_ary_tbl str_tbl lexicon matrix)
exe_task 'lsd', daemon_libs, 'serial_daemon'

TESTS_P = true
if TESTS_P
  test_task 'util',        %w()
  test_task 'diacritics',  %w(diacritics)
  test_task 'embolden',    %w(diacritics embolden)
  test_task 'int_ary',     %w(iter_fun mmap int_ary ord_int_ary)
  # test_task 'idx_file',    %w(iter_fun mmap int_ary ord_int_ary idx_file)
  #-- require files.  use together w/ idx_file, tbl_mkr. --
  test_task 'bool_ary',    %w(iter_fun mmap int_ary ord_int_ary idx_file bool_ary)
  test_task 'char_ary',    %w(iter_fun mmap int_ary ord_int_ary idx_file char_ary)
  test_task 'tbl_mkr',     %w(iter_fun mmap int_ary ord_int_ary idx_file tbl_mkr)
  test_task 'int_ary_tbl', %w(iter_fun mmap int_ary ord_int_ary idx_file tbl_mkr int_ary_tbl)
  test_task 'str_tbl',     %w(mmap char_ary iter_fun int_ary ord_int_ary idx_file tbl_mkr str_tbl)

  # task 'test/lexicon_test' => ['mk_lexicon_idx']
  # test_task 'lexicon',        %w(mmap char_ary iter_fun int_ary ord_int_ary idx_file tbl_mkr str_tbl int_ary_tbl lexicon)
  test_task 'lexer',          %w(diacritics lexer entities)
  test_task 'str_query_tree', %w(mmap bool_ary iter_fun int_ary ord_int_ary int_ary_tbl char_ary str_tbl lexicon diacritics lexer entities str_query_tree)
  test_task 'str_file_wrtr',  %w(diacritics lexer entities sql series str_file_wrtr)
  test_task 'prio_queue',     %w(prio_queue)
  # test_task 'sql',            %w(sql)
  test_task 'series',         %w(diacritics lexer entities series)

  # test_task 'matrix',     %w(iter_fun mmap int_ary ord_int_ary int_ary_tbl matrix)
  # test_task 'result_tbl', %w(mmap char_ary bool_ary iter_fun int_ary ord_int_ary int_ary_tbl str_tbl target_idx result_tbl)
  # test_task 'result',     %w(mmap char_ary bool_ary iter_fun int_ary ord_int_ary int_ary_tbl str_tbl target_idx result_tbl result)
  # test_task 'ids', daemon_libs
  # test_task 'target_idx', daemon_libs

  # test_task 'str_sim', %w(str_sim)
  test_task 'entities', %w(diacritics lexer entities)
  test_task 'line_info', %w(line_info)
end
