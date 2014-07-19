
other_libs = 
  [ 'pcre',
    'annexlib',    # time-related functions (format_tm, gm_time, etc.)
    'netsys',      # adds POSIX calls missing from Unix module (necessary for http)
    'netstring',
    'mysql',
    'http',
    #-- for HTTP *client* --
    'equeue',
    'netclient'
  ]

# ExtLib: ExtList provides tail-recursive funcs.
#   http://ocaml-extlib.googlecode.com/svn/doc/apiref/index.html

$lib_dir = '/usr/local/lib/ocaml/site-lib'

$other_libs_str =
  ( ' -cclib /usr/lib64/mysql/libmysqlclient.so ' +   # C library.
    other_libs.map {|n| "-I #{$lib_dir}/#{n} #{n}.cmxa" }.join(' ') +
      " -I #{$lib_dir}/extlib     extLib.cmxa      \
        -I #{$lib_dir}/json-wheel jsonwheel.cmxa "
    ).gsub(/\s+/, ' ')
