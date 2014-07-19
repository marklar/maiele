#!/usr/bin/env ruby

# This file simply outputs 2 commands that can be used to run a mostly complete
# toplevel of the Maiele environment. Simply enter them on the command line
#
# The commented objs towards the end can not be added in there current state as
# toplevel will try to run them and Sys.args will not contain valid values.
# They must be run piece by piece in the top level.

begin

  STD_LIB_DIR = "/usr/local/lib/ocaml"
  LIB_DIR = "#{STD_LIB_DIR}/site-lib"

  OTHER_LIB_DIRS =
    "-I #{LIB_DIR}/stublibs " +
    "-I #{LIB_DIR}/pcre " +
    "-I #{LIB_DIR}/netsys " +
    "-I #{LIB_DIR}/netstring " +
    "-I #{LIB_DIR}/mysql " +
    "-I #{LIB_DIR}/http " +
    "-I #{LIB_DIR}/extlib " +
    "-I #{LIB_DIR}/json-wheel " +
    "-I #{LIB_DIR}/equeue " +
    "-I #{LIB_DIR}/netclient " +
    "-I #{LIB_DIR}/oUnit " +
    "-I #{STD_LIB_DIR}/threads"

  GLYDE_INCLUDES = "-I src -I lib/time" # The lib include is for the special Time module to replace annexlib in toplevel
  INCLUDES = "#{GLYDE_INCLUDES} -I #{LIB_DIR} #{OTHER_LIB_DIRS}"

  CCLIB = "-cclib /usr/lib64/mysql/libmysqlclient.so"

  OTHER_LIBS =
    "#{LIB_DIR}/pcre/pcre.cma " +
    "#{LIB_DIR}/netsys/netsys.cma " +
    "#{LIB_DIR}/netstring/netstring.cma " +
    "#{LIB_DIR}/mysql/mysql.cma " +
    "#{LIB_DIR}/http/http.cma " +
    "#{LIB_DIR}/equeue/equeue.cma " +
    "#{LIB_DIR}/netclient/netclient.cma " +
    "#{LIB_DIR}/extlib/extLib.cma " +
    "#{LIB_DIR}/json-wheel/jsonwheel.cma " +
    "#{LIB_DIR}/oUnit/oUnit.cma"

  DAEMON_OBJS =
    "util.cmo " +
    "logger.cmo " +
    "cfg.cmo " +
    "dbd.cmo " +
    "solr.cmo " +
    "diacritics.cmo " +
    "lexer_util.cmo " +
    "variant.cmo " +
    "maiele_lexer.cmo " +
    "doc_lexer.cmo " +
    "mmap.cmo " +
    "iter_fun.cmo " +
    "int_ary.cmo " +
    "ord_int_ary.cmo " +
    "request.cmo " +
    "show.cmo " +
    "tag.cmo " +
    "conflator.cmo " +
    "embolden.cmo " +
    "str_query_tree.cmo " +
    "bool_ary.cmo " +
    "int_ary_tbl.cmo " +
    "int_ary_idx.cmo " +
    "char_ary.cmo " +
    "str_tbl.cmo " +
    "glu_tbl.cmo " +
    "tag_tbl.cmo " +
    "lexicon.cmo " +
    "product_code_tbl.cmo " +
    "matrix.cmo " +
    "query_opts.cmo " +
    "result.cmo " +
    "result_tbl.cmo " +
    "store_in_stock.cmo " +
    "uniquer.cmo " +
    "domain.cmo " +
    "id_query_tree.cmo " +
    "sans_id_query_tree.cmo " +
    "filter_fun.cmo " +
    "result_fetcher.cmo " +
    "product_code_search.cmo " +
    "query_tree.cmo " +
    "query.cmo " +
    "domain_searcher.cmo " +
    "charities_domain.cmo " +
    "searcher.cmo " +
    "data_service.cmo " +
    "in_stock.cmo " +
    "is_sellable.cmo " +
    "event_handlers.cmo " +
    "cookies.cmo " +
    "tag_in_stock.cmo "

  IDX_SPECIFIC_OBJS = "" +
    "idx_file.cmo " +
    "tbl_mkr.cmo " +
    "result_tbl_mkr.cmo " +
    "matrix_mkr.cmo " +
    "faux_result_tbl_mkr.cmo " +
    "line_info.cmo " +
    "entities.cmo " +
    "series.cmo " +
    "sql.cmo " #+
    "prio_queue.cmo "

  RESULT_TBL_OBJS = "" +
    "write_str_file.cmo " #+
    #"read_str_file.cmo " +
    #"fix_pops.cmo " +
    #"create_faux.cmo " +
    #"make_pop_idx.cmo " +
    #"make_target_idx.cmo " +
    #"tag_tbl_mkr.cmo " +
    #"glu_tbl_mkr.cmo " +
    #"storefront_mkr.cmo " +
    #"dump_product_codes.cmo " +
    #"mk_product_codes_tbl.cmo " +
    #"mk_glu_2_codes.cmo "

  BUILD_IDX_OBJS =  "" #+
    #"dump_postings.cmo " +
    #"postings_file_rdr.cmo " +
    #"mk_lexicon_idx.cmo "

  EMBIGGEN_MTX_OBJS = "" #+
    #"partition_mtx.cmo " +
    #"concat_mtxs.cmo " +
    #"add_pop_postings.cmo "

  MAIELE_OBJS = DAEMON_OBJS + IDX_SPECIFIC_OBJS + RESULT_TBL_OBJS + BUILD_IDX_OBJS + EMBIGGEN_MTX_OBJS

  NO_JO_STD_LIBS =
  "time.cma " + # Special lib to replace annexlib when building a top level
	"unix.cma " +
  "bigarray.cma " +
  "str.cma " +
  "#{STD_LIB_DIR}/threads/threads.cma "

  puts "ocamlmktop -o top #{CCLIB} -custom -ccopt -Wl,--export-dynamic"
  puts "./top #{INCLUDES} #{NO_JO_STD_LIBS} #{OTHER_LIBS} #{MAIELE_OBJS}"

end




























