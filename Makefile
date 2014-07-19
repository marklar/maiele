#
# docs:
#   - ocamldep : http://caml.inria.fr/pub/docs/manual-ocaml/manual027.html
#   - Gnu make : http://www.gnu.org/software/make/manual/
#
# This copy of the makefile is the standard one to actually build the project
# make sure that this copy is the one in Makefile to build correctly for actual use
#

.PHONY: all
all: .depend daemons index test

.PHONY: dist
dist: clean all

# ocamlc -where
STD_LIB_DIR = /usr/local/lib/ocaml
LIB_DIR = $(STD_LIB_DIR)/site-lib

JO_OTHER_LIB_DIRS = \
	-I $(LIB_DIR)/pcre \
	-I $(LIB_DIR)/annexlib \
	-I $(LIB_DIR)/netsys \
	-I $(LIB_DIR)/netstring \
	-I $(LIB_DIR)/mysql \
	-I $(LIB_DIR)/http \
	-I $(LIB_DIR)/extlib \
	-I $(LIB_DIR)/json-wheel \
	-I $(LIB_DIR)/equeue \
	-I $(LIB_DIR)/netclient \
	-I $(LIB_DIR)/oUnit

OTHER_LIB_DIRS = $(JO_OTHER_LIB_DIRS) -I $(STD_LIB_DIR)/threads

CCLIB = -cclib /usr/lib64/mysql/libmysqlclient.so

GLYDE_INCLUDES = -I src -I test
JO_INCLUDES = $(GLYDE_INCLUDES) -I $(LIB_DIR) $(JO_OTHER_LIB_DIRS)     # all relevant -I options here
INCLUDES = $(GLYDE_INCLUDES) -I $(LIB_DIR) $(OTHER_LIB_DIRS)           # all relevant -I options here

# ocaml
OCAMLC   = ocamlc
OCAMLOPT = ocamlopt.opt
OCAMLDEP = jocamldep
# jocaml
JOCAMLC   = jocamlc
JOCAMLOPT = jocamlopt.opt

OCAMLFLAGS = $(INCLUDES) $(CCLIB)                           # add other options for ocamlc here
JOCAMLFLAGS = $(JO_INCLUDES) $(CCLIB)                           # add other options for ocamlc here

INLINE = -inline 10   # default is 1
OCAMLOPTFLAGS = $(INCLUDES) $(CCLIB) -nodynlink -fno-PIC $(INLINE)  # add other options for ocamlopt here
JOCAMLOPTFLAGS = $(JO_INCLUDES) $(CCLIB) -nodynlink -fno-PIC    # add other options for ocamlopt here

OTHER_LIBS = \
	$(LIB_DIR)/pcre/pcre.cmxa \
	$(LIB_DIR)/annexlib/annexlib.cmxa \
	$(LIB_DIR)/netsys/netsys.cmxa \
	$(LIB_DIR)/netstring/netstring.cmxa \
	$(LIB_DIR)/mysql/mysql.cmxa \
	$(LIB_DIR)/http/http.cmxa \
	$(LIB_DIR)/equeue/equeue.cmxa \
	$(LIB_DIR)/netclient/netclient.cmxa \
	$(LIB_DIR)/extlib/extLib.cmxa \
	$(LIB_DIR)/json-wheel/jsonwheel.cmxa \
	$(LIB_DIR)/oUnit/oUnit.cmxa

# unix.cmxa - only for non-Jo.
NO_JO_STD_LIBS = \
	unix.cmxa \
        bigarray.cmxa \
	str.cmxa \
	$(STD_LIB_DIR)/threads/threads.cmxa

JO_STD_LIBS = \
        bigarray.cmxa \
	str.cmxa

DAEMON_OBJS = \
	src/util.cmx \
	src/logger.cmx \
	src/cfg.cmx \
	src/dbd.cmx \
	src/solr.cmx \
	src/diacritics.cmx \
	src/lexer_util.cmx \
	src/variant.cmx \
	src/maiele_lexer.cmx \
	src/doc_lexer.cmx \
	src/mmap.cmx \
	src/int_ary.cmx \
	src/ord_int_ary.cmx \
	src/request.cmx \
	src/show.cmx \
	src/tag.cmx \
	src/conflator.cmx \
	src/embolden.cmx \
	src/str_query_tree.cmx \
	src/bool_ary.cmx \
	src/int_ary_tbl.cmx \
	src/int_ary_idx.cmx \
	src/char_ary.cmx \
	src/str_tbl.cmx \
	src/glu_tbl.cmx\
	src/tag_tbl.cmx \
	src/iter_fun.cmx \
	src/lexicon.cmx \
	src/product_code_tbl.cmx \
	src/matrix.cmx \
	src/query_opts.cmx \
	src/result.cmx \
	src/result_tbl.cmx \
	src/store_in_stock.cmx \
	src/uniquer.cmx \
	src/domain.cmx \
	src/id_query_tree.cmx \
	src/sans_id_query_tree.cmx \
	src/filter_fun.cmx \
	src/result_fetcher.cmx \
	src/product_code_search.cmx \
	src/query_tree.cmx \
	src/query.cmx \
	src/domain_searcher.cmx \
	src/charities_domain.cmx \
	src/searcher.cmx \
	src/data_service.cmx \
	src/in_stock.cmx \
	src/is_sellable.cmx \
	src/event_handlers.cmx \
	src/cookies.cmx \
	src/tag_in_stock.cmx

JO_OBJS = \
	src/jo_util \
	src/jo_minion \
	src/jo_master \
	src/jo_controller \
	src/jo_lsd


## PUT IN A SEPARATE MAKEFILE IN SEPARATE DIRECTORY.
# .PHONY: jo_objs
# jo_objs:
# 	for obj in $(JO_OBJS); do \
# 		if test -f $$obj.mli; then $(JOCAMLC) $(OCAMLFLAGS) -c $$obj.mli; fi; \
# 		$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $$obj.ml; \
# 	done

src/jo_util.cmx: src/jo_util.ml
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $<
src/jo_util.cmi: src/jo_util.mli
	$(JOCAMLC) $(OCAMLFLAGS) -c $<
src/jo_minion.cmx: src/jo_minion.ml
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $<
src/jo_minion.cmi: src/jo_minion.mli
	$(JOCAMLC) $(OCAMLFLAGS) -c $<
src/jo_master.cmx: src/jo_master.ml
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $<
src/jo_master.cmi: src/jo_master.mli
	$(JOCAMLC) $(OCAMLFLAGS) -c $<
src/jo_controller.cmx: src/jo_controller.ml
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $<
src/jo_controller.cmi: src/jo_controller.mli
	$(JOCAMLC) $(OCAMLFLAGS) -c $<
src/jo_lsd.cmx: src/jo_util.ml
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $<
src/jo_denormalize_mtx.cmx: src/jo_denormalize_mtx.ml
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $<

.PHONY: daemons
daemons: bin/lsd bin/lsync bin/search_product_code

SEARCH_PRODUCT_CODE_OBJS = $(OTHER_LIBS) $(DAEMON_OBJS) src/search_product_code.cmx
bin/search_product_code: $(SEARCH_PRODUCT_CODE_OBJS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(NO_JO_STD_LIBS) $(SEARCH_PRODUCT_CODE_OBJS) -o $@

LSD_OBJS = $(OTHER_LIBS) $(DAEMON_OBJS) src/search_fun.cmx src/controller.cmx src/lsd.cmx
bin/lsd: $(LSD_OBJS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(NO_JO_STD_LIBS) $(LSD_OBJS) -o $@

LSYNC_OBJS = $(OTHER_LIBS) $(DAEMON_OBJS) src/lsync.cmx
bin/lsync: $(LSYNC_OBJS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(NO_JO_STD_LIBS) $(LSYNC_OBJS) -o $@

LSYNC_OBJS = $(OTHER_LIBS) $(DAEMON_OBJS) src/lsync.cmx
bin/lsync: $(LSYNC_OBJS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(NO_JO_STD_LIBS) $(LSYNC_OBJS) -o $@

# src/conflator.cmx
IDX_SPECIFIC_OBJS = \
	src/idx_file.cmx \
	src/tbl_mkr.cmx \
	src/result_tbl_mkr.cmx \
	src/matrix_mkr.cmx \
	src/faux_result_tbl_mkr.cmx \
	src/line_info.cmx \
	src/entities.cmx \
	src/series.cmx \
	src/sql.cmx \
	src/prio_queue.cmx

RESULT_TBL_EXES = \
	bin/write_str_file \
	bin/read_str_file \
	bin/fix_pops \
	bin/create_faux \
	bin/make_pop_idx \
	bin/make_target_idx \
	bin/tag_tbl_mkr \
	bin/glu_tbl_mkr \
	bin/storefront_mkr \
	bin/dump_product_codes \
	bin/mk_product_codes_tbl \
	bin/mk_glu_2_codes

# Also: bin/mk_supers_idx.  For optimizing corner-case queries.
BUILD_IDX_EXES = \
	bin/dump_postings \
	bin/postings_file_rdr \
	bin/mk_lexicon_idx

# EXCEPT jo_denormalize_mtx, which needs to be built differently.
EMBIGGEN_MTX_EXES = \
	bin/partition_mtx \
	bin/concat_mtxs \
	bin/add_pop_postings

NO_JO_IDX_EXES = $(RESULT_TBL_EXES) $(BUILD_IDX_EXES) $(EMBIGGEN_MTX_EXES)
.PHONY: index
index: $(NO_JO_IDX_EXES) bin/jo_denormalize_mtx

ALL_IDX_OBJS = $(OTHER_LIBS) $(DAEMON_OBJS) $(IDX_SPECIFIC_OBJS)

bin/jo_denormalize_mtx: $(ALL_IDX_OBJS) src/jo_denormalize_mtx.cmx
	$(JOCAMLOPT) $(JOCAMLOPTFLAGS) $(JO_STD_LIBS) $(ALL_IDX_OBJS) src/jo_denormalize_mtx.ml -o $@

# SOMETHING MORE GENERIC HERE?

IDX_BUILD = $(OCAMLOPT) $(OCAMLOPTFLAGS) $(NO_JO_STD_LIBS) $(ALL_IDX_OBJS)

bin/write_str_file: $(ALL_IDX_OBJS) src/write_str_file.cmx
	$(IDX_BUILD) src/write_str_file.cmx -o $@
bin/read_str_file: $(ALL_IDX_OBJS) src/read_str_file.cmx
	$(IDX_BUILD) src/read_str_file.cmx -o $@
bin/fix_pops: $(ALL_IDX_OBJS) src/fix_pops.cmx
	$(IDX_BUILD) src/fix_pops.cmx -o $@
bin/create_faux: $(ALL_IDX_OBJS) src/create_faux.cmx
	$(IDX_BUILD) src/create_faux.cmx -o $@
bin/make_pop_idx: $(ALL_IDX_OBJS) src/make_pop_idx.cmx
	$(IDX_BUILD) src/make_pop_idx.cmx -o $@
bin/make_target_idx: $(ALL_IDX_OBJS) src/make_target_idx.cmx
	$(IDX_BUILD) src/make_target_idx.cmx -o $@
bin/tag_tbl_mkr: $(ALL_IDX_OBJS) src/tag_tbl_mkr.cmx
	$(IDX_BUILD) src/tag_tbl_mkr.cmx -o $@
bin/glu_tbl_mkr: $(ALL_IDX_OBJS) src/glu_tbl_mkr.cmx
	$(IDX_BUILD) src/glu_tbl_mkr.cmx -o $@
bin/storefront_mkr: $(ALL_IDX_OBJS) src/storefront_mkr.cmx
	$(IDX_BUILD) src/storefront_mkr.cmx -o $@
bin/dump_postings: $(ALL_IDX_OBJS) src/dump_postings.cmx
	$(IDX_BUILD) src/dump_postings.cmx -o $@
bin/postings_file_rdr: $(ALL_IDX_OBJS) src/postings_file_rdr.cmx
	$(IDX_BUILD) src/postings_file_rdr.cmx -o $@
bin/mk_lexicon_idx: $(ALL_IDX_OBJS) src/mk_lexicon_idx.cmx
	$(IDX_BUILD) src/mk_lexicon_idx.cmx -o $@
bin/partition_mtx: $(ALL_IDX_OBJS) src/partition_mtx.cmx
	$(IDX_BUILD) src/partition_mtx.cmx -o $@
bin/concat_mtxs: $(ALL_IDX_OBJS) src/concat_mtxs.cmx
	$(IDX_BUILD) src/concat_mtxs.cmx -o $@
bin/add_pop_postings: $(ALL_IDX_OBJS) src/add_pop_postings.cmx
	$(IDX_BUILD) src/add_pop_postings.cmx -o $@

bin/dump_product_codes: $(ALL_IDX_OBJS) src/dump_product_codes.cmx
	$(IDX_BUILD) src/dump_product_codes.cmx -o $@

bin/mk_product_codes_tbl: $(ALL_IDX_OBJS) src/mk_product_codes_tbl.cmx
	$(IDX_BUILD) src/mk_product_codes_tbl.cmx -o $@
bin/mk_glu_2_codes: $(ALL_IDX_OBJS) src/mk_glu_2_codes.cmx
	$(IDX_BUILD) src/mk_glu_2_codes.cmx -o $@


# Tests

.PHONY: build_tests
build_tests: \
	test/test_helper.cmx \
	test/util_test \
	test/diacritics_test \
	test/embolden_test \
	test/int_ary_test \
	test/bool_ary_test \
	test/char_ary_test \
	test/tbl_mkr_test \
	test/int_ary_tbl_test \
	test/str_tbl_test \
	test/lexicon_test \
	test/lexer_util_test \
	test/variant_test \
	test/lexer_test \
	test/doc_lexer_test \
	test/str_query_tree_test \
	test/write_str_file_test \
	test/prio_queue_test \
	test/series_test \
	test/str_sim_test \
	test/entities_test \
	test/line_info_test \
	test/query_test

.PHONY: setup_tests
setup_tests:
	script/yaml_2_json.rb cfg/config.yml
	mkdir -p tmp

.PHONY: test
test: build_tests setup_tests
	test/util_test
	test/diacritics_test
	test/embolden_test
	test/int_ary_test
	test/bool_ary_test
	test/char_ary_test
	test/tbl_mkr_test
	test/int_ary_tbl_test
	test/str_tbl_test
	test/lexer_util_test
	test/lexer_test
	test/variant_test
	test/doc_lexer_test
	test/str_query_tree_test
	test/write_str_file_test
	test/prio_queue_test
	test/series_test
	test/str_sim_test
	test/entities_test
	test/line_info_test
	test/query_test
	rm -rf tmp

TEST_BUILD = $(OCAMLOPT) $(OCAMLOPTFLAGS) $(NO_JO_STD_LIBS) $(OTHER_LIBS) $(DAEMON_OBJS) test/test_helper.cmx

# SIMPLE_TESTS = \
# 	test/util_test \
# 	test/diacritics_test \
# 	test/embolden_test \
# 	test/int_ary_test
# simple_tests:
# 	for test in $(SIMPLE_TESTS); do \
# 		$(DAEMON_OBJS) $$test.; \
# 		( $(JOCAMLOPT) $(JOCAMLOPTFLAGS) -c $$obj.ml || echo 'skipping' ); \
# 	done

test/util_test: $(DAEMON_OBJS) test/util_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/diacritics_test: $(DAEMON_OBJS) test/diacritics_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/embolden_test: $(DAEMON_OBJS) test/embolden_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/int_ary_test: $(DAEMON_OBJS) test/int_ary_test.cmx
	$(TEST_BUILD) $@.cmx -o $@

test/bool_ary_test: $(DAEMON_OBJS) src/idx_file.cmx test/bool_ary_test.cmx
	$(TEST_BUILD) src/idx_file.cmx $@.cmx -o $@
test/char_ary_test: $(DAEMON_OBJS) src/idx_file.cmx test/char_ary_test.cmx
	$(TEST_BUILD) src/idx_file.cmx $@.cmx -o $@
test/tbl_mkr_test: $(DAEMON_OBJS) src/idx_file.cmx src/tbl_mkr.cmx test/tbl_mkr_test.cmx
	$(TEST_BUILD) src/idx_file.cmx src/tbl_mkr.cmx $@.cmx -o $@
test/int_ary_tbl_test: $(DAEMON_OBJS) src/idx_file.cmx src/tbl_mkr.cmx test/int_ary_tbl_test.cmx
	$(TEST_BUILD) src/idx_file.cmx src/tbl_mkr.cmx $@.cmx -o $@
test/str_tbl_test: $(DAEMON_OBJS) src/idx_file.cmx src/tbl_mkr.cmx test/str_tbl_test.cmx
	$(TEST_BUILD) src/idx_file.cmx src/tbl_mkr.cmx $@.cmx -o $@
test/lexicon_test: $(DAEMON_OBJS) src/idx_file.cmx src/tbl_mkr.cmx test/lexicon_test.cmx bin/mk_lexicon_idx
	$(TEST_BUILD) src/idx_file.cmx src/tbl_mkr.cmx $@.cmx -o $@

test/lexer_util_test: $(DAEMON_OBJS) test/lexer_util_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/lexer_test: $(DAEMON_OBJS) test/lexer_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/variant_test: $(DAEMON_OBJS) test/variant_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/doc_lexer_test: $(DAEMON_OBJS) test/doc_lexer_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/str_query_tree_test: $(DAEMON_OBJS) test/str_query_tree_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/query_test: $(DAEMON_OBJS) test/query_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/write_str_file_test: $(DAEMON_OBJS) test/write_str_file_test.cmx
	$(TEST_BUILD) $@.cmx -o $@
test/prio_queue_test: $(DAEMON_OBJS) src/prio_queue.cmx test/prio_queue_test.cmx
	$(TEST_BUILD) src/prio_queue.cmx $@.cmx -o $@
test/series_test: $(DAEMON_OBJS) $(IDX_SPECIFIC_OBJS) test/series_test.cmx
	$(TEST_BUILD) $(IDX_SPECIFIC_OBJS) $@.cmx -o $@
test/str_sim_test: src/str_sim.cmx test/str_sim_test.cmx
	$(TEST_BUILD) src/str_sim.cmx $@.cmx -o $@
test/entities_test: $(IDX_SPECIFIC_OBJS) test/entities_test.cmx
	$(TEST_BUILD) src/entities.cmx $@.cmx -o $@
test/line_info_test: $(IDX_SPECIFIC_OBJS) test/line_info_test.cmx
	$(TEST_BUILD) src/line_info.cmx $@.cmx -o $@

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.PHONY: clean
clean:
	rm -f .depend
	rm -rf tmp                      # testing temp dir
	rm -f bin/* test/*_test         # executables
	rm -f test/*.cm[iox] test/*.o   # test obj files
	rm -f src/*.cm[iox] src/*.o     # src obj files

.depend:
	$(OCAMLDEP) $(INCLUDES) -native src/*.mli src/*.ml test/*.ml > $@

include .depend
