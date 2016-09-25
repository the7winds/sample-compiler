TESTS=test001 test002 test003 test004 test005 test006 test007 test008 test009

.PHONY: check $(TESTS)
check: $(TESTS)

$(TESTS): %: %.expr %.input orig/%.log
	../src/rc.byte $@.expr && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm *.log *.s *~
