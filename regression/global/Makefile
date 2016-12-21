TESTS=test001 test002 test003

.PHONY: check $(TESTS) 

check: $(TESTS) 

$(TESTS): %: %.expr
	cat $@.input | ../../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../../rc.native -s $< > $@.log && diff $@.log orig/$@.log
#	RC_RUNTIME=../runtime ../rc.native -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
