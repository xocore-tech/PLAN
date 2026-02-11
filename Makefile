RPN_GOLD  = $(foreach i,$(shell seq 1 45),gold/$(i)) \
            gold/apply gold/add gold/trunc gold/istype \
            gold/mul

REPL_GOLD  = $(foreach i,$(shell seq 0 4),repl/$(i)) \
             repl/0_0 repl/0_1 repl/0_2 repl/0_3

EXECS = x/plan x/rpn x/boot x/test_mutex x/test_condition_vars x/test_barrier x/test_rwlock x/test_thread

OBJS = planvm-amd64/plan.o \
       planvm-amd64/rbtree_range.o \
       planvm-amd64/printf.o \
       planvm-amd64/boot.o \
       planvm-amd64/rpn.o \
       planvm-amd64/persist.o \
       planvm-amd64/common.o

ASFLAGS = -g

CFLAGS = -std=gnu99 -O0 -g -fno-stack-protector -z noexecstack \
         -msse4.2 \
         -Wall -Werror \
         -fdiagnostics-color=always

################################################################################

all: ${EXECS}

testrepl: ${REPL_GOLD}
testrpn: ${RPN_GOLD}

testexn: x/plan xseed/sire.seed
	x/plan xseed/sire.seed <<< '##Die 3' > .tmpoutput || true
	diff <(grep ERROR <.tmpoutput) <(echo 'ERROR: Die')
	x/plan xseed/sire.seed <<< '##8 (0 ##21 3)' > .tmpoutput
	diff <(grep 21 < .tmpoutput) <(echo '(21 3)')
	x/plan xseed/sire.seed <<< '##8 (0 ##2 6)' > .tmpoutput
	diff <(grep 7 < .tmpoutput) <(echo '(0 7)')
	rm -f .tmpoutput

xseed/wisp.seed: sire/wisp.sire sire/newboot.sire
	plunder save -Z --new-format xseed/wisp.seed wisp

gold/optest: x/plan xseed/wisp.seed wisp/base.lisp wisp/op.lisp
	x/wisp-make wisp > .wisp-input
	x/plan xseed/wisp.seed < .wisp-input 2> .tmpfile
	mv .tmpfile gold/optest
	rm -f .wisp-input

test: x/plan x/rpn testrpn testrepl testexn utest gold/optest

optest:
	rm -f gold/optest
	make gold/optest

utest: x/rpn
	x/rpn test

fuzz:
	x/fuzz-nats 1000

clean:
	rm -f planvm-amd64/*.o ${EXECS} planvm-amd64/planvm-amd64.s planvm-amd64/planvm-amd64data.s planvm-amd64/planvm-amd64.incc.c *.html

.PHONY: clean repatch test fuzz all testrepl testrpn testexn utest optest

################################################################################

planvm-amd64/planvm-amd64.s: doc/planvm-amd64.tex
	x/extract-asm < $^ > $@

planvm-amd64/planvm-amd64data.s: doc/planvm-amd64.tex
	x/extract-asmdata < $^ > $@

planvm-amd64/planvm-amd64.incc.c: doc/planvm-amd64.tex
	x/extract-clang < $^ > $@

planvm-amd64/persist.o: planvm-amd64/planvm-amd64.incc.c

planvm-amd64/plan.o: planvm-amd64/planvm-amd64data.s planvm-amd64/planvm-amd64.s planvm-amd64/constants.s

x/rpn: $(OBJS)
	gcc $(CFLAGS) -nostdlib -o $@ $^ -e rpn_start

x/plan: $(OBJS)
	gcc $(CFLAGS) -nostdlib -o $@ $^ -e plan_start

x/boot: $(OBJS)
	gcc $(CFLAGS) -nostdlib -o $@ $^ -e boot_start

################################################################################

# Q: Why don't these use the -e form above?
# A: Because having a libc with pthread is a hard requirement for performing
#    the testing. I've tried to make this as close to the above builds targets.
#    The main difference is that you can't use -nostdlib, which means that you
#    use the test binary's main() function.

x/test_mutex: $(OBJS) planvm-amd64/test_mutex.o
	gcc $(CFLAGS) -o $@ $^

x/test_condition_vars: $(OBJS) planvm-amd64/test_condition_vars.o
	gcc $(CFLAGS) -o $@ $^

x/test_barrier: $(OBJS) planvm-amd64/test_barrier.o
	gcc $(CFLAGS) -o $@ $^

x/test_rwlock: $(OBJS) planvm-amd64/test_rwlock.o
	gcc $(CFLAGS) -o $@ $^

x/test_thread: $(OBJS) planvm-amd64/test_thread.o
	gcc $(CFLAGS) -nostdlib -o $@ $^


################################################################################

gold/%: tests/% x/rpn
	x/rpn > $@ < $<

repl/%: sire/% x/plan xseed/sire.seed
	x/plan xseed/sire.seed > $@ < $< || true
