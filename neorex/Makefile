TESTS=bloc expo strip ifix itrail nest node qfmt quip simple slug trad twrap

INPUTS=$(foreach t,$(TESTS),ex/$(t).rex)

LEXED=$(foreach t,$(TESTS),hilite/$(t).rex)

GOLD=$(foreach t,$(TESTS),gold/$(t).rex)

test: o.rex ${GOLD} ${LEXED}

all: gold

clean:
	rm -f o.rex

gold/%.rex: ex/%.rex o.rex
	./o.rex parse --no-color < $< > $@

hilite/%.rex: ex/%.rex o.rex
	./o.rex quip < $< > $@

o.rex: rex.c
	gcc rex.c -Wall -Werror -O1 -g -o o.rex
