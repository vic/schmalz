# This is a basic Makefile for buildling the Erlang Z-code interpreter
# 
.SUFFIXES: .erl .beam .yrl

CFLAGS = #-DDEBUG

.erl.beam:
	erlc ${CFLAGS} -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = util memory encoding dictionary objects \
  instruction_info decode_instr instruction \
  machine streams screen main

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:

main: compile
	${ERL} -noshell -s main main -storyfile ../testfiles/minizork.z3

clean:
	rm -rf *.beam erl_crash.dump


