# This is a basic Makefile for buildling the Erlang Z-code interpreter
# 
.SUFFIXES: .erl .beam .yrl

CFLAGS = -DDEBUG

.erl.beam:
	erlc ${CFLAGS} -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = glulx_mem glulx_vm glulx_decode_instr glulx_instr glulx_main

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:

main: compile
	${ERL} -noshell -s glulx_main main -storyfile ../testfiles/risorg.ulx

clean:
	rm -rf *.beam erl_crash.dump


