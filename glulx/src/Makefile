# This is a basic Makefile for buildling the Erlang Z-code interpreter
# 
.SUFFIXES: .erl .beam .yrl

CFLAGS = -DDEBUG
#TESTFILE = ../testfiles/risorg.ulx
TESTFILE = ../testfiles/Advent.ulx

.erl.beam:
	erlc ${CFLAGS} -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = glulx_mem glulx_util glulx_vm glulx_decode_instr glulx_instr \
  glulx_main glk glk_win

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:

main: compile
	${ERL} -noshell -s glulx_main main -storyfile ${TESTFILE}

clean:
	rm -rf *.beam erl_crash.dump


