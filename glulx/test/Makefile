# This is a basic Makefile for buildling the Erlang Z-code interpreter
# 
.SUFFIXES: .erl .beam .yrl

CFLAGS = #-DDEBUG

.erl.beam:
	erlc ${CFLAGS} -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean -pa ../src

MODS = glulx_mem_test

all: compile

compile: ${MODS:%=%.beam}

test: compile
	${ERL} -noshell -s glulx_mem_test test_all

clean:
	rm -rf *.beam erl_crash.dump


