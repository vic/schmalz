all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

#TESTFILE = testfiles/amfv.z4
#TESTFILE = testfiles/minizork.z3
TESTFILE = testfiles/curses.z5
ERL = erl -boot start_clean

main:
	${ERL} -pa ebin -noshell -s main main -storyfile ${TESTFILE}

