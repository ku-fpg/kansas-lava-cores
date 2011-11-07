DIR := sims
# default to 2-core
N := 2

test:
	../dist/build/kansas-lava-cores-tests/kansas-lava-cores-tests +RTS -N$(N) -RTS $(ARGS)

simulate:
	$(DIR)/runsims

report:
	kansas-lava-testreport $(DIR)

clean:
	mv sims sims.X
	rm -Rf sims.X

init:
# 	Create a symbolic link to the Prelude directory
	ln -s ../../kansas-lava KansasLava
