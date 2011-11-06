DIR := sims
KANSAS_LAVA_ROOT := ../../kansas-lava
RUN := env KANSAS_LAVA_ROOT=$(KANSAS_LAVA_ROOT)
# default to 2-core
N := 2

test:
	$(RUN) ../dist/build/kansas-lava-cores-tests/kansas-lava-cores-tests +RTS -N$(N) -RTS $(ARGS)

simulate:
	$(DIR)/runsims

report:
	$(RUN) $(KANSAS_LAVA_ROOT)/dist/build/kansas-lava-testreport/kansas-lava-testreport $(DIR)

clean:
	mv sims sims.X
	rm -Rf sims.X

init:
# 	Create a symbolic link to the Prelude directory
	ln -s ../../kansas-lava KansasLava
