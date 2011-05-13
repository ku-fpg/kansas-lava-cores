DIR := sims

test:
	../dist/build/kansas-lava-cores-tests/kansas-lava-cores-tests

simulate:
	$(DIR)/runsims

report:
	./KansasLava/dist/build/kansas-lava-testreport/kansas-lava-testreport $(DIR)

clean:
	mv sims sims.X
	rm -Rf sims.X

init:
# 	Create a symbolic link to the Prelude directory
	ln -s ../../kansas-lava KansasLava
