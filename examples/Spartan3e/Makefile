run::
	ghci -i../..:../../dist/build/autogen/:. Main.hs 


boot::
#	echo main | ghci -i../.. -i../../../kansas-lava Main.hs  -fcontext-stack=100
	echo main | ghci -i../.. -i../../dist/build/autogen  Main.hs  -fcontext-stack=100

copy::


	scp main.vhd andygill@rome:xilinx/test4/main.vhd
	scp ../../../kansas-lava/Prelude/VHDL/Lava.vhd andygill@rome:xilinx/test4/Lava.vhd
