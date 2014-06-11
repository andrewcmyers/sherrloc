setTestOpts([only_compiler_types(['ghc']),
             omit_ways(['ghci']),
             when(fast(), skip)])

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('hpc_raytrace', \
	[ when(fast(), skip), cmd_prefix(hpc_prefix), reqlib('parsec') ], \
	multimod_compile_and_run, \
	['Main','-fhpc -package parsec'])

