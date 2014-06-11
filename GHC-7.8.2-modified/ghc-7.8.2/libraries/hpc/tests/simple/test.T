setTestOpts([only_compiler_types(['ghc']),
             omit_ways(['ghci']),
             when(fast(), skip)])

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('hpc001', cmd_prefix(hpc_prefix), compile_and_run, ['-fhpc'])

