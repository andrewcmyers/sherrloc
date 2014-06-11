setTestOpts([only_compiler_types(['ghc']),
             omit_ways(['ghci','threaded2']),
             when(fast(), skip)])

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('hpc_fork', [ when(opsys('mingw32'), skip), # no forkProcess on Windows
                   cmd_prefix(hpc_prefix) ], compile_and_run, ['-fhpc'])

