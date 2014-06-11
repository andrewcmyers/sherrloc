setTestOpts([only_compiler_types(['ghc']),
             omit_ways(['ghci']),
             when(fast(), skip)])

hpcsample = 'hpc_sample'

test('hpc_report_multi_001', normal, run_command, \
	["'" + config.hpc + "' report " + hpcsample + " --include=Geometry --per-module"])
test('hpc_report_multi_002', normal, run_command, \
	["'" + config.hpc + "' report " + hpcsample + " --exclude=Geometry --per-module"])
test('hpc_report_multi_003', normal, run_command, \
	["'" + config.hpc + "' report " + hpcsample + " --include=Geometry --per-module --decl-list"])

test('hpc_markup_multi_001', normal, run_command, \
        ["'" + config.hpc + "' markup --srcdir=.. --hpcdir=tixs/.hpc " + hpcsample + " --include=Geometry"])
test('hpc_markup_multi_002', normal, run_command, \
	["'" + config.hpc + "' markup --srcdir=.. --hpcdir=tixs/.hpc " + hpcsample + " --exclude=Geometry"])
test('hpc_markup_multi_003', normal, run_command, \
	["'" + config.hpc + "' markup --srcdir=.. --hpcdir=tixs/.hpc " + hpcsample + " --fun-entry-count"])

test('hpc_show_multi_001', normal, run_command,
     ["'" + config.hpc + "' show " + hpcsample])
test('hpc_show_multi_002', normal, run_command,
     ["'" + config.hpc + "' show " + hpcsample + " --include=Geometry"])

# XXX We used to do clean_o_hi(), but that doesn't work any more
# Fix cleaning properly

