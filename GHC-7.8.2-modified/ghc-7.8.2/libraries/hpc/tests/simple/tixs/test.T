setTestOpts([only_compiler_types(['ghc']),
             omit_ways(['ghci']),
             when(fast(), skip)])

hpcsample = 'hpc_sample'

test('hpc_help', normal, run_command, ["'" + config.hpc + "' help"])
test('hpc_help_help', normal, run_command, ["'" + config.hpc + "' help help"])

test('hpc_help_report', normal, run_command, ["'" + config.hpc + "' help report"])
test('hpc_report_001', normal, run_command, \
	["'" + config.hpc + "' report " + hpcsample + ".tix --xml-output"])
test('hpc_report_002', normal, run_command, \
	["'" + config.hpc + "' report " + hpcsample + ".tix --xml-output --per-module"])
test('hpc_report_003', normal, run_command, \
	["'" + config.hpc + "' report " + hpcsample + ".tix --per-module --decl-list"])
test('hpc_report_error_001', exit_code(1), run_command, ["'" + config.hpc + "' report"]) # no .tix file
test('hpc_report_error_002', exit_code(1), run_command, ["'" + config.hpc + "' report hpc001.hs"]) # bad .tix file

test('hpc_help_markup', normal, run_command, ["'" + config.hpc + "' help markup"])
test('hpc_markup_error_001', exit_code(1), run_command, ["'" + config.hpc + "' markup"]) # no .tix file
test('hpc_markup_error_002', exit_code(1), run_command, ["'" + config.hpc + "' markup hpc001.hs"]) # bad .tix file
test('hpc_markup_001', normal, run_command, \
	['(' + "'" + config.hpc + "' markup " + hpcsample + ".tix --highlight-covered ; cat Main.hs.html )"])
test('hpc_markup_002', normal, run_command, \
	['(' + "'" + config.hpc + "' markup " + hpcsample + ".tix --fun-entry-count ; cat Main.hs.html )"])

test('hpc_help_show', normal, run_command, ["'" + config.hpc + "' help show"])
test('hpc_show', normal, run_command, ["'" + config.hpc + "' show " + hpcsample + ".tix"])
test('hpc_show_error_001', exit_code(1), run_command, ["'" + config.hpc + "' show"]) # no .tix file
test('hpc_show_error_002', exit_code(1), run_command, ["'" + config.hpc + "' show hpc001.hs"]) # bad .tix file

test('hpc_help_version', normal, run_command, ["'" + config.hpc + "' help version"])
test('hpc_version', normal, run_command, ["'" + config.hpc + "' version"])

test('hpc_help_draft', normal, run_command, ["'" + config.hpc + "' help draft"])
test('hpc_draft', normal, run_command, ["'" + config.hpc + "' draft " + hpcsample + ".tix"])

test('hpc_help_overlay', normal, run_command, ["'" + config.hpc + "' help overlay"])

test('hpc_overlay', [ extra_clean(['sample_overlay1.tix', 'total1.tix']) ],
                     run_command,
                       [     "'" + config.hpc + "' overlay sample_overlay.ovr > sample_overlay1.tix" +
                        ";  cat sample_overlay1.tix " +
                        ";'" + config.hpc + "' combine " + hpcsample + ".tix sample_overlay1.tix > total1.tix" +
                        ";'" + config.hpc + "' report total1.tix"]);

test('hpc_overlay2', [ extra_clean(['sample_overlay2.tix', 'total2.tix']) ],
                     run_command,
                        [     "'" + config.hpc + "' overlay sample_overlay.ovr --output=sample_overlay2.tix" +
                        ";  cat sample_overlay2.tix " +
                        ";'" + config.hpc + "' combine " + hpcsample + ".tix sample_overlay2.tix --output=total2.tix" +
                        ";'" + config.hpc + "' report total2.tix"]);

test('hpc_hand_overlay', [ extra_clean(['sample_overlay3.tix', 'total3.tix']) ],
                         run_command,
                        [     "'" + config.hpc + "' overlay hand_overlay.ovr --output=sample_overlay3.tix" +
                        ";  cat sample_overlay3.tix " +
                        ";'" + config.hpc + "' combine " + hpcsample + ".tix sample_overlay3.tix --output=total3.tix" +
                        ";'" + config.hpc + "' report total3.tix"]);

test('hpc_bad_001', exit_code(1), run_command, ["'" + config.hpc + "' bad arguments"])
