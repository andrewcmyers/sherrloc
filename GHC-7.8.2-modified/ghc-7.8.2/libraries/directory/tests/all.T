test('currentDirectory001',     normal, compile_and_run, [''])
test('directory001',            normal, compile_and_run, [''])
test('doesDirectoryExist001',   normal, compile_and_run, [''])

# This test is a bit bogus.  Disable for GHCi.
test('getDirContents001', omit_ways(['ghci']), compile_and_run, ['-fno-gen-manifest'])

test('getDirContents002', [ normalise_exe, exit_code(1) ],
                          compile_and_run, [''])

# Depends on binary from previous run, which gets removed by the driver way=ghci
test('getPermissions001', omit_ways(['ghci']), compile_and_run, ['-cpp'])

test('copyFile001', extra_clean(['copyFile001dir/target']),
                    compile_and_run, [''])
test('copyFile002', extra_clean(['copyFile002dir/target']),
                    compile_and_run, [''])

test('renameFile001', extra_clean(['renameFile001.tmp1','renameFile001.tmp2']),
      compile_and_run, [''])

test('createDirectory001',  normal, compile_and_run, [''])

test('createDirectoryIfMissing001',  normal, compile_and_run, [''])

# No sane way to tell whether the output is reasonable here...
test('getHomeDirectory001',  ignore_output, compile_and_run, [''])

test('T4113', when(platform('i386-apple-darwin'), expect_broken(7604)), compile_and_run, [''])
