#!/bin/sh
FMHOME='/home/zhdf/APL/Fabric/examples/friendmap'
FABHOME="/home/zhdf/APL/Fabric/";

# first, revert Fabric to the version where all examples compiles
#cd $FABHOME
#svn up -r{20120306}
#ant clobber
#ant

cd $FMHOME
# update to the reversion specified by the command line
svn up -r $@
ant clean
ant
