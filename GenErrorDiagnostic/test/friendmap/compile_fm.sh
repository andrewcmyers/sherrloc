#!/bin/sh
FMHOME='/home/zhdf/APL/Fabric/examples/friendmap'
cd $FMHOME
# update to the reversion specified by the command line
svn up -r $@
ant clean
ant
