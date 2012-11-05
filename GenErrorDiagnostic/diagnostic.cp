#!/bin/sh

dir=`dirname "$0"`

java=java

classpath="$dir/build:$dir/lib/commons-cli-1.2.jar:$dir/lib/java-cup-11a.jar:$dir/lib/JFlex.jar"

eval "$java" -classpath "'$classpath'" "diagnostic.Analysis $@"
