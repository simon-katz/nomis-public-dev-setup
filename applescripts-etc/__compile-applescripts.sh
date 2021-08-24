#!/bin/bash -e


for x in *
do
  if [ -f "$x" ] ; then
    ext=${x##*.}
    if [ $ext = "applescript" ] ; then
        echo "==== $x ===="
        basename=`basename $x .applescript`
        osacompile -o "$basename.scpt" "$x"
    fi
  fi
done

# bells
