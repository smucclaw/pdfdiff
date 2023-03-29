#!/bin/bash
# from https://stackoverflow.com/questions/1075708/diff-utility-works-for-2-files-how-to-compare-more-than-2-files-at-a-time


SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )";
BASEDIR="${SCRIPTDIR%/*}"
OUTDIR="${BASEDIR}/out/diff2subsets"

mkdir -p ${OUTDIR}

ORGFILES=($(ls $BASEDIR/FTA/*.org))               # Array of files to compare
max=${#ORGFILES[@]}                               # Take the length of that array
for ((idxA=0; idxA<max; idxA++)); do              # iterate idxA from 0 to length
  for ((idxB=idxA + 1; idxB<max; idxB++)); do     # iterate idxB + 1 from idxA to length
    filenameA=$(echo ${ORGFILES[$idxA]} | sed -r "s/.+\/(.+)\..+/\1/")
    filenameB=$(echo ${ORGFILES[$idxB]} | sed -r "s/.+\/(.+)\..+/\1/")
    outfile="${OUTDIR}/${filenameA}_vs_${filenameB}.diff"
    
    echo "saving diff to ${outfile}"
    diff ${ORGFILES[$idxA]} ${ORGFILES[$idxB]} > ${outfile} # Do whatever you're here for.
  done
done