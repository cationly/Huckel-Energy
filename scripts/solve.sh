#!/bin/bash
# pass in directories to scan and the script will go into 
# each and get the eigenvalues of all the hamiltonians and put
# them into 1 file called ${DIRNAME}_out with 1 line per
# eigenvalue set


# parse command line args
i=0
while [ $# -gt 0 ]
do
    hamDirs[$i]=$1
    echo ${hamDirs[$i]}
    if [ ! -d ${hamDirs[$i]} ]; then
        echo "Error: directory ${hamDirs[$i]} does not exists"
        echo 'EXITING'
        exit 1
    fi
    shift
    i=$i+1
done

for hamSet in ${hamDirs[@]}
do
    cd ~/work/employment/UROP/huckel/hamiltonians
    cd $hamSet

    for hamiltonian in `ls | grep -v _out` #No "outFile"
    do
       ../../src/huckel_energy $hamiltonian ${hamiltonian}_out N
    done
    
    if [ -f _out ]; then
        rm -f _out
    fi
    touch _out
    
    for outFile in `ls | grep _out`
    do
        if [ ! "$outFile" == "_out" ]; then 
            cat $outFile >> ${hamset}_out
            rm $outFile
        fi
    done
done
exit 0
