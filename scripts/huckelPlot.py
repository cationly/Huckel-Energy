#!/usr/bin/python
''' This is a short Python program to study a varying Huckel (tight binding)
    model hamiltonian using plots - Welcome to huckelPlot!

    When studying the behaviour of molecules, it can often be interesting to
    see how smoothly changing from one molecule to another will affect the 
    behaviour of said molecule, specifically relating to the stationary states
    and allowed energies. This program is used to vary a user-specified
    hamiltonian with (up to two- any more would not be plottable!)
    variable arguments which will be used to change the hamiltonian 
    (and thus the described molecule) pseudo-smoothly.
    E.g. Changing a hydrogen 3-chain to a 3-ring, so the haniltonian goes like:

    0 1 0      0 1 x
    1 0 1  ==> 1 0 1
    0 1 0      x 1 0

    Where x runs from 0 (chain) to 1 (ring). One could also tend x to infinity
    and thus see in this limit that one approaches a hydrogen 2-chain.


    USAGE: Call from the /scripts folder relative to the huckel root directory.
           The output file will be placed in the /hamiltonians directory with a
           user specified (@runtime) name
           
           TODO: add in usage details when it is refined

    OUTPUT: huckelPlot prints its output to a .csv file (user specified @ runtime)
            in a space-separated column format:

            X  [Y]  E1  E2 ... EN
            |   |   \           /
            |   |    Eigenvalues
            |   |
            |   Second hamiltonian parameter (optional)
            |
            First hamiltonian parameter

            This file format is common and easily plottable by, for example, 
            GNUPlot or Open Office Calc (see appropriate documentation for info)
'''

#TODO Import the argument-passing module
#TODO Import the OS-type modules for executing shell commands

#TODO CALL parse command line args ( -debug, -batch .ham file outFile [start/stop/step val])
#TODO CALL parse: Hamiltonian, (var starting/stop/step value) x 2, output filename 
#TODO Put the hamiltonian in an ARRAY with entries = constants or up to 2 different variables

#TODO create the OUTFILE
#TODO create tmp.out
#TODO create hamtemp

# WHILE PARAM 2 [opt] < MAX
    # WHILE PARAM 1 < MAX
       
        #TODO print hamiltonian to hamtemp (overwrite) 
        
        #TODO CALL huckel_energy hamtemp tmp.out N
        #TODO append tmp.out to OUTFILE in format given above ^^^
        
        #TODO increment PARAM 1
        #TODO increment hamiltonian
    # END WHILE
    #TODO increment PARAM 2
    #TODO increment hamiltonian
# END WHILE

#TODO delete tmp.out
#TODO delete hamtemp


#FUNCTION parseargs(dbgFlag,batchFlag,batchParms)
''' This function parses the command line arguments passed to huckelPlot.
    We set 1 boolean flag (dbgFlag) and 1 integer flag (batchFlag). 
    If dbgFlag is set to TRUE then all debugging info gets printed to stdout
    If batchFlag != 0 then the number stored is the number of optional args
    passed to huckelPlot - this uniquely defines which information was passed.

'''
