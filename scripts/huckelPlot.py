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

    OUTPUT: huckelPlot prints its output to a .csv file
            (user specified @ runtime) in a space-separated column format:

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

# Import the OS-type modules for executing shell commands
import os
import sys
from os import system
from os import path
from sys import exit

#TODO CALL parse command line args 
    # ( -debug, -infile .ham file -outfile outFile -step [start/stop/step val])
debugFlag,outFile,hamiltonian,tokens,start,stop,step = parseArgs()

#TODO match the hamiltonian elements corresponding to TOKENS to variables
#     would really like pointers to point to PARAMETERS...
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


#FUNCTION parseargs()
def parseArgs()
    ''' 
        This function parses the command line arguments passed to huckelPlot.
        We set 1 boolean flag (dbgFlag). If dbgFlag is set to TRUE then all
        debugging info gets printed to stdout.

        RETURNS: tuple containing: 
             
             (debugFlag,outFile,hamiltonian[][],tokens[],start[],stop[],step[]) 
    '''
    #TODO import getopts module

    #TODO parse commandline and set the debugFlag
    #TODO request output filename (append .csv)
    #TODO request the token definitions (i.e. what letters/strings will be used)
    #TODO input hamiltonian (make sure to echo in a nice format & if any 
    #TODO check that hamiltonian is: symmetric, only contains numbers or tokens
    #TODO request start/stop/step values (be smart about what tokens defined)
    
    #DEBUG
    debugFlag = True
    outfile = 'out.out'
    tokens = ['x']
    hamiltonian = parseHamiltonian(tokens)
    start = [0]
    stop = [10]
    step = [0.1]

    return debugFlag,outFile,hamiltonian,tokens,start,stop,step
    #END DEBUG

def parseHamiltonian(tokens)
    '''
        This function takes user input and returns a list of lists representing
        the Hamiltonian matrix
    '''

    #TODO request hamiltonian input
    #TODO read hamiltonian
    #TODO check parsed hamiltonian against tokens
    #TODO check hamiltonian symmetric

    #DEBUG
    hamiltonian = [[0,1,tokens[0]],[1,0,1],[tokens[0],1,0]]
    return hamiltonian
    #END DEBUG
