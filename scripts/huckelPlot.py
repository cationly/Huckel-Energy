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

from hamiltonian.py import hamiltonian
import os
import sys

# parse command line options, set debug flag, set outFile name

# parse hamiltonian,system data etc (inbuilt ham methods)
# for tokens, prompt use to enter start,stop and step values, put in a map

# open the outfile

# SHOULD BE ABLE TO DO THIS BY RECURSION...
# loop over token 1
#     loop over token 2
#         .....
#             set the hamiltonian tokens to their values
#             print the hamiltonian to ham.tmp
#             call huckel_energy ham.tmp tmp.out N
#             read in tmp.out 
#             append the token values followed by the eigenvalues to the outfile
#

# close the outfile

#---END---
