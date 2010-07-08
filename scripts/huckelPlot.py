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
def main():
    from hamiltonian import hamiltonian
    from Numeric import arange
    import os
    import sys

    # parse command line options, set debug flag, set outFile name
    debugFlag,outFileName = parseArgs()

    # parse hamiltonian,system data etc (inbuilt ham methods)
    ham = hamiltonian()
    ham.parse()

    # open the outfile
    outFile = open(outFileName+'.out','w') # change the fileName to a file object

    # SHOULD BE ABLE TO DO THIS BY RECURSION...
    # only loop over 1st token till I can sort out the recursion
    # loop over token 1
    #      .....
    #      set the hamiltonian tokens to their values
    #      print the hamiltonian to ham.tmp
    #      call huckel_energy ham.tmp tmp.out N
    #      read in tmp.out 
    #      append the token values followed by the eigenvalues to the outfile
    #
    if len(ham.getTokens()) != 0:
        token = ham.getTokens()[0]
        start = float(ham.getTokenRange(token)[0])
        stop = float(ham.getTokenRange(token)[1])
        step = float(ham.getTokenRange(token)[2])
        for tokenVal in arange(start,stop,step):
            ham.assign(token,tokenVal)
            ham.filePrint("ham.tmp")
            os.system("../src/huckel_energy ham.tmp out.tmp N") # call Fortran code
            outTmp = open('out.tmp','r')
            outFile.write(str(tokenVal)+' '+outTmp.readline()) # write the eigenvalues to the outFile
            outTmp.close()
        os.system('rm ham.tmp out.tmp')
    else:
        print 'FAIL: no tokens specified'

    # close the outfile
    outFile.close()

    # write a gnuplot script

    plot(outFileName)

    #---END---

def parseArgs():
    '''
        Parse the command line arguments:

        USAGE: "-d" or "--debug" argument sets the debugFlag
               "-o" or "--outfile" sets the filename to print to 
               
        RETURNS: tuple of form:  (debugFlag,outfileName)
                                      |         |
                                      boolean   string
    '''

    from getopt import getopt
    from sys import argv

    shortOpt = 'do:'
    longOpt = ['debug','outfile=']
    
    debugFlag = False
    outfileName = 'result.out'
    options, rest = getopt(argv[1:],shortOpt,longOpt)

    if rest != []:  # unknown args passed
        print 'Unknown arguments: ', rest, ' passed: ignoring...'

    for option,value in options:
        if option in ('-d','--debug'):
            debugFlag = True
        if option in ('-o','--outfile'):
            outfileName = value
    
    if debugFlag:
        print outFileName
    return debugFlag,outfileName

def plot(outputFileName):

    outputFile = open(outputFileName+'.out','r')
    script = open(outputFileName+'.p','w')
    
    script.write('plot "'+outputFileName+'.out" using 1:2 , ')

    for columns in range(3,len(outputFile.readline().split())):
        script.write('"'+outputFileName+'.out" using 1:'+str(columns)+', ')
    
    script.write('"'+outputFileName+'.out" using 1:'+str(len(outputFile.readline().split())))

    
if __name__ == '__main__':
    main()
