''' 
    This is a class to implement a Hamiltonian with up 
    variable parameters 
'''

class hamiltonian:
    import array

    def __init__(self)
        '''
            We simulate the hamiltonian as an array. The action of a "2D array"
            is replicated by a 1D array and knowledge of the hamiltonian size.
            This should be faster, although in reality other things will 
            probably kill the speed before array access, but it's nice to be
            efficient....
        '''

        elements = array('f') # The actual elements of the hamiltonian
        size = 0        # The dimension of the hamiltonian
        tokens = []       # list of the tokens to be used
        tokenLocs = {}    # a map of token "names" to their locations within H
        tokenRange = {}   # map of token names to a list of form [start,stop,step]

    def parse()
        '''
            Parse in the hamiltonian from the command line
        '''    
        
        # prompt user for token declarations
        # store tokens in tokens and tokenLocs 

        # prompt user for hamSize, set size
        
        # prompt for hamiltonian
        # iterate SIZE times
        #    while(0)
        #         get hamLine       
        #         if(len(hamline.split() == size))
        #             iterate through words in hamLine and set hamiltonian elements
        #                 if not floats then set element to 0 and append array index to
        #                 tokenLocs (appropriate token) (check for invalid tokens too)
        #             break
        #         else
        #             print "invalid number of elements passed; re-enter line \n"
        #             continue
        #
        # print spaces to niceify input
        # get and store hamline
   
        # for token in tokens: prompt user for start,stop and step values 

    def symmetric(self)
        '''
            Test for a symmetric hamiltonian
        '''

        # iterate over upper triangle (minus the leading diagonal)
        #    i = 0 --> N-2
        #        j =  i+1 -->N-1
        #           test that ham[i][j] == ham[j][i]
        #           i.e. ham[i*size + j] == ham[j*size + i]
        #           if not then return false
        

    def assign(self,token,value)
        '''
            assign the elements in 'token' to 'value'
        '''

        # iterate through tokenLocs(token)
        #    for each location, assign value to ham[loc]
 

    def print(self,fileName)
        ''' 
            Print the hamiltonian to a file in a format compatible with
            the huckel_energy program.
        '''

        # open file
        # print size
        # for i in sequence 0 --> size-1
        #     for j in sequence 0 --> size-1
        #         print ham[i*size + j]
        #     print "\n"
        # close file


    def getTokenVal(self,string)
        '''
            Return a list of the values of the tokens in the hamiltonian
        '''
        
        # if string in tokens
        #     return ham[tokenLocs[string][0]] 
        # else
        #     return BADNUM
    

    def getTokenRange(self,string)
        '''
            Return a list of form [start,stop,step] for token "string"
        '''

        # if string in tokens
        #     return tokenRange[string]
        # else
        #     return BADNUM


    def getTokens(self)
        return self.tokens
    
    def getElement(self,i,j)
        '''
            Return element i,j of the hamiltonian
        '''
        if(self.size == 0)
            # RETURN NAN
        else    
            return self.elements(i*size + j)
