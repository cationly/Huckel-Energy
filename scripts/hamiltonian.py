''' 
    This is a class to implement a Hamiltonian with up 
    variable parameters 
'''

class hamiltonian:

    def __init__(self)
        '''
            We simulate the hamiltonian as an array. The action of a "2D array"
            is replicated by a 1D array and knowledge of the hamiltonian size.
            This should be faster, although in reality other things will 
            probably kill the speed before array access, but it's nice to be
            efficient....
        '''

        elements = [] # The actual elements of the hamiltonian
        size = 0        # The dimension of the hamiltonian
        tokens = []       # list of the tokens to be used
        tokenLocs = {}    # a map of token "names" to their locations within H
        tokenRange = {}   # map of token names to a list of form [start,stop,step]

    def parse(self)
        '''
            Parse in the hamiltonian from the command line
        '''    
        
        # we have to reinitialise everything from scratch
        self.elements = []
        self.size = 0
        self.tokens = []
        self.tokenLocs = {}
        self.tokenRange = {}

        raw_input('Token declarations: ') tokenString
        if(len(tokenString.split()) > 2) # any more than 2 becomes unplottable
            while(len(tokenString.split()) > 2)
                raw_input('too many token declarations (max. 2)') tokenString

        for token in tokenString.split()
            self.tokens.append(token) # add it to our list of tokens
            self.tokenLocs[token] = [] # initialise our location dictionary
            self.tokenRange[token] = [] # initialise range dicionary 

        # prompt user for hamSize, set size
        raw_input('\n\nHamiltonian size: ') hamSize
        while(True)
            try:
                self.size = int(hamSize)
                break
            except ValueError
                raw_input('\n invalid size passed, choose again:') hamSize
        
        # prompt for hamiltonian
        print '\n\nInput hamiltonian matrix: '
        
        # iterate SIZE times
        for row in range(self.size)
            while(True)         # must make sure we have a valid line
                errVal = False  # is there an INvalid entry in the line? 

                raw_input() hamLine 
                if(len(hamline.split() == size))
                    for entry in hamline.split()
                        try:
                            float(entry)
                        except ValueError   # the value is *not* a number
                            if(entry not in self.tokenList) # and *not* a token
                                print "\nundefined token passed,\
                                        re-enter line: "
                                errVal = True # entry is invalid...
                                break # one invalid input ruins the line
                    
                    
                    if(errVal)    # entry is invalid so we cycle and ask for
                        continue  # input again
                    else
                        for entry in hamline.split()
                            try:
                                self.hamiltonian.append(float(entry))
                            except ValueError # if it is a token
                                self.hamiltonian.append(0) # could have put anything
                                self.tokenLocs[entry].append(len(hamiltonian)-1)
                                # the -1 accounts for the fact we start @ 0
                                break
                
                else
                    print "\ninvalid number of elements passed,\
                            re-enter line: "
                    continue
   
        # print spaces to niceify input
            print('                          ')
   
        # prompt user for start,stop and step values 
        for token in self.tokens
            errVal = False
            print('enter start,stop and step values for token "'+token+'" :') 
            while(True)
                raw_input() loop_vals
                
                if(len(loop_vals.split()) != 3)
                    print '\ninvalid number of inputs, re-enter: '
                    continue

                for value in loop_vals.split()
                    try:
                        float(value)
                    except ValueError
                        print'\nnon-numeric input, re-enter: '
                        errVal = True
                        break
                
                if(errVal)
                    continue
                else
                    for value in loop_vals.split()
                        self.tokenRange[token].append(value)
                    break    


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
        
        for token in self.tokens:
            if(len(self.tokens[token]) % 2 != 0)
                return False # if the number of token entries for any given 
                             # token is not even then cannot be symmetric
            
            for position in self.tokens[token]:
                row = position / self.size  # find the "row" of the element
                col = position % self.size  # "column" of the element
                if col*self.size + row not in self.tokens[token]: 
                    return False  # this is just taking the transpose
                # i.e. if the element's transposed position is not also a token
                # then the hamiltonian cannot be symmetric

        for row in range(self.size -1) # now check numerical values
            for col in range(row+1, self.size)
                if self.entries[row*self.size + col]\
                == self.entries[col*self.size + row]
                    continue
                else
                    return False
        
        return True
    
    def assign(self,token,value)
        '''
            assign the elements in 'token' to 'value'
        '''

        # iterate through tokenLocs(token)
        #    for each location, assign value to ham[loc]
 
        for ham.value
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
