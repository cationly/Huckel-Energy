''' 
    This is a class to implement a Hamiltonian  
    variable parameters (now, up to 2)
'''

class hamiltonian:



    def __init__(self):
        '''
            We simulate the hamiltonian as an array. The action of a "2D array"
            is replicated by a 1D array and knowledge of the hamiltonian size.
            This should be faster, although in reality other things will 
            probably kill the speed before array access, but it's nice to be
            efficient....
        '''

        self.elements = [] # The actual elements of the hamiltonian
        self.size = 0        # The dimension of the hamiltonian
        self.tokens = []       # list of the tokens to be used
        self.tokenLocs = {}    # a map of token "names" to their locations within H
        self.tokenRange = {}   # map of token names to a list of form [start,stop,step]


    def parse(self):
        '''
            Parse in the hamiltonian from the command line
        '''    
        
        # we have to reinitialise everything from scratch
        self.elements = []
        self.size = 0
        self.tokens = []
        self.tokenLocs = {}
        self.tokenRange = {}

        tokenString = raw_input('Token declarations: ')
                
        #if len(tokenString.split()) == 0: 
        #    while len(tokenString.split()) > 2 or len(tokenString.split()) == 0:
        #        print('invalid number of token declarations (1 or 2)')                
        #        tokenString = raw_input('\nToken declarations: ')

        for token in tokenString.split():
            self.tokens.append(token) # add it to our list of tokens
            self.tokenLocs[token] = [] # initialise our location dictionary
            self.tokenRange[token] = [] # initialise range dicionary 

        # prompt user for hamSize, set size
        while True:
            try:
                hamSize = raw_input('\n Hamiltonian size: ')
                self.size = int(hamSize)
                if self.size == 0:
                    print '\ninvalid size passed'
                    continue
            except ValueError:
                hameSize = raw_input('\n invalid size passed, choose again: ')
                continue
            else:
                break


        # prompt for hamiltonian
        print '\n\nInput hamiltonian matrix:\n '
        
        # iterate SIZE times
        for row in range(self.size):
            while True:         # must make sure we have a valid line
                errVal = False  # is there an INvalid entry in the line? 

                hamLine = raw_input() 
                if len(hamLine.split()) == self.size:
                    for entry in hamLine.split():
                        try:
                            float(entry)
                        except ValueError:   # the value is *not* a number
                            if(entry not in self.tokens): # and *not* a token
                                print "\nundefined token passed,\
                                        re-enter line: "
                                errVal = True # entry is invalid...
                                break # one invalid input ruins the line 
                    
                    if(errVal):    # entry is invalid so we cycle and ask for
                        continue  # input again
                    else:
                        for entry in hamLine.split():
                            try:
                                self.elements.append(float(entry))
                            except ValueError: # if it is a token
                                self.elements.append(0.0) # could have put anything
                                self.tokenLocs[entry].append(len(self.elements)-1)
                                # the -1 accounts for the fact we start @ 0
                    break                

                else:
                    print "\ninvalid number of elements passed,\
                            re-enter line: "
                    continue
   


        # print spaces to niceify input
            print('                          ')
   
        # prompt user for start,stop and step values 
        for token in self.tokens:
            errVal = False
            print('enter start,stop and step values for token "'+token+'" :') 
            while True:
                loop_vals = raw_input()
                
                if(len(loop_vals.split()) != 3):
                    print '\ninvalid number of inputs, re-enter: '
                    continue
             
                for value in loop_vals.split():
                    try:
                        float(value)
                    except ValueError:
                        print'\nnon-numeric input, re-enter: '
                        errVal = True
                        break
                
                if(errVal):
                    continue
                else:
                    if float(loop_vals.split()[0]) > float(loop_vals.split()[1]):
                        print '\ninvalid range, re-enter all values: '
                        continue
                    elif float(loop_vals.split()[2]) == 0:
                        print '\ninvalid step value, re-enter all values: '
                        continue
                    
                    for value in loop_vals.split():
                        self.tokenRange[token].append(value)
                    break    

        if(not(self.symmetric())):
            print 'asymmetric hamiltonian parsed: please re-enter\n'
            self.parse()


    def symmetric(self):
        '''
            Test for a symmetric hamiltonian
        '''

        # iterate over upper triangle (minus the leading diagonal)
        #    i = 0 --> N-2
        #        j =  i+1 -->N-1
        #           test that ham[i][j] == ham[j][i]
        #           i.e. ham[i*size + j] == ham[j*size + i]
        #           if not then return false
        
        #for token in self.tokens:
        #    if(len(self.tokens[token]) % 2 != 0):
        #        return False # if the number of token entries for any given 
        #                     # token is not even then cannot be symmetric
        for token in self.tokens:    
            for position in self.tokenLocs[token]:
                row = position / self.size  # find the "row" of the element
                col = position % self.size  # "column" of the element
                if col*self.size + row not in self.tokenLocs[token]: 
                    return False  # this is just taking the transpose
            # i.e. if the element's transposed position is not also the 
            # same token then the hamiltonian cannot be symmetric

        # NB: even though the embedded loop is slower, it means it is more
        # evident what we are doing: also enables us to easily only choose
        # the upper triangle to iterate through - making it faster overall
        for row in range(self.size -1): # now check numerical values
            for col in range(row+1, self.size):
                if self.elements[row*self.size + col]\
                == self.elements[col*self.size + row]:
                    continue
                else:
                    return False
        
        return True
    


    def assign(self,token,value):
        '''
            assign the elements in 'token' to 'value'
        '''

        # iterate through tokenLocs(token)
        #    for each location, assign value to ham[loc]
        
        if token not in token:
            raise LookupError
        elif value < 0:
            raise ArithmeticError

        for location in self.tokenLocs[token]:
            self.elements[location] = value


    def filePrint(self,fileName):
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
        try:
             hamFile = open(fileName,'w')
             hamFile.write(str(self.size)+'\n')
             
             #NB: I do not know whether embedded loops or a single loop with
             # an IF statement would be more efficient. I think that the loops
             # would be more inefficient, as python would use more stack to 
             # keep the loop variables in scope, whereas IF statements are cheap

             for row in range(self.size):
                 for col in range(self.size):
                     hamFile.write(str(self.elements[row*self.size + col])+' ') 
                 hamFile.write('\n')

             hamFile.close()
             return 0

        except TypeError:  # invalid filename type (not string) passed
            return 1


    def getTokenVal(self,string):
        '''
            Return a list of the values of the tokens in the hamiltonian
        '''
        
        # if string in tokens
        #     return ham[tokenLocs[string][0]] 
        # else
        #     return BADNUM
        if string in self.tokens:
                return self.elements[self.tokenLocs[string][0]]
        else:
                raise LookupError 
        

    def getTokenRange(self,string):
        '''
            Return a list of form [start,stop,step] for token "string"
        '''

        # if string in tokens
        #     return tokenRange[string]
        # else
        #     return BADNUM
        if string in self.tokens:
            return self.tokenRange[string]
        else:
            return [] # the empty list


    def getTokens(self):
        return self.tokens
    
    def getElement(self,i,j):
        '''
            Return element i,j of the hamiltonian. The "-1"s are an offset so
            that the user can have a 1-based as opposed to 0-based index for
            compatability with standard mathematical notation
        '''

        if(self.size == 0):
            raise ArithmeticError
        elif((i-1)*self.size + (j-1) > self.size*self.size -1):
            raise LookupError
        else:
            return self.elements[(i-1)*self.size + (j-1)]
          
