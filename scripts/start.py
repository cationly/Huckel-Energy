#!/usr/bin/python

from hamiltonian import hamiltonian

h = hamiltonian()
h.parse()

for token in h.getTokens():
    h.assign(token,100)
    print h.getTokenVal(token)
    h.getTokenRange(token)
print h.getElement(1,1), h.getElement(1,3)
h.filePrint("myhams.hams")
