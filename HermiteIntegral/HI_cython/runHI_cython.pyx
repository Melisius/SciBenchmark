import cython
import time
import numpy as np
cimport numpy as np
include "HI_cython.pxi"

cpdef runHI():
    cdef double[:,:] integralfloat
    cdef int[:,:] integralint
    cdef int N, j, i
    cdef double[:,:,:] R1buffer
    cdef double[:,:,:,:] Rbuffer
    cdef double start
    integralargs = np.genfromtxt('integralinput.txt')
    timingfile = open('timing.txt','w')
    integralfloat = integralargs[:,3:]
    integralint = integralargs[:,0:3].astype(np.int32)
    
    N = 1
    
    R1buffer = np.zeros((4*N+1,4*N+1,4*N+1))
    Rbuffer = np.zeros((4*N+1,4*N+1,4*N+1,3*4*N+1))
    
    start = time.time()
    for j in range(10):
        outfile = open('output.txt','w')
        for i in range(0, len(integralargs)):
            l1l2 = integralint[i,0]
            m1m2 = integralint[i,1]
            n1n2 = integralint[i,2]
            Cx = integralfloat[i,0]
            Cy = integralfloat[i,1]
            Cz = integralfloat[i,2]
            Px = integralfloat[i,3]
            Py = integralfloat[i,4]
            Pz = integralfloat[i,5]
            p = integralfloat[i,6]
            R1 = R(l1l2, m1m2, n1n2, Cx, Cy, Cz, Px,  Py, Pz, p, R1buffer, Rbuffer)
            outfile.write(str(R1[l1l2, m1m2, n1n2])+'\n')
        outfile.close()
    timingfile.write(str((time.time()-start)*10))
    timingfile.close()