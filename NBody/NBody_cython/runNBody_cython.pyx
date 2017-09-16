import cython
import time
import numpy as np
cimport numpy as np
include "NBody_cython.pxi"

cpdef runMD():
    cdef double Ekin, pot, cuttoff, dt, m, eps, sigma, N, box_x, box_y, box_z, start, runtime
    cdef double[:,:] particles
    cdef double[:] parameters
    cdef int step, i, steps
    particles  = np.genfromtxt('particlesdone.txt')
    parameters = np.genfromtxt('parametersdone.txt')
    cuttoff = parameters[0]
    dt      = parameters[1]
    m       = parameters[2]
    eps     = parameters[3]
    sigma   = parameters[4]
    N       = parameters[5] # Is not used
    box_x   = parameters[6]
    box_y   = parameters[7]
    box_z   = parameters[8]
    steps   = 800
    out = open('output.txt','w')
    timing = open('timing.txt','w')
    start = time.time()
    for step in range(1, steps+1):
        particles, pot, Ekin = VelocityVerlet(particles, cuttoff, m, eps, sigma, box_x, box_y, box_z, dt)
        out.write(str(Ekin)+' '+str(pot)+'\n')
    runtime = time.time() - start
    timing.write(str(runtime/steps))
    timing.close()
    out.close()