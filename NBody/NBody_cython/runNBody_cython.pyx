import cython
import os
import numpy as np
cimport numpy as np
include "NBody_cython.pxi"

cpdef runMD(double[:,:] particles, double cuttoff, double dt, double m, double eps, double sigma, double box_x, double box_y, double box_z, int steps):
    cdef double Ekin, pot
    cdef int step, i
    pwd = os.getcwd()
    out = open(pwd+'/NBody/NBody_cython/output.txt','w')
    for step in range(1, steps+1):
        particles, pot, Ekin = VelocityVerlet(particles, cuttoff, m, eps, sigma, box_x, box_y, box_z, dt)
        out.write(str(Ekin)+' '+str(pot)+'\n')
    out.close()