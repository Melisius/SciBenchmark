import numpy as np
import time
import os
import NBody.NBody_numba.runNBody_numba as NBody_numba
import NBody.NBody_cython.runNBody_cython as NBody_cython
import NBody.NBody_f2py.runNBody_f2py as NBody_f2py

def runNBodyinitializer():
    pwd = os.getcwd()
    particles  = np.genfromtxt(pwd+'/NBody/particlesdone.txt')
    parameters = np.genfromtxt(pwd+'/NBody/parametersdone.txt')
    cuttoff = parameters[0]
    dt      = parameters[1]
    m       = parameters[2]
    eps     = parameters[3]
    sigma   = parameters[4]
    N       = parameters[5] # Is not used
    box_x   = parameters[6]
    box_y   = parameters[7]
    box_z   = parameters[8]
    steps   = 100
    return particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps

def runNBody():
    
    particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps = runNBodyinitializer()
    start = time.time()
    NBody_numba.runMD(particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps)
    print(time.time()-start, 'Numba')
    
    particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps = runNBodyinitializer()
    start = time.time()
    NBody_cython.runMD(particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps)
    print(time.time()-start, 'Cython')
    
    particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps = runNBodyinitializer()
    particles = np.asfortranarray(particles)
    start = time.time()
    NBody_f2py.runMD(particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps)
    print(time.time()-start, 'f2py')
    
    
runNBody()