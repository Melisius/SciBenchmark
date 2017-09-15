import numpy as np
import os
import NBody.NBody_f2py.nbody_f2py as NBody_f2py

def runMD(particles, cuttoff, dt, m, eps, sigma, box_x, box_y, box_z, steps):
    pwd = os.getcwd()
    out = open(pwd+'/NBody/NBody_f2py/output.txt','w')
    for step in range(1, steps+1):
        pot, Ekin = NBody_f2py.nbody.velocityverlet(particles, cuttoff, m, eps, sigma, box_x, box_y, box_z, dt)
        out.write(str(Ekin)+' '+str(pot)+'\n')
    out.close()