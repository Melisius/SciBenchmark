import cython
import numpy as np
cimport numpy as np
from libc.math cimport round


cdef Get_Forces(double[:,:] particles, double m, double sigma, double eps, double cuttoff, double box_x, double box_y, double box_z):
    cdef double cuttoffsq, sigma6, sigma12, pot, X, Y, Z, x2, y2, z2, x1, y1, z1, rsq, r2, r6, F
    cdef int i, j, N
    
    cuttoffsq = (sigma*cuttoff)**2
    sigma6 = sigma**6
    sigma12 = sigma**12
    pot = 0.0
    N = len(particles)
    
    for i in range(0, N-1):
        x1 = particles[i,0]
        y1 = particles[i,1]
        z1 = particles[i,2]
        
        for j in range(i+1, N):
            
            x2 = particles[j,0]
            y2 = particles[j,1]
            z2 = particles[j,2]
            
            X = x2 - x1
            Y = y2 - y1
            Z = z2 - z1
            
            X  -= box_x * round(X/box_x)
            Y  -= box_y * round(Y/box_y)
            Z  -= box_z * round(Z/box_z)
            
            rsq = X*X + Y*Y + Z*Z
            if rsq <= cuttoffsq:
                r2 = 1/rsq
                r6 = r2*r2*r2
                
                pot += 4.0*eps*(sigma12*r6 - sigma6)*r6
                #F is the force multiplied with 1/r
                F = 48.0*eps*(-sigma12*r6 + 0.5*sigma6)*r2*r6
                
                particles[i,6] += F/m*X
                particles[i,7] += F/m*Y
                particles[i,8] += F/m*Z
                particles[j,6] -= F/m*X
                particles[j,7] -= F/m*Y
                particles[j,8] -= F/m*Z

    return particles, pot
    

cdef VelocityVerlet(double[:,:] particles, double cuttoff, double m, double eps, double sigma, double box_x, double box_y, double box_z, double dt):
    cdef double Ekin, pot 
    cdef double[:,:] Forces_old
    cdef int N, i
    
    Forces_old = np.zeros((len(particles), 3))
    N = len(particles)
    Ekin = 0.0
    
    # Update position
    for i in range(0, N):
        particles[i,0] = particles[i,0] + particles[i,3]*dt + 0.5*particles[i,6]*dt*dt
        particles[i,1] = particles[i,1] + particles[i,4]*dt + 0.5*particles[i,7]*dt*dt
        particles[i,2] = particles[i,2] + particles[i,5]*dt + 0.5*particles[i,8]*dt*dt
        
        if particles[i,0] <= 0:
            particles[i,0] = particles[i,0] + box_x
        elif particles[i,0] >= box_x:
            particles[i,0] = particles[i,0] - box_x
            
        if particles[i,1] <= 0:
            particles[i,1] = particles[i,1] + box_y
        elif particles[i,1] >= box_y:
            particles[i,1] = particles[i,1] - box_y
        
        if particles[i,2] <= 0:
            particles[i,2] = particles[i,2] + box_z
        elif particles[i,2] >= box_z:
            particles[i,2] = particles[i,2] - box_z
        
        # Save old forces
        Forces_old[i,0] = particles[i,6]
        Forces_old[i,1] = particles[i,7]
        Forces_old[i,2] = particles[i,8]
        particles[i,6] = 0
        particles[i,7] = 0
        particles[i,8] = 0
        
    # Get forces
    particles, pot = Get_Forces(particles, m, sigma, eps, cuttoff, box_x, box_y, box_z)
    
    # Update velocity
    for i in range(0, N):
        particles[i,3] = particles[i,3] + 0.5*(Forces_old[i,0]+particles[i,6])*dt
        particles[i,4] = particles[i,4] + 0.5*(Forces_old[i,1]+particles[i,7])*dt
        particles[i,5] = particles[i,5] + 0.5*(Forces_old[i,2]+particles[i,8])*dt
        
        Ekin += 0.5*m*(particles[i,3]**2+particles[i,4]**2+particles[i,5]**2)

    return particles, pot, Ekin