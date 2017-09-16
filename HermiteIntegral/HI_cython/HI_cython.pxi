import cython
import numpy as np
cimport numpy as np

cdef double factorial2(double n):
    cdef int n_range, i
    cdef double out
    n_range = int(n)
    out = 1.0
    if n > 0:
        for i in range(0, int(n_range+1)//2):
            out = out*(n-2*i)
    return out


cdef double boys(double m, double z):
    cdef int i
    cdef double F, Fcheck, temp1
    cdef double pi = 3.141592653589793238462643383279
    if z > 25:
        F = factorial2(2*m-1)/(2**(m+1))*(pi/(z**(2*m+1)))**0.5
    else:
        F = 0.0
        temp1 = factorial2(2*m-1)
        for i in range(0, 100):
            Fcheck = F
            F += (temp1*(2*z)**i)/(factorial2(2*m+2*i+1))
            Fcheck -= F
            if abs(Fcheck) < 10**-12:
                break
        F *= np.exp(-z)
    return F


cdef double[:,:,:] R(int l1l2, int m1m2, int n1n2, double Cx, double Cy, double Cz, double Px, double Py, double Pz, double p, double[:,:,:] R1, double[:,:,:,:] Rbuffer):
    cdef int t, u, v, exclude_from_n
    cdef double PCx, PCy, PCz, RPC, val
    PCx = Px-Cx
    PCy = Py-Cy
    PCz = Pz-Cz
    RPC = ((PCx)**2+(PCy)**2+(PCz)**2)**0.5
    for t in range(0, l1l2+1):
        for u in range(0, m1m2+1):
            for v in range(0, n1n2+1):
                # Check the range of n, to ensure no redundent n are calculated
                if t == u == 0:
                    exclude_from_n = v
                elif t == 0:
                    exclude_from_n = n1n2 + u
                else:
                    exclude_from_n = n1n2 + m1m2 + t
                for n in range(0, l1l2+m1m2+n1n2+1-exclude_from_n):
                    val = 0.0
                    if t == u == v == 0:
                        Rbuffer[t,u,v,n] = (-2.0*p)**n*boys(n,p*RPC*RPC)
                    else:
                        if t == u == 0:
                            if v > 1:
                                val += (v-1)*Rbuffer[t,u,v-2,n+1]
                            val += PCz*Rbuffer[t,u,v-1,n+1]  
                        elif t == 0:
                            if u > 1:
                                val += (u-1)*Rbuffer[t,u-2,v,n+1]
                            val += PCy*Rbuffer[t,u-1,v,n+1]
                        else:
                            if t > 1:
                                val += (t-1)*Rbuffer[t-2,u,v,n+1]
                            val += PCx*Rbuffer[t-1,u,v,n+1]
                        Rbuffer[t,u,v,n] = val
                        
                    if n == 0:
                        R1[t,u,v] = Rbuffer[t,u,v,n]
    return R1