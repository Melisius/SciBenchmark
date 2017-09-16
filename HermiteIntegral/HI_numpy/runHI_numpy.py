import numpy as np
import time


def factorial2(n):
	n_range = int(n)
	out = 1.0
	if n > 0:
		for i in range(0, int(n_range+1)//2):
			out = out*(n-2*i)
	return out


def boys(m,z):
	pi = 3.141592653589793238462643383279
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


def R(l1l2, m1m2, n1n2, Cx, Cy, Cz, Px,  Py, Pz, p, R1, Rbuffer):
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


def runHI():
	integralargs = np.genfromtxt('integralinput.txt')
	timingfile = open('timing.txt','w')
	outfile = open('output.txt','w')
	integralfloat = integralargs[:,3:]
	integralint = integralargs[:,0:3].astype(int)
	
	N = 1
	
	R1buffer = np.zeros((4*N+1,4*N+1,4*N+1))
	Rbuffer = np.zeros((4*N+1,4*N+1,4*N+1,3*4*N+1))
	
	start = time.time()
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
		outfile.write(str(R1[l1l2,m1m2,n1n2])+'\n')
	timingfile.write(str((time.time()-start)*100))
	timingfile.close()
	outfile.close()
	
runHI()

