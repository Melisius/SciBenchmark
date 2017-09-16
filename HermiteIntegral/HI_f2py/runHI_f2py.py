import numpy as np
import time
import hi_f2py as HI_f2py

def runHI():
	integralargs = np.genfromtxt('integralinput.txt')
	timingfile = open('timing.txt','w')
	integralfloat = integralargs[:,3:]
	integralint = integralargs[:,0:3].astype(int)
	
	N = 1
	
	R1buffer = np.zeros((4*N+1,4*N+1,4*N+1))
	Rbuffer = np.zeros((4*N+1,4*N+1,4*N+1,3*4*N+1))
	R1buffer = np.asfortranarray(R1buffer)
	Rbuffer = np.asfortranarray(Rbuffer)
	
	start = time.time()
	for j in range(100):
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
			HI_f2py.hi.r(l1l2, m1m2, n1n2, Cx, Cy, Cz, Px,  Py, Pz, p, R1buffer, Rbuffer)
			outfile.write(str(R1buffer[0,0,0])+'\n')
		outfile.close()
	timingfile.write(str(time.time()-start))
	timingfile.close()

	
runHI()

