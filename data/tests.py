import os
import subprocess
import numpy as np

def test_Nbody():
	## RUN ALL NBody
	os.chdir('NBody/NBody_numpy')
	bashCommand = "python runNBody_numpy.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../NBody_numba')
	bashCommand = "python runNBody_numba.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../NBody_cython')
	bashCommand = "python runNBody.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../NBody_f2py')
	bashCommand = "python runNBody_f2py.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../NBody_fortran')
	bashCommand = "./runNBody_fortran.run"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../NBody_cpp')
	bashCommand = "./runNBody_cpp.run"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()
	os.chdir('../..')
	
	## ASSERT RESUlTS NBody
	os.chdir('NBody')
	check = np.genfromtxt('NBodyOutputCheck.txt')
	os.chdir('NBody_numpy')
	calc = np.genfromtxt('output.txt')
	assert (check[0:4] - calc[0:4] < 1).all()
	os.chdir('../NBody_numba')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 1).all()
	os.chdir('../NBody_cython')
	calc  = np.genfromtxt('output.txt')
	assert (check - calc < 1).all()
	os.chdir('../NBody_f2py')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 1).all()
	os.chdir('../NBody_fortran')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 1).all()
	os.chdir('../NBody_cpp')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 1).all()
	os.chdir('../..')

def test_HermiteIntegral():
	## RUN ALL HermiteIntegral
	os.chdir('HermiteIntegral/HI_numpy')
	bashCommand = "python runHI_numpy.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../HI_numba')
	bashCommand = "python runHI_numba.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../HI_cython')
	bashCommand = "python runHI.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../HI_f2py')
	bashCommand = "python runHI_f2py.py"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../HI_fortran')
	bashCommand = "./runHI_fortran.run"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()

	os.chdir('../HI_cpp')
	bashCommand = "./runHI_cpp.run"
	process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()
	os.chdir('../..')

	## ASSERT RSULTS HermiteIntegral
	os.chdir('HermiteIntegral')
	check = np.genfromtxt('HIOutputCheck.txt')
	os.chdir('HI_numpy')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 10**-4).all()
	os.chdir('../HI_numba')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 10**-4).all()
	os.chdir('../HI_cython')
	calc  = np.genfromtxt('output.txt')
	assert (check - calc < 10**-4).all()
	os.chdir('../HI_f2py')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 10**-4).all()
	os.chdir('../HI_fortran')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 10**-4).all()
	os.chdir('../HI_cpp')
	calc = np.genfromtxt('output.txt')
	assert (check - calc < 10**-4).all()
	os.chdir('../..')
