import os
import subprocess
import numpy as np
"""
# RUN ALL NBODY
os.chdir('NBody/NBody_numpy')
bashCommand = "python runNBody_numpy.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_numpy done')

os.chdir('../NBody_numba')
bashCommand = "python runNBody_numba.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_numba done')

os.chdir('../NBody_cython')
bashCommand = "python runNBody.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_cython done')

os.chdir('../NBody_f2py')
bashCommand = "python runNBody_f2py.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_f2py done')

os.chdir('../NBody_fortran')
bashCommand = "./runNBody_fortran.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_fortran done')

os.chdir('../NBody_cpp')
bashCommand = "./runNBody_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_cpp done')
os.chdir('../..')
"""
# ASSERT NBody results
os.chdir('NBody')
check = np.genfromtxt('NBodyOutputCheck.txt')
os.chdir('NBody_numba')
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
print('Results checked')

# COMPILE ALL TIMINGS
os.chdir('NBody/NBody_numpy')
t_numpy   = np.genfromtxt('timing.txt')
os.chdir('../NBody_numba')
t_numba   = np.genfromtxt('timing.txt')
os.chdir('../NBody_cython')
t_cython  = np.genfromtxt('timing.txt')
os.chdir('../NBody_f2py')
t_f2py    = np.genfromtxt('timing.txt')
os.chdir('../NBody_fortran')
t_fortran = np.genfromtxt('timing.txt')
os.chdir('../NBody_cpp')
t_cpp     = np.genfromtxt('timing.txt')
os.chdir('../..')
fast = np.min([t_numpy,t_numba,t_cython,t_f2py,t_fortran,t_cpp])

# WRITE BENCHMARK FILE
timefile = open('Benchmarks.csv','w')
timefile.write(',Numpy,Numba,Cython,f2py,Fortran,C++'+'\n')
timefile.write('NBody per step,'+str(t_numpy)+','+str(t_numba)+','+str(t_cython)+','+str(t_f2py)+','+str(t_fortran)+','+str(t_cpp)+'\n')
timefile.write('NBody relative to fastest,'+str(t_numpy/fast)+','+str(t_numba/fast)+','+str(t_cython/fast)+','+str(t_f2py/fast)+','+str(t_fortran/fast)+','+str(t_cpp/fast)+'\n')
timefile.close()

