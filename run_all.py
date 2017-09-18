import os
import subprocess
import numpy as np

## RUN ALL NBody
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

os.chdir('../NBody_cpp_gpp')
bashCommand = "./runNBody_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_c++_g++ done')

os.chdir('../NBody_cpp_clangpp')
bashCommand = "./runNBody_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('NBody_c++_clang++ done')
os.chdir('../..')

## RUN ALL HermiteIntegral
os.chdir('HermiteIntegral/HI_numpy')
bashCommand = "python runHI_numpy.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_numpy done')

os.chdir('../HI_numba')
bashCommand = "python runHI_numba.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_numba done')

os.chdir('../HI_cython')
bashCommand = "python runHI.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_cython done')

os.chdir('../HI_f2py')
bashCommand = "python runHI_f2py.py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_f2py done')

os.chdir('../HI_fortran')
bashCommand = "./runHI_fortran.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_fortran done')

os.chdir('../HI_cpp_gpp')
bashCommand = "./runHI_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_c++_g++ done')

os.chdir('../HI_cpp_clangpp')
bashCommand = "./runHI_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print('HI_c++_clang++ done')
os.chdir('../..')

## ASSERT RESUlTS NBody
os.chdir('NBody')
check = np.genfromtxt('NBodyOutputCheck.txt')
os.chdir('NBody_numpy')
calc = np.genfromtxt('output.txt')
assert (np.abs(check[0:4] - calc[0:4]) < 1).all()
os.chdir('../NBody_numba')
calc = np.genfromtxt('output.txt')
assert (np.abs(np.abs(check - calc)) < 1).all()
os.chdir('../NBody_cython')
calc  = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 1).all()
os.chdir('../NBody_f2py')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 1).all()
os.chdir('../NBody_fortran')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 1).all()
os.chdir('../NBody_cpp_gpp')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 1).all()
os.chdir('../NBody_cpp_clangpp')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 1).all()
os.chdir('../..')
print('NBody results checked')

## ASSERT RSULTS HermiteIntegral
os.chdir('HermiteIntegral')
check = np.genfromtxt('HIOutputCheck.txt')
os.chdir('HI_numpy')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 10**-4).all()
os.chdir('../HI_numba')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 10**-4).all()
os.chdir('../HI_cython')
calc  = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 10**-4).all()
os.chdir('../HI_f2py')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 10**-4).all()
os.chdir('../HI_fortran')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 10**-4).all()
os.chdir('../HI_cpp_gpp')
calc = np.genfromtxt('output.txt')
assert (np.abs(check - calc) < 10**-4).all()
os.chdir('../HI_cpp_clangpp')
calc = np.genfromtxt('output.txt')
assert (np.abs(check[0:100] - calc[0:100]) < 10**-4).all()
os.chdir('../..')
print('HermiteINtegral results checked')

## WRITE BENCHMARK FILE NBody
timefile = open('Benchmarks.csv','w')
timefile.write(',Numpy,Numba,Cython,f2py,Fortran,C++ (g++),C++ (clang++)'+'\n')

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
os.chdir('../NBody_cpp_gpp')
t_cpp_gpp     = np.genfromtxt('timing.txt')
os.chdir('../NBody_cpp_clangpp')
t_cpp_clangpp     = np.genfromtxt('timing.txt')
os.chdir('../..')
fast = np.min([t_numpy,t_numba,t_cython,t_f2py,t_fortran,t_cpp_gpp,t_cpp_clangpp])

timefile.write('NBody,'+"{0:.2f}".format(t_numpy/fast)+','+"{0:.2f}".format(t_numba/fast)+','+"{0:.2f}".format(t_cython/fast)+','+"{0:.2f}".format(t_f2py/fast)+','+"{0:.2f}".format(t_fortran/fast)+','+"{0:.2f}".format(t_cpp_gpp/fast)+','+"{0:.2f}".format(t_cpp_clangpp/fast)+'\n')

## WRITE BENCHMARK FILE HermiteIntegral
os.chdir('HermiteIntegral/HI_numpy')
t_numpy   = np.genfromtxt('timing.txt')
os.chdir('../HI_numba')
t_numba   = np.genfromtxt('timing.txt')
os.chdir('../HI_cython')
t_cython  = np.genfromtxt('timing.txt')
os.chdir('../HI_f2py')
t_f2py    = np.genfromtxt('timing.txt')
os.chdir('../HI_fortran')
t_fortran = np.genfromtxt('timing.txt')
os.chdir('../HI_cpp_gpp')
t_cpp_gpp     = np.genfromtxt('timing.txt')
os.chdir('../HI_cpp_clangpp')
t_cpp_clangpp     = np.genfromtxt('timing.txt')
os.chdir('../..')
fast = np.min([t_numpy,t_numba,t_cython,t_f2py,t_fortran,t_cpp_gpp,t_cpp_clangpp])

timefile.write('Hermite Integral,'+"{0:.2f}".format(t_numpy/fast)+','+"{0:.2f}".format(t_numba/fast)+','+"{0:.2f}".format(t_cython/fast)+','+"{0:.2f}".format(t_f2py/fast)+','+"{0:.2f}".format(t_fortran/fast)+','+"{0:.2f}".format(t_cpp_gpp/fast)+','+"{0:.2f}".format(t_cpp_clangpp/fast)+'\n')

timefile.close()
print('Benchmark file written')
