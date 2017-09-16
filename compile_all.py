import os
import subprocess

os.chdir('NBody/NBody_cython')
bashCommand = "python setup.py build_ext --inplace"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_f2py')
bashCommand = "f2py -c NBody_f2py.F90 --f90flags=-O3 -m nbody_f2py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_cpp')
bashCommand = "g++ -std=c++14 -O3 runNBody_cpp.cc -o runNBody_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_fortran')
bashCommand = "gfortran -O3 runNBody_fortran.F90 -o runNBody_fortran.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)
