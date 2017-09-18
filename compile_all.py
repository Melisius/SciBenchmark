import os
import subprocess

# Compile all NBody
os.chdir('NBody/NBody_cython')
bashCommand = "python setup.py build_ext --inplace"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_f2py')
bashCommand = "f2py -c NBody_f2py.F90 --f90flags='-march=native' --opt=-Ofast -m nbody_f2py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_cpp_gpp')
bashCommand = "g++ -march=native -std=c++11 -Ofast runNBody_cpp.cc -o runNBody_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_cpp_clangpp')
bashCommand = "clang++ -march=native -std=c++11 -Ofast runNBody_cpp.cc -o runNBody_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../NBody_fortran')
bashCommand = "gfortran -march=native -Ofast runNBody_fortran.F90 -o runNBody_fortran.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)
os.chdir('../..')

# Compile all HermiteIntegral
os.chdir('HermiteIntegral/HI_cython')
bashCommand = "python setup.py build_ext --inplace"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../HI_f2py')
bashCommand = "f2py -c HI_f2py.F90 --f90flags='-march=native' --opt=-Ofast -m hi_f2py"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../HI_cpp_gpp')
bashCommand = "g++ -march=native -std=c++11 -Ofast runHI_cpp.cc -o runHI_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../HI_cpp_clangpp')
bashCommand = "clang++ -march=native -std=c++11 -Ofast runHI_cpp.cc -o runHI_cpp.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)

os.chdir('../HI_fortran')
bashCommand = "gfortran -march=native -Ofast runHI_fortran.F90 -o runHI_fortran.run"
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output, error = process.communicate()
print(output)
os.chdir('../..')