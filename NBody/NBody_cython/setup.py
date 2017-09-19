from setuptools import setup
from setuptools.extension import Extension
from Cython.Build import cythonize
import numpy
import Cython.Compiler.Options
Cython.Compiler.Options.get_directive_defaults()['cdivision'] = True
Cython.Compiler.Options.get_directive_defaults()['boundscheck'] = False
Cython.Compiler.Options.get_directive_defaults()['wraparound'] = False

ext_modules=[Extension('runNBody_cython',['runNBody_cython.pyx'],extra_compile_args=["-O3 -march=native"])]

setup(ext_modules=cythonize(ext_modules), include_dirs=[numpy.get_include()])