
Installation and running
========================

Here the installtion and execution of the benchmark is described.

Requirements
------------

No version is given in requirements. Choose the version that is wanted to be benchmarked.

- Python 3.x
- numpy 1.x
- numba 0.x
- cython 0.x
- gcc 5.x
- gfortran 5.x
- g++ 5.x


Installation
------------

If all of the above requirements are met just execute the following lines:

::

   git clone https://github.com/Melisius/SlowQuant.git

In the folder SciBenchmark

::

   python compile_all.py
   
   
Running
-------

The benchmark is run by executing in the folder SciBenchmark:

::
   
   python run_all.py

The benchmark timing will be written to the file Benchmarks.csv