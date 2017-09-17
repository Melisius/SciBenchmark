
N-Body
======

A gas constisting of a number of particles, is an example of an N-Body problem. Here is an overview of the mathematical funtions used in NBody.

Lenneard-Jones potential
------------------------

The Lennard-Jones potential is given as:

.. math::
   U_{\mathrm{vdw}}\left(r_{ij}\right)=4\varepsilon_{ij}\left[\left(\frac{\sigma_{ij}}{r_{ij}}\right)^{12}-\left(\frac{\sigma_{ij}}{r_{ij}}\right)^{6}\right]
   
The evaluated forces is given as the geometrical derivative of the LJ potential:

.. math::
   -\frac{\mathrm{d}}{\mathrm{d}\vec{r}}U_{\mathrm{vdw}}\left(r_{ij}\right)=F_{\mathrm{vdw}}\left(r_{ij}\right)=-4\varepsilon_{ij}\left[-\frac{12\sigma_{ij}^{12}}{r_{ij}^{13}}+\frac{6\sigma_{ij}^{6}}{r_{ij}^{7}}\right]

Velocity-Verlet
---------------

The Velocity Verlet algorithm is a symplectic integrator. The positions of the particles is updated as:

.. math::
   \vec{x}\left(t+\Delta t\right)=\vec{x}\left(t\right)+\vec{v}\left(t\right)\Delta t+\frac{1}{2}\vec{a}\left(t\right)\Delta t^{2}

.. math::
   \vec{v}\left(t+\Delta t\right)=\vec{v}\left(t\right)+\frac{1}{2}\left(\vec{a}\left(t\right)+\vec{a}\left(t+\Delta t\right)\right)\Delta t