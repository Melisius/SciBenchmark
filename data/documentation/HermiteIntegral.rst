

Hermite Integral
================

Hermite integrals can be used to evaluate electronic integrals. Here is an overview of the mathematical funtions used in the Hermite integrals.

Boys function
-------------

The boys can be evaulated as:

.. math::
   F_{m}(z)=\exp\left(-z\right)\sum_{i=0}^{\infty}\frac{\left(2m-1\right)!!\left(2z\right)^{i}}{\left(2m+2i+1\right)!!}

And for large $z$ larger than 25 it can be approximated as:

.. math::
   F_{m}\left(z\right)\approx\frac{\left(2m-1\right)!!}{2^{m+1}}\sqrt{\frac{\pi}{z^{2m+1}}}
   
Hermite integral
------------------------

The Hermite integrals is given as the following recurrence relations:

.. math::
   R_{t+1,u,v}^{n}\left(p,R_{\mathrm{PC}}\right)=tR_{t-1,u,v}^{n+1}\left(p,R_{\mathrm{PC}}\right)+X_{\mathrm{PC}}R_{t,u,v}^{n+1}\left(p,R_{\mathrm{PC}}\right)

.. math::
   R_{t,u+1,v}^{n}\left(p,R_{\mathrm{PC}}\right)=uR_{t,u-1,v}^{n+1}\left(p,R_{\mathrm{PC}}\right)+Y_{\mathrm{PC}}R_{t,u,v}^{n+1}\left(p,R_{\mathrm{PC}}\right)

.. math::
   R_{t,u,v+1}^{n}\left(p,R_{\mathrm{PC}}\right)=vR_{t,u,v-1}^{n+1}\left(p,R_{\mathrm{PC}}\right)+Z_{\mathrm{PC}}R_{t,u,v}^{n+1}\left(p,R_{\mathrm{PC}}\right)

With the boundary condition:

.. math::
   R_{0,0,0}^{n}\left(p,R_{\mathrm{PC}}\right)=\left(-2p\right)^{n}F_{n}\left(pR_{\mathrm{PC}}^{2}\right)
