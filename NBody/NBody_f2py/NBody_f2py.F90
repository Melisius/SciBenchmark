MODULE nbody
contains 

SUBROUTINE get_forces(particles, m, sigma, eps, cuttoff, box_x, box_y, box_z, pot, particlesout)
	IMPLICIT NONE
	REAL(8), INTENT(in) :: m, sigma, eps, cuttoff, box_x, box_y, box_z
	REAL(8), DIMENSION(:,:), INTENT(inout) :: particles
	REAL(8) :: X, Y, Z, x1, y1, z1, x2, y2, z2, F, rsq, r2, r6, cuttoffsq, sigma6, sigma12
	INTEGER :: i, j, N
	REAL(8), INTENT(out) :: pot
	REAL(8), DIMENSION(:,:), INTENT(out) :: particlesout
	
	cuttoffsq=(sigma*cuttoff)**2
	sigma6=sigma**6
	sigma12=sigma**12
	N = size(particles,1)
	DO i=1, N-1
		x1 = particles(i,1)
		y1 = particles(i,2)
		z1 = particles(i,3)
		DO j=i+1, N
			x2 = particles(j,1)
			y2 = particles(j,2)
			z2 = particles(j,3)
			
			X = x2 - x1
			Y = y2 - y1
			Z = z2 - z1
			
			X = -box_x*NINT(X/box_x)
			Y = -box_x*NINT(Y/box_y)
			Z = -box_x*NINT(Z/box_z)
			
			rsq = X*X + Y*Y + Z*Z*Z
			IF (rsq <= cuttoffsq) THEN
				r2 = 1.0d0/rsq
				r6 = r2*r2*r2
				
				pot = pot + 4.0d0*eps*(sigma12*r6 - sigma6)*r6
				F = 48.0d0*eps*(-sigma12*r6 + 0.5d0*sigma6)*r2*r6
				
				particles(i,7) = particles(i,7) + F/m*X
				particles(i,8) = particles(i,8) + F/m*X
				particles(i,9) = particles(i,9) + F/m*X
				particles(j,7) = particles(j,7) - F/m*X
				particles(j,8) = particles(j,8) - F/m*X
				particles(j,9) = particles(j,9) - F/m*X
			END IF
		END DO
	END DO
	particlesout = particles
END SUBROUTINE get_forces


SUBROUTINE velocityverlet(particles, cuttoff, m, eps, sigma, box_x, box_y, box_z, dt, particlesout, pot, ekin)
	REAL(8), INTENT(in) :: cuttoff, m, eps, sigma, box_x, box_y, box_z, dt
	REAL(8), DIMENSION(:,:), INTENT(inout) :: particles
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: forces_old
	INTEGER :: N, i
	REAL(8), INTENT(out) :: pot, ekin
	REAL(8), DIMENSION(:, :), INTENT(out) :: particlesout
	
	N = size(particles,1)
	ekin = 0.0d0
	ALLOCATE(forces_old(N,3))
	
	DO i=1, N
		particles(i,1) = particles(i,1) + particles(i,4)*dt + 0.5d0*particles(i,7)*dt*dt
		particles(i,2) = particles(i,2) + particles(i,5)*dt + 0.5d0*particles(i,8)*dt*dt
		particles(i,3) = particles(i,3) + particles(i,6)*dt + 0.5d0*particles(i,9)*dt*dt
		
		IF (particles(i,1) <= 0.0d0) THEN
			particles(i,1) = particles(i,1) + box_x
		ELSE IF (particles(i,1) >= box_x) THEN
			particles(i,1) = particles(i,1) - box_x
		END IF
		IF (particles(i,2) <= 0.0d0) THEN
			particles(i,2) = particles(i,2) + box_y
		ELSE IF (particles(i,2) >= box_y) THEN
			particles(i,2) = particles(i,2) - box_y
		END IF
		IF (particles(i,3) <= 0.0d0) THEN
			particles(i,3) = particles(i,3) + box_z
		ELSE IF (particles(i,3) >= box_z) THEN
			particles(i,3) = particles(i,3) - box_z
		END IF
		
		Forces_old(i,1) = particles(i,7)
        Forces_old(i,2) = particles(i,8)
        Forces_old(i,3) = particles(i,9)
        particles(i,7) = 0.0d0
        particles(i,8) = 0.0d0
        particles(i,9) = 0.0d0
	END DO
	
	CALL get_forces(particles, m, sigma, eps, cuttoff, box_x, box_y, box_z, pot, particlesout)
	
	DO i=1, N
		particlesout(i,4) = particlesout(i,4) + 0.5d0*(Forces_old(i,1)+particlesout(i,7))*dt
		particlesout(i,5) = particlesout(i,5) + 0.5d0*(Forces_old(i,2)+particlesout(i,8))*dt
		particlesout(i,6) = particlesout(i,6) + 0.5d0*(Forces_old(i,3)+particlesout(i,9))*dt
		
		ekin = ekin + 0.5d0*m*(particlesout(i,4)**2+particlesout(i,5)**2+particlesout(i,6)**2)
	END DO
END SUBROUTINE velocityverlet

END MODULE nbody