
MODULE hi
contains

PURE SUBROUTINE factorial2(n, outval)
	IMPLICIT NONE
	REAL(8), INTENT(in) :: n
	INTEGER :: n_range, i
	REAL(8), INTENT(out) :: outval
	n_range = int(n)
	outval = 1.0d0
	IF (n > 0) THEN
		DO i=0, (n_range+1)/2-1
			outval = outval*(n-2*i)
		END DO
	END IF
END SUBROUTINE factorial2

PURE SUBROUTINE boys(m, z, F)
	IMPLICIT NONE
	REAL(8), INTENT(in) :: m, z
	INTEGER :: i
	REAL(8) :: Fcheck, outval, temp1
	REAL(8), PARAMETER :: pi = 3.141592653589793238462643383279
	REAL(8), INTENT(out) :: F
	IF (z > 25.0d0) THEN
		CALL factorial2(2*m-1,outval)
		F = outval/(2.0d0**(m+1))*(pi/(z**(2*m+1)))**0.5
	ELSE
		F = 0.0d0
		CALL factorial2(2*m-1,outval)
		temp1 = outval
		DO i=0, 100
			Fcheck = F
			CALL factorial2(2*m+2*i+1,outval)
			F = F + (temp1*(2.0d0*z)**i)/outval
			Fcheck = Fcheck - F
			IF (ABS(Fcheck) < 10e-12) THEN
				EXIT
			END IF
		END DO
		F = F*exp(-z)
	END IF
END SUBROUTINE boys
	
PURE SUBROUTINE R(l1l2, m1m2, n1n2, Cx, Cy, Cz, Px, Py, Pz, p, R1, Rbuffer)
    IMPLICIT NONE
    
    ! INPUTS
    INTEGER, INTENT(in) :: l1l2, m1m2, n1n2
    REAL(8) , INTENT(in):: Cx, Cy, Cz, Px, Py, Pz, p
    REAL(8), DIMENSION(:,:,:,:), INTENT(inout) :: Rbuffer
    
    ! OUTPUTS
    REAL(8), DIMENSION(:,:,:), INTENT(inout) :: R1
    
    ! INTERNAL
    INTEGER :: t, u, v, n, exclude_from_n
    REAL(8) :: RPC, PCx, PCy, PCz, outputvalue, F
    
    PCx = Px-Cx
    PCy = Py-Cy
    PCz = Pz-Cz
    RPC = ((PCx)**2+(PCy)**2+(PCz)**2)**0.5
    DO t = 0, l1l2
        DO u = 0, m1m2
            DO v = 0, n1n2
                ! Check the range of n, to ensure no redundent n are calculated
                IF (t == 0 .AND. u == 0) THEN
                    exclude_from_n = v
                ELSEIF (t == 0) THEN
                    exclude_from_n = n1n2 + u
                ELSE
                    exclude_from_n = n1n2 + m1m2 + t
                END IF
                
                DO n = 0, l1l2+m1m2+n1n2 - exclude_from_n
                    outputvalue = 0.0d0
                    IF (t == 0 .AND. u == 0 .AND. v == 0) THEN
						CALL boys(real(n,8),p*RPC*RPC,F)
                        Rbuffer(t+1,u+1,v+1,n+1) = (-2.0d0*p)**n*F
                    ELSE
                        IF (t == 0 .AND. u == 0) THEN
                            IF (v > 1) THEN
                                outputvalue = outputvalue + (v-1.0d0)*Rbuffer(t+1,u+1,v+1-2,n+1+1)
                            END IF
                            outputvalue = outputvalue + PCz*Rbuffer(t+1,u+1,v+1-1,n+1+1)  
                        ELSEIF (t == 0) THEN
                            IF (u > 1) THEN
                                outputvalue = outputvalue + (u-1.0d0)*Rbuffer(t+1,u+1-2,v+1,n+1+1)
                            END IF
                            outputvalue = outputvalue + PCy*Rbuffer(t+1,u+1-1,v+1,n+1+1)
                        ELSE
                            IF (t > 1) THEN
                                outputvalue = outputvalue + (t-1.0d0)*Rbuffer(t+1-2,u+1,v+1,n+1+1)
                            END IF
                            outputvalue = outputvalue + PCx*Rbuffer(t+1-1,u+1,v+1,n+1+1)
                        END IF
                        Rbuffer(t+1,u+1,v+1,n+1) = outputvalue
                    END IF
                        
                    IF (n == 0) THEN
                        R1(t+1,u+1,v+1) = Rbuffer(t+1,u+1,v+1,n+1)
                    END IF
                END DO
            END DO
        END DO
    END DO
END SUBROUTINE R
END MODULE hi

PROGRAM runhi
	USE hi
	INTEGER :: i,j,l1l2,m1m2,n1n2, end_time, begin_time, rate
	REAL(8) :: Cx, Cy, Cz, Px, Py, Pz, p
	REAL(8), DIMENSION(33777,10) :: parameters
	INTEGER, DIMENSION(33777,3) :: integralint
	REAL(8), DIMENSION(33777,7) :: integralfloat
	REAL(8), DIMENSION(5,5,5) :: R1buffer
	Real(8), DIMENSION(5,5,5,13) :: Rbuffer
	OPEN(7,file='integralinput.txt',status='old',action='read')
	DO i=1,33777
		READ(7,*) parameters(i,:)
	END DO
	CLOSE(7)
	DO i=1,33777
		integralint(i,1) = int(parameters(i,1))
		integralint(i,2) = int(parameters(i,2))
		integralint(i,3) = int(parameters(i,3))
		integralfloat(i,1) = parameters(i,4)
		integralfloat(i,2) = parameters(i,5)
		integralfloat(i,3) = parameters(i,6)
		integralfloat(i,4) = parameters(i,7)
		integralfloat(i,5) = parameters(i,8)
		integralfloat(i,6) = parameters(i,9)
		integralfloat(i,7) = parameters(i,10)
	END DO
	
	OPEN(4,file='timing.txt',action='write',status='replace')
	call system_clock(begin_time, rate)
	DO j=1,100
		OPEN(3,file='output.txt',action='write',status='replace')
		DO i=1,33777
			l1l2 = integralint(i,1)
			m1m2 = integralint(i,2)
			n1n2 = integralint(i,3)
			Cx = integralfloat(i,1)
			Cy = integralfloat(i,2)
			Cz = integralfloat(i,3)
			Px = integralfloat(i,4)
			Py = integralfloat(i,5)
			Pz = integralfloat(i,6)
			p = integralfloat(i,7)
			CALL r(l1l2, m1m2, n1n2, Cx, Cy, Cz, Px, Py, Pz, p, R1buffer, Rbuffer)
			WRITE(3,*) R1buffer(l1l2+1,m1m2+1,n1n2+1)
		END DO
		CLOSE(3)
	END DO
	call system_clock(end_time)
	WRITE(4,*)real(end_time-begin_time)/real(rate)
	CLOSE(4)
	
END PROGRAM runhi

