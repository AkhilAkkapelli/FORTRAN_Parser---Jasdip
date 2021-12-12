PROGRAM testLPSolver
USE LPPGeneration!, ONLY : LPGen
!USE LAPACKOperators, ONLY : POTRF, GEMV
USE LinearProblemSolver!, ONLY : LPType
!USE ScilabLP, ONLY : ScilabLPSolver
!USE Postprocessor!, ONLY : EigenAnalysis
!USE LAOperators!, ONLY : TRANS, SPDINV, ONES, GEMV, GEMM, UPPER, LOWER, SPDLUD
!USE HistogramPlot!, ONLY : Histogram
!USE Statistics!, ONLY : Arithmetic_Mean, Geometric_Mean, Harmonic_Mean
!USE GeoGebra!, ONLY : GGBPlot, GGBCommand, GGBPlotPoint, GGBInit
IMPLICIT NONE


!INTEGER,PARAMETER :: m = rowGGBPlotPoint
!INTEGER, PARAMETER :: n = column

!REAL(16) :: A(row,column), L(row,column), U(row,column), b(row), c(column)
!INTEGER :: i, j

!Ainv = POTRF(A)

!print*, real(Ainv)

!x = GEMV(Ainv, b)

!print*, "x", x

!L = CholeskyDecomposition(A)
!print*, "L", real(L)

!u = ForSubstitution(L, b)

!x = BackSubstitution(TRANSPOSE(L), u)

!print*, "x", x

!CHARACTER(len=32) :: arg

!i = 1
!  DO
!    CALL get_command_argument(i, arg)
!    IF (LEN_TRIM(arg) == 0) EXIT

!    WRITE (*,*) TRIM(arg)
!    i = i+1
!  END DO



!!!!Canonical form Example
!c = (/ 0D0, 2D0, -1D0 /)
!A = RESHAPE((/ 0.41D0, -0.82D0, 0.41D0 /), shape(A))


!!!Equality form Example
!c = (/ 3, -1, 1 /)
!A = RESHAPE((/ -8,1, 4,1, 1,1 /), shape(A))
!b = (/ 1,1 /)
!x0 = (/ 1./9, 3./9, 5./9 /)


!!!!Standard form Example Diagram
!c = (/ 3.,5./2,2.,7./2 /)
!A = RESHAPE((/ 1,1, 2,1, -3,1, 4,1 /), shape(A))
!b = (/ 0,1 /)

!!!!!Standard form Example Diagram 2
!c = (/ 3./2,5./4,1.,7./4 /)
!A = RESHAPE((/ 5,-3,1, -2,4,1, -3,-2,1, 4,1,1 /), shape(A))
!b = (/ 0,0,1 /)


!!!!Standard form Example
!c = (/ 3,9 /)
!A = RESHAPE((/ -1,1,-1, -3,1,1 /), shape(A))
!b = (/ -60,10,0 /)

!!!!Standard Form Example II
!c = (/ -5, -4, -3 /)
!A = RESHAPE((/ -2,-4,-3, -3,-1,-4, -1,-2,-2 /), shape(A))
!b = (/ -5, -11, -8 /)

!!!!Standard Form Example III
!c = (/ -5, -5, -3 /)
!A = RESHAPE((/ -1,1,-2,-2, -3,0,1,-3, -1,-3,-2,1 /), shape(A))
!b = (/ -3,-2,-4,-2 /)


!!!!Multiple Standard form Example
!c = (/ -3,-9 /)
!A = RESHAPE((/ -1,1,-1, -3,1,1 /), shape(A))
!b = (/ -60,10,0 /)


!!Unbounded Standard form Example
!c = (/ -1,1 /)
!A = RESHAPE((/ 2,1, -1,2 /), shape(A))
!b = (/ 1,2 /)

!!!Unbounded Standard form Example II
!c = (/ -1,-3,1 /)
!A = RESHAPE((/ -2,-3,-1, -2,2,3, 1,-1,-1 /), shape(A))
!b = (/ -10,-10,-10 /)

!!!!Unbounded Standard form Example III
!c = (/ -3,-1 /)
!A = RESHAPE((/ -1,1,-2, 1,1,1 /), shape(A))
!b = (/ 1,3,-2 /)


!!!Infeasible Standard form Example
!c = (/ -3,1 /)
!A = RESHAPE((/ -1,2, -1,2 /), shape(A))
!b = (/ -2,10 /)

!!!Infeasible Standard form Example II
!c = (/ -3,-1 /)
!A = RESHAPE((/ -1,1,-2, 1,1,-1 /), shape(A))
!b = (/ 1,3,-2 /)

!!!Infeasible Standard form Example III
!c = (/ 3,2 /)
!A = RESHAPE((/ 1,-3, 1,-5 /), shape(A))
!b = (/ 8,-15 /)

!DO i=1,100
!call random_number(p)
!END DO


!!!!! MATRIX GENERATION
!DO i=1,100

!#ifdef ProbDist 
!!CALL LPGen
!#endif

!!! LPSOLVER

!CALL GGBInit

!OPEN(unit = 10, file= './1.IO/A.txt')
!DO i=1,row
!READ(10, *) (A(i,j) ,j=1,column)
!END DO
!CLOSE(10)  
!CALL SPDLUD(A, L, U)
!print*, "A", A
!print*, "U", U
!print*, "L", L
!print*, "Ok",A-GEMM(L,U)
!!CALL LPType

!#ifdef EigenAnalyse
!!CALL EigenAnalysis
!#endif

!!CALL GGBPlot
!!END DO
!!!!POSTPROCESSING
!!CALL Analyze

!!END DO


!!IF(STAT .EQV. .TRUE.) THEN 
!!print*, "LP Solution is Converged"  
!!ELSE
!!print*, "LP Solution is not Converged"  
!!END IF

!!CALL Dual(A, b, c, A_dual, b_dual, c_dual)

!!x_dual = LPSolver(A_dual, b_dual, c_dual)
!!print*, 'x_dual', x_dual

!!x_sci = ScilabLPSolver(real(A), real(b), real(c))
!!print*, x_sci


END PROGRAM testLPSolver

MODULE ProbabilityDistribution
IMPLICIT NONE

!PRIVATE
!PUBLIC :: ProbDist

CONTAINS

FUNCTION UniformDist(seed) RESULT(u)

INTEGER(8) :: seed
INTEGER(8), PARAMETER :: IA=16807,IM=2147483647,IQ=127773,IR=2836
REAL(16), SAVE :: am
INTEGER(8), SAVE :: ix=-1,iy=-1,k

REAL(16) :: u


!!!Initialise
!IF (seed <= 0 .OR. iy < 0) THEN
!am=nearest(1.0,-1.0)/IM
!iy=ior(ieor(888889999,abs(seed)),1)
!ix=ieor(777755555,abs(seed))
!seed=abs(seed)+1
!END IF

!!!! Marsaglia shift sequence with period 2^32 -1
!ix=ieor(ix,ishft(ix,13))
!ix=ieor(ix,ishft(ix,-17))
!ix=ieor(ix,ishft(ix,5))

!!!! Park-Miller sequence by Schrage’s method with period2^31−2
!k=iy/IQ
!iy=IA*(iy-k*IQ)-IR*k
!if (iy < 0) iy=iy+IM
!!!! Combine two Generators
!u=am*ior(iand(IM,ieor(ix,iy)),1)

!u = (upperbound-lowerbound)*u + lowerbound

END FUNCTION UniformDist

FUNCTION NormalDist(seed) RESULT(n)

INTEGER(8) :: seed
REAL(16), PARAMETER :: PI = 4!.!Q0*DATAN(1.D0)
REAL(16) :: u1,u2

REAL(16) :: n


u1 =  Uniform(seed)
u2 =  Uniform(seed)

n = sqrt(-2*log(u1))*cos(2*PI*u2)

n = variance*n + mean

END FUNCTION NormalDist

END MODULE ProbabilityDistribution


MODULE ProbabilityGeneration
USE ProbabilityDistribution!, ONLY : ProbDist
IMPLICIT NONE

!PRIVATE
!PUBLIC :: ProbGen

INTERFACE ProbGen
	MODULE PROCEDURE ProbVecGen
	MODULE PROCEDURE ProbMatGen
END INTERFACE ProbGen

CONTAINS 

SUBROUTINE ProbVecGen(v)

REAL(16) :: v(:)
INTEGER(8) ::  seed = 54321, count
INTEGER :: i


!CALL SYSTEM_CLOCK(count)
!seed = MOD(count,100000)

DO i = 1, size(v)
	v(i) = ProbDist(seed)
END DO

END SUBROUTINE ProbVecGen

SUBROUTINE ProbMatGen(M)

REAL(16) :: M(:,:)
INTEGER(8) ::  seed = 54321, count
INTEGER :: i,j


!CALL SYSTEM_CLOCK(count)
!seed = MOD(count,100000)

DO i = 1, size(M,1)
DO j = 1, size(M,2)
	M(i,j) = ProbDist(seed)
END DO
END DO

END SUBROUTINE ProbMatGen

END MODULE ProbabilityGeneration

MODULE LPPGeneration
USE ProbabilityDistribution!, ONLY : ProbDist
IMPLICIT NONE

!PRIVATE
!PUBLIC :: LPGen

CONTAINS 

SUBROUTINE LPPGen()

REAL(16) :: A(row,column), b(row), c(column)
INTEGER(8) ::  seed = 123456, count
INTEGER :: i, j

!CALL SYSTEM_CLOCK(count)
!seed = MOD(count,10000)

!OPEN(unit = 10, file= './1.IO/A.txt')
!DO i = 1, row
!DO j = 1, column
!	A(i,j) = ProbDist(seed)
!	WRITE(10, *) A(i,j)
!END DO
!WRITE(10, *) ""
!END DO
!CLOSE(10) 

!OPEN(unit = 20, file= './1.IO/b.txt')
!DO i = 1, row
!	b(i) = ProbDist(seed)
!	WRITE(20, *) b(i)
!END DO
!CLOSE(20)

!OPEN(unit = 30, file= './1.IO/c.txt')
!DO i = 1, column
!	c(i) = ProbDist(seed)
!	WRITE(30, *) c(i)
!END DO
!CLOSE(30)

END SUBROUTINE LPPGen

END MODULE LPPGeneration


MODULE LinearProblemSolver
USE ProbabilityGeneration!, ONLY : ProbGen 
USE Algorithm!, ONLY : Karmarkar
USE LPTools!, ONLY : StdToCan, Transform, InvTransform, zero_stop, potential_stop, optimum_stop, zero_ratio, min_ratio
USE LAOperators!, ONLY : ONES, DOT, COLMULT, TRANS, GEMV, ENORM, SPDINV, GEMM
USE Augment!, ONLY : OPERATOR(.VAUG.), OPERATOR(.HAUG.)
USE GeoGebra!, ONLY : GGBPlotSystem
USE HistogramPlot!, ONLY : Histogram
USE PostProcessor!, ONLY : Stats
IMPLICIT NONE

!PRIVATE
!PUBLIC :: LPType

CONTAINS

SUBROUTINE CanonicalForm()

REAL(16) :: A(row, column), c(column), x_opt(column)
INTEGER :: i, j


!OPEN(unit = 10, file= './1.IO/A.txt')
!DO i=1,row
!READ(10, *) (A(i,j) ,j=1,column)
!END DO
!CLOSE(10) 

!OPEN(unit = 30, file= './1.IO/c.txt')
!DO j=1,column
!READ(30, *) c(j)
!END DO
!CLOSE(30)

!!open(unit=100,file="log.txt")

!!write(100,'(A,/)') "Canonical LP Problem:"
!!write(100,'(2X,A4,I3,5X,A4,I3,/)') "m = ", size(A,1),"n = ", size(A,2)

!!CALL GGBPlotSystem(A,c)

!!x_opt = Karmarkar(A, c)

!!close(100)

END SUBROUTINE CanonicalForm

SUBROUTINE EqualityForm()

REAL(16) :: A_eq(row, column), b_eq(row), c_eq(column), x0(column), x_eq(column), eps, &
			A_can(size(A_eq,1),size(A_eq,2)+1), c_can(size(c_eq)+1), x_can(size(A_eq,2)+1), &
			A_can1(size(A_eq,1),size(A_eq,2)+2), c_can1(size(c_eq)+2), x_can1(size(A_eq,2)+2), &
				A(row,column+1), b(row), c(column+1), x01(column+1), x_opt1(column+1), lambda
INTEGER :: i, j

!open(unit=100,file="log.txt")

!write(100,'(A20)') "Equality LP Problem:"
!write(100, *) ""

!write(100,'(2X,A4,I3,5X,A4,I3)') "m = ", size(A,1),"n = ", size(A,2)
!write(100, *) ""

!write(100, *) "A_eq"
!do i=1,size(A,1)
!     write(100, *) ( A(i,j) ,j=1,size(A,2))
!end do
!write(100,*) ""

!write(100, *) "b_eq"
!do i=1,size(b)
!     write(100,'(F15.7)') b(i)
!end do
!write(100,*) ""

!write(100, *) "c_eq"
!do i=1,size(c)
!     write(100,'(F15.7)') c(i)
!end do
!write(100,*) ""

!write(100, *) "x0"
!do i=1,size(x0)
!     write(100,'(F15.7)') x0(i)
!end do
!write(100,*) ""

!write(100,*) "Equality form converted to Canonical form:"
!write(100,*) ""

!OPEN(unit = 10, file= './1.IO/A.txt')
!DO i=1,row
!READ(10, *) (A_eq(i,j) ,j=1,column)
!END DO
!CLOSE(10)  

!OPEN(unit = 20, file= './1.IO/b.txt')
!DO i=1,row
!     READ(20, *) b_eq(i)
!END DO
!CLOSE(20)

!OPEN(unit = 30, file= './1.IO/c.txt')
!DO j=1,column
!     READ(30, *) c_eq(j)
!END DO
!CLOSE(30)

!!OPEN(unit = 40, file= './1.IO/x0.txt')
!!DO i=1,column
!!     READ(40, *) x0(i)
!!END DO
!!CLOSE(40)
!!!!PHASE I:

!CALL ProbGen(x0)


!!A_com = 

!!eps = real(1.Q-3, 16)
!!IF(ANY(ABS(GEMV(A_eq,x0)-b_eq) >= eps))  STOP "Initial Point doesn't lie in the Feasible Region."
!!write(100,*) ""

!	x01 = x0 .VAUG. real(1, 16)

!	b = b_eq + 0*(b_eq - GEMV(A_eq,x0))

!	A(:,:column) = A_eq
!	A(:,column+1) = 1*(b_eq - GEMV(A_eq,x0))

!	c = 0
!	c(column+1) = 1


!!OPEN(unit = 50, file= './1.IO/Result.txt')

!!WRITE(50,*) "A_initial:"
!!DO i=1,row
!!WRITE(50, *) (A_eq(i,j) ,j=1,column)
!!END DO


!!WRITE(50,*) "b_initial:"
!!DO i=1,row
!!WRITE(50, *) b_eq(i)
!!END DO

!!WRITE(50,*) "c_initial:"
!!DO j=1,column
!!WRITE(50, *) c_eq(j)
!!END DO

!!WRITE(50,*) "A:"
!!DO i=1,row
!!WRITE(50, *) (A(i,j) ,j=1,column+1)
!!END DO


!!WRITE(50,*) "b:"
!!DO i=1,row
!!WRITE(50, *) b(i)
!!END DO

!!WRITE(50,*) "c:"
!!DO j=1,column+1
!!WRITE(50, *) c(j)
!!END DO

!!WRITE(50,*) "Initial Residue:"
!!WRITE(50,*) GEMV(A,x01) -b

!!CALL GGBPlotSystem(A,c, b)

!CALL Transform(A,b,c, A_can1,c_can1, x01)

!!WRITE(50,*) "A:"
!!DO i=1,row
!!WRITE(50, *) (A_can(i,j) ,j=1,column+2)
!!END DO


!!WRITE(50,*) "c_can:"
!!DO j=1,column+2
!!WRITE(50, *) c_can(j)
!!END DO
!	
!!OPEN(unit = 50, file= './1.IO/A_can.txt')
!!DO i=1,row
!!WRITE(50, *) (A_can(i,j) ,j=1,column)
!!WRITE(50, *) ""
!!END DO
!!CLOSE(50)  

!!OPEN(unit = 60, file= './1.IO/c_can.txt')
!!DO j=1,column
!!WRITE(60, *) c_can(j)
!!END DO
!!CLOSE(60)
!!print*, "A", A
!!print*, "b", b
!!CLOSE(50)
!x_can1 = Karmarkar(A_can1, c_can1, zero_stop, zero_ratio)

!!IF(x_can1(size(x_can1)-1) < 1.Q-30) THEN

!!lambda = (2*x_p(n-1) - x_p(n))/(x(n) + 2*x_p(n-1) - x_p(n))

!!x_p = x_p + lambda*(x - x_p)
!!!!!print*, iter, real(x(n-1)), real(x(n-1)/x(n))!,  "x", real(x(n-1)/x(n))
!!!!!lambda = (5.Q-1 - (x_p(n-1)/x_p(n)))/((x(n-1)/x(n)) - (x_p(n-1)/x_p(n)))
!!!!!x(1:n-1) = x(n)*(x_p(1:n-1)/x_p(n) + lambda*(x(1:n-1)/x(n) - x_p(1:n-1)/x_p(n)))
!!!!!x(n) = 1 - SUM(x(1:n-1))

!!!EXIT
!!END IF

!!OPEN(unit = 50, file= './1.IO/Result.txt', access = 'append')
!!WRITE(50,*) "x_can:"
!!DO j=1,column+2
!!WRITE(50, *) x_can(j)
!!END DO

!!!print*,  "x_can", x_can
!!!print*, "error", GEMV(A_can,x_can)

!x_opt1 = InvTransform(x_can1, x01)



!!WRITE(50,*) "x_opt:"
!!DO j=1,column+1
!!WRITE(50, *) x_opt1(j)
!!END DO

!!WRITE(50,*) "Final Residue:"
!!WRITE(50,*) GEMV(A,x_opt1) -b
!!WRITE(50,*) "Error:"
!!WRITE(50,*) GEMV(A_eq,x_opt1(1:size(x_opt1)-1)) -b_eq

!!OPEN(unit = 60, file= './1.IO/x0.txt')
!!DO j=1,column
!!     READ(60, *) x0(j)
!!END DO
!!CLOSE(60)

!x0 = x_opt1(:column)
!!PRINT*, ENORM(GEMV(A_eq,x0) -b_eq)
!!CALL Transform(A_eq,b_eq,c_eq, A_can,c_can, x0)

!!x_can = Karmarkar(A_can, c_can, potential_stop, min_ratio)

!!x_eq = InvTransform(x_can, x0)

!!print*, 'error', DOT(c_can, x_can)

!!!!Optimum value check
!!IF( DOT(c_eq,x_eq) >= eps )	STOP "Wrong input arguments. Optimum value is not zero."
!!write(100, *) 'Optimum value check: c^T*x_opt = ',DOT(c,x_opt)
!!write(100, *) ""

!!write(100, *) "x_opt"
!!do i=1,size(x_opt)
!!     write(100,'(F19.17)') x_opt(i)
!!end do
!!write(100, *) ""

!close(100)
!CLOSE(50)
END SUBROUTINE EqualityForm

SUBROUTINE StandardForm()

REAL(16) :: A(row, column), b(row), c(column), x_opt(column), &
		A_can(size(A,1)+size(A,2)+1,2*(size(A,1)+size(A,2)+1)), c_can(2*(size(A,1)+size(A,2)+1)), &
		 x_can(2*(size(A,1)+size(A,2)+1)), x(2*(size(A,1)+size(A,2))+1), a0(2*(size(A,1)+size(A,2))+1)
INTEGER :: i, j


!open(unit=100,file="log.txt")

!write(100,'(A19)') "Standard LP Problem:"
!write(100,*) ""
!write(100,'(2X,A4,I3,5X,A4,I3)') "m = ", size(A,1),"n = ", size(A,2)
!write(100,*) ""

!write(100, *) "A_std"
!do i=1,size(A,1)
!     write(100, *) ( A(i,j) ,j=1,size(A,2))
!end do
!write(100,*) ""

!write(100, *) "b_std"
!do i=1,size(b)
!     write(100,'(F15.7)') b(i)
!end do
!write(100,*) ""

!write(100, *) "c_std"
!do i=1,size(c)
!     write(100,'(F15.7)') c(i)
!end do
!write(100,*) ""

!OPEN(unit = 10, file= './1.IO/A.txt')
!DO i=1,row
!READ(10, *) ( A(i,j) ,j=1,column)
!!READ(10, *) ""
!END DO
!CLOSE(10)

!OPEN(unit = 40, file= './1.IO/b.txt')
!DO i=1,row
!     READ(40, *) b(i)
!END DO
!CLOSE(40)

!OPEN(unit = 30, file= './1.IO/c.txt')
!DO j=1,column
!     READ(30, *) c(j)
!END DO
!CLOSE(30)

!!CALL GGBPlotSystem(A,c, b)

!CALL StdToCan(A,b,c, A_can,c_can, a0)

!!write(100,*) "Standard form converted to Canonical form:"
!!write(100,*) ""
!x_can = Karmarkar(A_can,c_can, optimum_stop)!, min_ratio)

!x = InvTransform(x_can, a0)

!!open(unit=10,file="log.txt",status='old',position='append',action='write')

!!write(100, *) "x"
!!do i=1,size(x)
!!     write(100,'(F19.17)') x(i)
!!end do
!!write(100,*) ""

!x_opt = x(:size(A,2))
!print*, GEMV(A, x_opt) - b
!!write(100, *) "x_opt"
!!do i=1,size(x_opt)
!!     write(100,'(F19.17)') x_opt(i)
!!end do
!!write(100,*) ""

!close(100)

END SUBROUTINE StandardForm

SUBROUTINE LeastNegativeForm()

REAL(16) :: A_ln(row, column), b_ln(row), c_ln(column), y0(row), x0(column), x_ln(column), eps, e(column), &
		A_aug(row,column+1), b_aug(row), c_aug(column+1), x0_aug(column+1), x_aug(column+1), &
			A_augcan(size(A_ln,1),size(A_ln,2)+2), c_augcan(size(c_ln)+2), x_augcan(size(A_ln,2)+2), &
				A_can(size(A_ln,1),size(A_ln,2)+1), c_can(size(c_ln)+1), x_can(size(A_ln,2)+1)
			
INTEGER :: i, j, opt=0, greater=0, num=0, feas=0, fail=0,ini_feas=0


!OPEN(unit = 10, file= './1.IO/A.txt')
!DO i=1,row
!READ(10, *) (A_ln(i,j) ,j=1,column)
!END DO
!CLOSE(10)  

!OPEN(unit = 20, file= './1.IO/b.txt')
!DO i=1,row
!     READ(20, *) b_ln(i)
!END DO
!CLOSE(20)

!!OPEN(unit = 30, file= './1.IO/c.txt')
!!DO j=1,column
!!     READ(30, *) c_ln(j)
!!END DO
!!CLOSE(30)

!!OPEN(unit = 40, file= './1.IO/x0.txt')
!!DO i=1,column
!!     READ(40, *) x0(i)
!!END DO
!!CLOSE(40)

!!PHASE I:

!!b_ln = 0.1*num-5
!!print*, "b", b_ln
!e=1

!y0 = SPDINV(GEMM(A_ln,TRANS(A_ln)), b_ln)

!x0 = GEMV(TRANS(A_ln), y0)
!print*, 'Solution Test', GEMV(A_ln, x0) - b_ln 


!IF(minval(x0) < 0) THEN

!eps = -2*minval(x0)

!A_aug = A_ln .HAUG. -GEMV(A_ln,e)

!b_aug = b_ln 

!x0_aug = (x0 + eps) .VAUG. eps

!c_aug = 0
!c_aug(column+1) = -1

!print*, 'Feasibility Test', GEMV(A_aug, x0_aug) - b_aug

!OPEN(unit = 40, file= './1.IO/x0.txt')
!DO i=1,column+1
!     WRITE(40, *) x0_aug(i)
!END DO
!CLOSE(40)

!CALL Transform(A_aug,b_aug,c_aug, A_augcan, c_augcan, x0_aug)

!x_augcan = Karmarkar(A_augcan, c_augcan, potential_stop, min_ratio)


!x_aug = InvTransform(x_augcan, x0_aug)
!!print*, "Feasibility Test2", ENORM(GEMV(A_aug,x_aug) - b_aug)
!!IF(x_aug(column+1) > 1.Q-10
!!CALL GGBPlotSystem(A_aug,c_aug, b_aug)
!!PRINT*, "Lower Bound is :", x0_aug(column+1), x_aug(column+1)

!!IF(x0_aug(column+1)>x_aug(column+1)) THEN
!!less=less+1
!!ELSE
!!greater = greater + 1
!!print*, 'x0', x_aug
!!IF(ENORM(GEMV(A_ln,x0)-b_ln)<1.Q-10) THEN
!!feas= feas+1
!!ELSE IF(x_aug(column+1)<eps)  THEN
!!x0=-x_aug(column+1)
!!END IF

!!END IF
!!print*, 'xaug', x_aug
!x0 = x_aug(:column)
!print*, "Feasibility Test2", ENORM(GEMV(A_ln,x0) - b_ln)
!x0 = -x_aug(column+1)
!print*, "Solution Test2", ENORM(GEMV(A_ln,x0) - b_ln)
!!IF(ENORM(GEMV(A_ln,x0)-b_ln)<1.Q-10) opt=opt+1
!!ELSE IF(x_aug(column+1)>eps)  THEN
!!fail=fail+1
!!!x_augcan = Karmarkar(A_augcan, -c_augcan, potential_stop)
!!!x_aug = InvTransform(x_augcan, x0_aug)
!!!IF(ENORM(GEMV(A_ln,x_aug(:column))-b_ln)<1.Q-10) THEN 
!!!feas=feas+1
!!!ELSE 
!!!x0= x_aug(column+1)
!!!IF(ENORM(GEMV(A_ln,-x0)-b_ln)<1.Q-10) opt=opt+1
!!!END IF
!!ENDIF
!!IF(x_aug(column+1)>eps) THEN 
!!x_augcan = Karmarkar(A_augcan, -c_augcan, potential_stop)
!!x_aug = InvTransform(x_augcan, x0_aug)
!!IF(ENORM(GEMV(A_ln,x_aug(:column))-b_ln)<1.Q-25) THEN 
!!feas = feas+1
!!ELSE 
!!x0= x_aug(column+1)
!!IF(ENORM(GEMV(A_ln,x0)-b_ln)<1.Q-25) THEN
!!opt = opt+1
!!ELSE
!!fail=fail+1
!!END
!!END IF
!!ELSE IF(ENORM(GEMV(A_ln,x_aug(:column))-b_ln)<1.Q-25) THEN
!!feas= feas+1
!!ELSE
!!x0= x_aug(column+1)
!!IF(ENORM(GEMV(A_ln,x0)-b_ln)<1.Q-25) opt = opt+1
!!END IF



!!IF(ENORM(x0)<1.Q-10) tot = tot+1
!!e = -x_aug(column+1)
!!print*, 'ok', 	x_aug(:column)
!!x0_aug = -x_aug(column+1)
!!x0_aug(column+1) = x_aug(column+1)

!!print*, "initial", eps,"final", x_aug(column+1)

!!print*, 'err', ENORM(GEMV(A_ln,e)-b_ln)
!!ELSE 
!! ini_feas = ini_feas +1
!END IF
!num = num+1
!!print*, "err", GEMV(A_ln,x0)-b_ln
!!x0 = -x_aug(column+1)
!!print*, "err", GEMV(A_ln,x0)-b_ln
!!print*, num, ini_feas
!!IF(ENORM(GEMV(A_ln,x0)-b_ln)<1.Q-25) opt = opt +1
!!print*, 'stat', num, ini_feas, opt, feas, fail
!!print*, less, greater, tot

!!CALL Transform(A_ln,b_ln,c_ln, A_can,c_can, x0)

!!x_can = Karmarkar(A_can, c_can, potential_stop, min_ratio)

!!x_ln = InvTransform(x_can, x0)

END SUBROUTINE LeastNegativeForm

!INCLUDE 'LPType/PrimalAffine.FOR'
!INCLUDE 'LPType/DualAffine.FOR'

END MODULE LinearProblemSolver

MODULE Algorithm
USE Augment!, ONLY : OPERATOR(.VAUG.)
USE LAPACKOperators!, ONLY : DPOINV, LAGEMV
USE LAOperators!, ONLY : DIAG, DOT, ENORM, GEMV, GEMM, TRANS, SPDINV, COLMULT
IMPLICIT NONE


!PUBLIC :: Karmarkar
!PRIVATE

CONTAINS


FUNCTION ProjectiveMethod(A,c) RESULT(x_can)

REAL(16), INTENT(IN) :: A(:,:), c(:)
!LOGICAL, EXTERNAL :: stop_cond
!REAL(16), EXTERNAL, OPTIONAL :: ratio_test
REAL(16) :: x_can(size(c))

REAL(16) :: x_p(size(A,2)), x(size(A,2)), f_p, f, eps, x0_aug(column+1), x_aug(column+1)
INTEGER :: n, iter, iter_limit, i, j, iter_num=1, iterz = 0

!!inquire(file="log.txt",opened=fstat)
!!if(fstat) then
!!	open(unit=10,action='write',access='append')
!!else
!!	open(unit=10,file="log.txt",action='write')
!!end if	

!!write(10, *) "A_can"
!!do i=1,size(A,1)
!!     write(10, *) ( A(i,j) ,j=1,size(A,2))
!!end do
!!write(10, *) ""

!!write(10, *) "c_can"
!!do i=1,size(c)
!!     write(10,'(F15.7)') c(i)
!!end do
!!write(10, *) ""

!OPEN(unit = 40, file= './1.IO/x0.txt')
!DO i=1,column+1
!     READ(40, *) x0_aug(i)
!END DO
!CLOSE(40)

!!!!Dimension check
!IF(size(A,2) /= size(c)) STOP "Algorithm ERROR: Wrong size for input argument"

!!!! STEP 0 : Initialize
!n = size(A,2)
!x_p = real(1.Q0, 16)/n

!x_aug = InvTransform(x_p, x0_aug)

!!CALL GGBPlotPoint(x_aug,"Initial", "FF0000")
!!!!Feasibility check
!eps = real(1.Q-2, 16)
!IF( ANY(GEMV(A,x_p) >= eps) ) STOP "Center of Simplex doesn't lie in Null space of the Input Matrix:"
!!write(10,*) "Feasibility check: A*x_initial = ", MAXVAL(GEMV(A,x_p))
!!write(10,*) ""

!iter = 1
!iter_limit = 1000


!!OPEN(unit = 50, file= './1.IO/Result.txt')
!!WRITE(50,*) "Initial point:", real(x_p)
!!print*, "lambda", iter, x_p(size(x_p)-1)/x_p(size(x_p))
!!write(10,'(A5,A15,A18,A15,/)') "Iter ", "       X Value ", "   Potential Diff ", "  Optimum Value"
!print*, 0,  real(x_p(n-1)), real(x_aug(n-1))
!DO WHILE(iter <= iter_limit)

!!!!STEP 1 : Compute the next point in the sequence
!CALL Optimize(x_p, x)
!!print*, iter, x(n-1), x_aug(n-1)

!x_aug = InvTransform(x, x0_aug)
!!e = -x_aug(column+1)
!!print*, 'err', ENORM(GEMV(A_aug,e)-b_aug)
!!IF(x(n-1) < 1.Q-20) THEN
!!iterz = iterz +1
!!EXIT
!!!lambda = (2*x_p(n-1) - x_p(n))/(x(n) + 2*x_p(n-1) - x_p(n))

!!!x = x_p + lambda*(x - x_p)
!!END IF

!!IF(x(n-1) < 1.Q-30) iterz = iterz +1
!	print*, iter, real(x(n-1)), real(x_aug(n-1))
!!write(str, *) int(iter), x_aug(n-1)

!CALL GGBPlotPoint(x_aug, "P"//TRIM(ADJUSTL(str)))

!IF(stop_cond(n, x,x_p, iter, c)) EXIT

!!OPEN(unit = 10, file= './1.IO/x0.txt')
!!READ(10, *) x0 
!!write(str, *) int(iter)
!!	print*, real(x_p(n-1)), real(x(n-1))!iter, x(n-1), real(x(n-1)/x(n))
!!iter_num = iter_num + 1

!!print*, "x", x(n-1)
!!lambda =  - x_p(n-1)/(x(n-1) - x_p(n-1))
!!x = x_p + lambda*(x - x_p)

!	!print*, "lambda", iter+1, x(size(x_p)-1)/x(size(x_p))
!	!print*, 'std', x(size(x)-1)


!!IF(x(size(x)-1)/x(size(x)) - real(1Q0, 16)*21Q-4) PRINT*, 'lambda', iter, x(size(x)-1)/x(size(x))
!!!!STEP 2 : Check for Infeasibility
!!	f_p = f
!!	f =  Potential(c,x)
!!print*, "err", GEMV(A,x)
!	

!!x_eq  = InvTransform(x,x0)
!!print*, iter, x_eq
!!IF(x_eq(column) < real(0.50, 16)) THEN
!!x_eqp = InvTransform(x_p,x0)
!!lambda =  (real(0.50, 16) - x_eqp(column))/(x_eq(column) - x_eqp(column))
!!x_eq = x_eqp + lambda*(x_eq - x_eqp)
!!x_p(:size(x_p)-1) = x_eq/(SUM(x_eq)+1)
!!x_p(size(x_p)) = 1 - SUM(x_p(:size(x_p)-1))
!!EXIT
!!END IF


!IF( ANY(ISNAN(x))) THEN
!PRINT*,  "WARNING: NAN occurred in the Solution"
!x= x_p
!EXIT 
!END IF

!x_p = x
!iter = iter + 1

!!write(10,'(I5,3F15.7)') iter, x(1), f-f_p, DOT(c,x)
!!do i=2,size(x)
!!     write(10,'(5X,F15.7)') x(i)
!!end do
!!write(10,*) ""
!!print*, "DOT", iter, DOT(c,x)
!!PAUSE "OK"
!END DO

!iter_num = iter_num+1	
!!write(10,'(A,I7,5X,A,F15.7/)')  'Total iterations: ',iter, 'Final Potential Difference: ',f-f_p
!!iter_num = iter_num + iter

!!!!Optimum value check
!!IF( DOT(c,x) >= eps )	STOP "Wrong input arguments. Optimum value is not zero."
!!write(10, *) 'Optimum value check: c^T*x_can = ',DOT(c,x_p)
!!write(10, *) ""

!x_can = x

!!x_eq = InvTransform(x_can,x0)
!!CALL GGBPlotPoint(x_eq,"Solution","008000")
!!write(10, *) "x_can"
!!do i=1,size(x_can)
!!     write(10,'(F19.17)') x_can(i)
!!end do
!!write(10,*) ""
!!close(50)
!!close(10)

CONTAINS 

SUBROUTINE Optimize(x_p, x)

REAL(16), INTENT(IN) :: x_p(:)
REAL(16) :: x(size(x_p))

REAL(16) :: e(size(x_p)), Ad(size(A,1),size(x_p)), B(size(A,1)+1,size(x_p)), &
		v(size(A,1)+1), c_p(size(c)), c_unit(size(c)), x0(size(x_p)), alpha


!e = ukind(1.Q0)
!x0 = real(1.Q0, 16)/n


!Ad = COLMULT(x_p,A)

!B = Ad .VAUG. e


!!c_p = GEMV((1 - GEMM(TRANSPOSE(B),GEMM(DPOINV(GEMM(B, TRANSPOSE(B))),B))),x_p*c)

!v = SPDINV(GEMM(B,TRANS(B)), GEMV(B,x_p*c))

!!print*, 'Matrix inversion Test', ENORM(GEMV(GEMM(B,TRANS(B)), v) - GEMV(B,x_p*c))
!c_p = x_p*c - GEMV(TRANS(B),v)

!!print*, "SUM Test", SUM(c_p)

!CALL EigenRecord(GEMM(B,TRANS(B)))

!c_unit = c_p/ENORM(c_p)
!!print*, 'proj_dir', c_unit

!!print*, 'verify_ratiotest', c_unit(n-1) >0
!!print*, 'Simplex test', SUM(c_unit)

!IF(PRESENT(ratio_test)) THEN
!IF(c_unit(n-1) > 0) alpha = ratio_test(n, c_unit)
!ELSE
!alpha = ukind(0.25Q0)*(1/sqrt(ukind(n)*(ukind(n)-1)))
!END IF

!!x_aug = InvTransform(c_unit, x0_aug)
!!CALL GGBPlotVector(alpha*x_aug)
!!x_aug = InvTransform(x0, x0_aug)
!!CALL GGBPlotVector(x_aug)

!x = x0 - alpha*c_unit
!!print*, 'b', x 
!!print*, 'err',GEMV(A,c_unit)!alpha!ACOSD(DOT(c_unit, c/ENORM(c)))

!x = x_p*x/DOT(x_p,x)
!!print*, 'x', x
END SUBROUTINE Optimize

END FUNCTION ProjectiveMethod

!SUBROUTINE PrimalAffineScaling(A,b,c, x0,  x_pri,x_dual)

!REAL(16), INTENT(IN) :: A(:,:), b(:), c(:), x0(:)
!REAL(16), INTENT(OUT) :: x_pri(size(A,2)), x_dual(size(A,1))

!INTEGER, PARAMETER :: iter_limit = 10000 
!INTEGER :: iter
!REAL(16), PARAMETER :: beta = ukind(99.Q-2)
!REAL(16) :: eps, x_p(size(A,2)), x(size(A,2)), y(size(A,1)), Ax(size(A,1),size(A,2)), rc(size(A,2)), dy(size(A,1)), alpha


!eps = ukind(1.Q-30)!!!PREDEFINE AS PREPROCESSOR VARIABLE

!IF(size(A,2) /= size(c) .OR. size(A,1) /= size(b) .OR. size(x0) /= size(c))  STOP "PRIMALAFFINESCALING ERROR: Invalid Input" 
!IF(GEMV(A,x0)-b > eps)  STOP "PRIMALAFFINESCALING ERROR: Initial Value should satisfy the Equations"

!iter = 1
!x_p = x0

!DO WHILE(iter <= iter_limit)

!  Ax = COLMULT(x_p,A)
!  y = PDINV(GEMM(Ax,TRANS(Ax)), GEMV(Ax,x_p*c))
!  rc = c - GEMV(TRANS(A),y)
!  
!  IF(rc >= 0 .AND. DOT(x_p,rc) < eps) EXIT!!!IS rc>=0 necessary
!  
!  dy = -x_p*rc
!  
!  IF(ALL(dy > 0)) STOP "PRIMALAFFINESCALING ERROR: Solution is Unbounded"

!  alpha = -beta/minval(dy)
!  x = x_p + alpha*(x_p*dy)

!  x_p = x
!  iter = iter +1

!END DO

!x_pri = x
!x_dual = y

!IF(GEMV(A,x_pri)-b > eps)  STOP "PRIMALAFFINESCALING ERROR: Primal Solution should satisfy the Equations"
!IF(GEMV(TRANS(A),x_dual) < b)  STOP "PRIMALAFFINESCALING ERROR: Dual Solution should satisfy the Equations"
!IF(DOT(c,x) - DOT(b,x_dual) > eps)  STOP "PRIMALAFFINESCALING ERROR: Solution should satisfy strong duality condition"

!END SUBROUTINE PrimalAffineScaling


!SUBROUTINE DualAffineScaling(A,b,c, y0,z0, y_dual,z_dual,x_pri)

!REAL(16), INTENT(IN) :: A(:,:), b(:), c(:), y0(:), z0(:)
!REAL(16), INTENT(OUT) :: y_dual(size(A,1)), z_dual(size(A,2)), x_pri(size(A,2))

!INTEGER, PARAMETER :: iter_limit = 10000 
!INTEGER :: iter
!REAL(16), PARAMETER :: beta = ukind(99.Q-2)
!REAL(16) :: eps, y(size(y_dual)), z(size(z_dual)), x(size(A,2)), y_p(size(y_dual)), z_p(size(z_dual)), dy(size(y_dual)), dz(size(z_dual)), alpha


!eps = ukind(1.Q-30)!!!PREDEFINE AS PREPROCESSOR VARIABLE

!IF(size(A,2) /= size(c) .OR. size(A,1) /= size(b))  STOP "DUALAFFINESCALING ERROR: Invalid Input"
!IF(size(y0) /= size(A,1) .OR. size(z0) /= size(A,2))  STOP "DUALAFFINESCALING ERROR: Invalid Input" 
!IF(GEMV(TRANS(A),y0) + z0 - c > eps)  STOP "DUALAFFINESCALING ERROR: Initial Value should satisfy the Equations"

!iter = 1
!y_p = y0
!z_p = z0

!DO WHILE(iter <= iter_limit)

!  Ay = COLMULT(y_p,A)
!  dy = PDINV(GEMM(Ay,TRANS(Ay)), b)
!  dz = -GEMV(TRANS(A), dy)
!  
!  IF(ALL(dz > 0)) STOP "DUALAFFINESCALING ERROR: Solution is Unbounded"!!!IS dz=0 needed?
!  
!  x = -y_p*y_p*dy
!  
!  IF(ALL(x >= 0) .AND. DOT(c,x)-DOT(b,y) < eps)  EXIT
!  
!  i = minloc(dy)
!  alpha = -beta*y(i)/dy(i)
!  y = y_p + alpha*dy
!  z = z_p + alpha*dz

!  y_p = y
!  z_p = z
!  iter = iter +1

!END DO

!y_dual = y
!z_dual = z
!x_pri = x

!IF(GEMV(A,x_pri)-b > eps)  STOP "AFFINESCALING ERROR: Primal Solution should satisfy the Equations"
!IF(GEMV(TRANS(A),x_dual) < b)  STOP "AFFINESCALING ERROR: Dual Solution should satisfy the Equations"
!IF(DOT(c,x) - DOT(b,x_dual) > eps)  STOP "AFFINESCALING ERROR: Solution should satisfy strong duality condition"

!END SUBROUTINE DualAffineScaling

END MODULE Algorithm

MODULE Statistics
IMPLICIT NONE

!PRIVATE
!PUBLIC :: Arithmetic_Mean, Geometric_Mean, Harmonic_Mean

INTERFACE Arithmetic_Mean
	MODULE PROCEDURE arithmetic_vector
	MODULE PROCEDURE arithmetic_matrix
END INTERFACE Arithmetic_Mean

INTERFACE Geometric_Mean
	MODULE PROCEDURE geometric_vector
	MODULE PROCEDURE geometric_matrix
END INTERFACE Geometric_Mean

INTERFACE Harmonic_Mean
	MODULE PROCEDURE harmonic_vector
	MODULE PROCEDURE harmonic_matrix
END INTERFACE Harmonic_Mean

CONTAINS

FUNCTION arithmetic_vector(vector) RESULT(amean)

REAL(16), INTENT(IN) :: vector(:)
REAL(16) :: amean

amean = SUM(vector)/size(vector)

END FUNCTION arithmetic_vector

FUNCTION arithmetic_matrix(Matrix) RESULT(amean)

REAL(16), INTENT(IN) :: Matrix(:,:)
REAL(16) :: amean

amean = arithmetic_vector(RESHAPE(Matrix,(/size(Matrix)/)))

END FUNCTION arithmetic_matrix


FUNCTION geometric_vector(vector) RESULT(gmean)

REAL(16), INTENT(IN) :: vector(:)
REAL(16) :: gmean

!IF(ANY(vector<0)) STOP "GEOMETRIC MEAN ERROR: Invalid Input"

gmean = EXP(arithmetic_vector(LOG(vector)))

END FUNCTION geometric_vector

FUNCTION geometric_matrix(Matrix) RESULT(gmean)

REAL(16), INTENT(IN) :: Matrix(:,:)
REAL(16) :: gmean

gmean = geometric_vector(RESHAPE(Matrix,(/size(Matrix)/)))

END FUNCTION geometric_matrix


FUNCTION harmonic_vector(vector) RESULT(hmean)

REAL(16), INTENT(IN) :: vector(:)
REAL(16) :: hmean

!IF(ANY(vector==0)) STOP "HARMONIC MEAN ERROR: Invalid Input"

hmean = size(vector)/SUM(1/vector)

END FUNCTION harmonic_vector

FUNCTION harmonic_matrix(Matrix) RESULT(hmean)

REAL(16), INTENT(IN) :: Matrix(:,:)
REAL(16) :: hmean

hmean = harmonic_vector(RESHAPE(Matrix,(/size(Matrix)/)))

END FUNCTION harmonic_matrix

END MODULE Statistics

MODULE HistogramPlot
IMPLICIT NONE

!PRIVATE
!PUBLIC :: Histogram, Plot

INTERFACE Histogram
	MODULE PROCEDURE histogram_vector
	MODULE PROCEDURE histogram_matrix
END INTERFACE Histogram

CONTAINS

SUBROUTINE histogram_vector(vector)

REAL(16), INTENT(IN) :: vector(:)

INTEGER(8) :: i, s, n = 0
REAL(16) :: limit, interval
CHARACTER(len = 6) :: lowerbound,upperbound
CHARACTER(len = 16) :: filename
REAL(16), ALLOCATABLE :: group(:,:)
INTEGER(8), ALLOCATABLE :: p(:)


!n = n + 1
!s = int(SQRT(real(size(vector))))
!limit = MAXVAL(vector) - MINVAL(vector)

!IF( mod(s,2) == 1) s = s+1

!interval = limit/s

!ALLOCATE(group(3,s),p(s))

!group(1,:) = (/ (MINVAL(vector) + interval*i, i=0,size(group,2)-1) /)
!group(2,:) = group(1,:) + interval
!group(3,:) = 0
!p = int((vector(:)-MINVAL(vector))/interval)+1
!group(3,p) = group(3,p) + 1
!group(3,s) = group(3,s)+1

!write (filename,"(A9,I0.3,A4)") "Histogram",n,".csv"

!open( 25, file = './4.Analysis/EigenAnalysis/'//filename,action='write')

!write( 25,'(A3,A,A5)' ) 'Bin',',','Count'

!do i = 1, size(group,2)
!	write(lowerbound,'(F6.2)') group(1,i)
!	write(upperbound,'(F6.2)') group(2,i)
!	write(25, '(A6,A4,A6,A,I10)' ) lowerbound,' to ',upperbound,',',INT(group(3,i))
!enddo

!close(25)

!CALL SYSTEM('cd 4.Analysis/EigenAnalysis/;./Histogram.py')

!DEALLOCATE(group,p)

END SUBROUTINE histogram_vector

SUBROUTINE histogram_matrix(Matrix)

REAL(16), INTENT(IN) :: Matrix(:,:)

!CALL histogram_vector(RESHAPE(Matrix,(/size(Matrix)/)))

END SUBROUTINE histogram_matrix


SUBROUTINE Plot(Y, Z)

REAL(16) :: Y(:)
REAL(16) :: Z(:)

REAL(16) :: X(size(Y))
INTEGER :: i


!IF(.NOT. PRESENT(Z)) THEN
!	DO i = 1,size(Y)
!		X(i) = i
!	END DO
!ELSE
!	IF(size(X) /= size(Y)) STOP "PLOT ERROR: Invalid Input Size"
!	X = Z
!END IF

!OPEN(unit = 100, file= './4.Analysis/EigenAnalysis/Cond.csv')
!DO i=1,size(Y)
!     WRITE(100, *) X(i),",", Y(i)
!END DO
!CLOSE(100)

!CALL SYSTEM('cd 4.Analysis/EigenAnalysis/;./Cond.py')

END SUBROUTINE Plot


END MODULE HistogramPlot

MODULE Postprocessor
!USE LinearProblemSolver, ONLY : LPSolver
!!USE LPTools, ONLY :: Dual
!!USE ScilabLP, ONLY : ScilabLPSolver
!USE LAOperators, ONLY :: DOT
USE LAPACKOperators!, ONLY : EIGEN
USE HistogramPlot!, ONLY : Histogram, Plot
IMPLICIT NONE

!PRIVATE
!PUBLIC :: EigenRecord, EigenAnalysis, Stats

INTERFACE Stats
	MODULE PROCEDURE Stats_vector
	MODULE PROCEDURE Stats_matrix
END INTERFACE Stats

CONTAINS

SUBROUTINE Stats_vector(vector, M, StdDev)

REAL(16), INTENT(IN) :: vector(:)
REAL(16), INTENT(OUT) :: M, StdDev 

REAL(16) :: s, ssq
INTEGER :: i, n


!n = size(vector)
!s = 0
!ssq = 0 

!DO i=1,n
!s = s + vector(i)
!ssq = ssq + vector(i)*vector(i)
!END DO

!M = s/n
!StdDev = SQRT((ssq - s*s/n)/(n-1))

END SUBROUTINE Stats_vector

SUBROUTINE Stats_matrix(Matrix, M, StdDev)

REAL(16), INTENT(IN) :: Matrix(:,:)
REAL(16), INTENT(OUT) :: M, StdDev 

!CALL Stats_vector(RESHAPE(Matrix,(/size(Matrix)/)), M, StdDev)

END SUBROUTINE Stats_matrix

SUBROUTINE EIGENRECORD(M)


REAL(16), INTENT(IN) :: M(:,:)

REAL(16) :: E(size(M,1)), COND
INTEGER :: i=1

!IF( SIZE(M,1) /= SIZE(M,2) ) STOP "EIGENANALYZE ERROR: Invalid input dimensions" 

!E = EIGEN(real(M,8))
!COND = MAXVAL(E)/MINVAL	(E)

!OPEN(100, FILE = './4.Analysis/EigenAnalysis/Eigen.txt', POSITION='append')
!WRITE(100,*) E
!WRITE(100,*) ""
!CLOSE(100)

!OPEN(200, FILE = './4.Analysis/EigenAnalysis/Cond.txt', POSITION='append')
!WRITE(200,*) COND
!CLOSE(200)

END SUBROUTINE EIGENRECORD

SUBROUTINE EigenAnalysis()

REAL(16) :: E(row+1,50), COND(50)
INTEGER :: IOstatus, i, n

!COND = 0
!OPEN(100, FILE = './4.Analysis/EigenAnalysis/Cond.txt')
!READ(100,*, IOSTAT=IOstatus) COND
!CLOSE(100)

!E = 0
!OPEN(200, FILE = './4.Analysis/EigenAnalysis/Eigen.txt')
!DO i = 1,50
!READ(200,*, IOSTAT=IOstatus) E(:row+1,i)
!IF(IOstatus<0) EXIT
!END DO
!CLOSE(200)

!n = i-1

!DO i = 1,n
!CALL Histogram(E(:,i))
!END DO

!CALL PLOT(COND(:n))

!CALL SYSTEM("rm 4.Analysis/EigenAnalysis/*.txt 4.Analysis/EigenAnalysis/*.csv")

END SUBROUTINE EigenAnalysis


!SUBROUTINE Analyze(A,c, x_opt, STAT, b, x0)

!REAL(16), INTENT(IN) :: A(:,:), c(:), x_opt(:)
!LOGICAL, INTENT(INOUT) :: STAT
!REAL(16), OPTIONAL :: b(:), x0(:)

!REAL(16) :: x_sci(size(x_opt)), A_dual(size(A,2),size(A,1)), b_dual(size(c)), c_dual(size(A,1)), x_dual(size(A,1)), eps, error
!INTEGER :: i, j, cnv=0
!REAL(16) ::  tot_error=0


!IF(.NOT. PRESENT(b) .OR. (PRESENT(b) .AND. PRESENT(x0))) THEN

!!	open (100, file = 'log.txt',status="old", access="append")

!	error = DOT(c,x_opt)
!	eps = 1.D-3

!	IF(ABS(error) < eps ) THEN
!		STAT = .TRUE.
!	ELSE
!		STAT = .FALSE.
!	END IF

!!	write(100, *) "Optimum value of LP: ", DOT(x_opt,c)
!!	close(100)

!ELSE
!	
!!	open (100, file = 'log.txt',status="old", access="append")

!	CALL Dual(A, b, c, A_dual, b_dual, c_dual)

!	x_dual = LPSolver(A_dual,c_dual, b_dual)
!	
!!	x_sci = ScilabLPSolver(A, b, c)


!!	IF(ANY(x_sci == -HUGE(x_sci))) THEN 
!!	write(100, *) "Scilab Solution is not Converged"
!!	write(100,*) ""
!!		PRINT*, "Scilab Solution is not Converged"	
!!	ELSE
!!	write(100, *) "x_sci"
!!	do i=1,size(x_sci)
!!	write(100,'(F15.7)') x_sci(i)
!!	end do
!!	write(100,*) ""
!!	write(100, *) "Optimum value of Scilab LP: ", DOT(x_sci,c)
!!		PRINT*, "Scilab Solution is Converged"	!
!!	END IF		

!	error = (ABS(DOT(c,x_opt)+DOT(c_dual,x_dual))*100)/ABS(DOT(c,x_opt)-DOT(c_dual,x_dual))


!!	write(100, *) "Optimum value of LP: ", DOT(c,x_opt)
!! 	write(100, *) "Optimum value of Dual LP: ", -DOT(c_dual,x_dual)

!!	write(100,*) "Relative error in Optimum Value: ", ABS(DOT(c,x_opt)+DOT(c_dual,x_dual))/2
!!	write(100,*) "Relative error percentage in Optimum Value: ", error

!	eps = 1.D-08

!	IF( ABS(DOT(c,x_opt)+DOT(c_dual,x_dual))/2 < eps ) THEN
!		cnv = cnv + 1
!		tot_error = tot_error+error
!		write(100,*) cnv
!		write(100,*) tot_error
!		STAT = .TRUE.
!	ELSE
!		STAT = .FALSE.	
!	END IF

!!	close(100)
!END IF

!END SUBROUTINE Analyze

END MODULE Postprocessor

MODULE LPTools
USE Augment!, ONLY : OPERATOR(.VAUG.)
USE ProbabilityGeneration!, ONLY : ProbGen
USE LAOperators!, ONLY : ONES, DOT, ColMult, GEMV, ENORM, TRANS
IMPLICIT NONE

!PRIVATE
!PUBLIC :: Dual, StdToCan, Transform, InvTransform, Potential, zero_stop, potential_stop, optimum_stop, zero_ratio, min_ratio

CONTAINS

FUNCTION Potential(c,x) RESULT(f)

REAL(16), INTENT(IN) :: c(column+1) , x(column+1)
REAL(16) :: f


!f = SUM(LOG(DOT(c,x)/x))

END FUNCTION Potential

!FUNCTION zero_stop(n, x,x_p, iter, c) RESULT(stp)

!INTEGER, INTENT(IN) :: iter, n
!REAL(16), INTENT(IN) :: x(n), x_p(n), c(n)
!LOGICAL :: stp

!INTEGER :: iter_limit


!!iter_limit = 500

!!stp = .FALSE.

!!IF(iter >= iter_limit .OR. x(n-1) < real(1.Q-30,16) .OR. x(n-1) > x_p(n-1)) stp = .TRUE.

!END FUNCTION zero_stop

!SFUNCTION potential_stop(n, x,x_p, iter, c) RESULT(stp)

!INTEGER, INTENT(IN) :: iter, n
!REAL(16), INTENT(IN) :: x(n), x_p(n), c(n)
!LOGICAL :: stp

!INTEGER :: iter_limit
!REAL(16) :: delta, f, f_p


!!iter_limit = 10000

!!delta = real(1.Q0,16)/8
!!f_p = Potential(c,x_p)
!!f =  Potential(c,x)

!!stp = .FALSE.

!!IF(iter >= iter_limit ) stp = .TRUE.

!END FUNCTION potential_stop

!FUNCTION optimum_stop(n, x,x_p, iter, c) RESULT(stp)

!INTEGER, INTENT(IN) :: iter, n
!REAL(16), INTENT(IN) :: x(n), x_p(n), c(n)
!LOGICAL :: stp

!INTEGER :: iter_limit
!REAL(16) :: obj


!!iter_limit = 1000

!!obj = DOT(c,x)

!!stp = .FALSE.

!!IF(iter >= iter_limit .OR. obj < real(1.Q-30,16)) stp = .TRUE.

!END FUNCTION optimum_stop

FUNCTION zero_ratio(n, c_unit) RESULT(alpha)

INTEGER, INTENT(IN) :: n
REAL(16), INTENT(IN) :: c_unit(n)
REAL(16) :: alpha, a

REAL(16) :: beta
INTEGER :: idx 


!beta = ukind(1.Q-1)
!alpha = real(1.Q0,16)/(n*c_unit(n-1))
!DO idx=1,n
!IF(c_unit(idx)<0 .OR. idx == n-1) CYCLE
!a = (ukind(1.Q0)-beta)/(n*c_unit(idx))
!IF(alpha>a) alpha = a
!END DO

END FUNCTION zero_ratio

FUNCTION min_ratio(n, c_unit) RESULT(alpha)

INTEGER, INTENT(IN) :: n
REAL(16), INTENT(IN) :: c_unit(n)
REAL(16) :: alpha

REAL(16) :: beta
INTEGER :: idx 


!beta = real(1.Q-1,16)
!alpha = (ukind(1.Q0)-beta)/(n*maxval(c_unit))

END FUNCTION min_ratio


SUBROUTINE Dual(A, b, c, A_dual, b_dual, c_dual)

REAL(16), INTENT(IN) :: A(:,:), b(:), c(:)

REAL(16), INTENT(OUT) :: A_dual(size(A,2),size(A,1)), b_dual(size(c)), c_dual(size(b))


!c_dual = -b

!A_dual = -TRANSPOSE(A)

!b_dual = -c

END SUBROUTINE Dual

SUBROUTINE StdToCan(A_std,b_std,c_std ,A_can,c_can, a0)

REAL(16), INTENT(IN) :: A_std(:,:), b_std(:), c_std(:)
REAL(16), INTENT(OUT) :: A_can(size(A_std,1)+size(A_std,2)+1,2*(size(A_std,1)+size(A_std,2)+1)), &
				 c_can(2*(size(A_std,1)+size(A_std,2)+1)), a0(2*(size(c_std)+size(b_std))+1)

REAL(16) :: A(size(A_std,1)+size(A_std,2)+1,2*(size(A_std,1)+size(A_std,2))+1), &
			b(size(A_std,1)+size(A_std,2)+1),c(2*(size(A_std,1)+size(A_std,2))+1)
REAL(16) :: x0(size(A_std,2)), y0(size(A_std,1)), u0(size(A_std,1)), v0(size(A_std,2)), lambda_0

INTEGER :: m, n, i,j


!m = size(A_std,1); n = size(A_std,2)

!CALL ProbGen(x0)
!CALL ProbGen(y0)
!CALL ProbGen(u0)
!CALL ProbGen(v0)
!lambda_0 = 1

!a0 = x0 .VAUG. y0 .VAUG. u0 .VAUG. v0 .VAUG. lambda_0


!A = 0

!A(1:m,1:n) = A_std
!A(1:m,n+1:n+m) = -Ones(m)

!A(m+1:m+n,m+n+1:2*m+n) = TRANS(A_std)
!A(m+1:m+n,2*m+n+1:2*(m+n)) = Ones(n)

!A(m+n+1,1:n) = c_std
!A(m+n+1,m+n+1:2*m+n) = -b_std

!A(1:m,2*(m+n)+1) =  b_std - GEMV(A_std,x0) + y0 
!A(m+1:m+n,2*(m+n)+1) = c_std - GEMV(TRANS(A_std),u0) - v0 
!A(m+n+1,2*(m+n)+1) = -DOT(c_std,x0) + DOT(b_std,u0) 


!b(:m) = b_std 
!b(m+1:m+n) = c_std
!b(m+n+1) = 0


!c = 0
!c(2*m+2*n+1) = 1

!CALL Transform(A,b,c, A_can,c_can, a0)

END SUBROUTINE StdToCan


SUBROUTINE Transform(A,b,c, A_can,c_can, a0)

REAL(16), INTENT(IN) :: A(:,:),b(:),c(:), a0(:)

REAL(16), INTENT(OUT) :: A_can(size(A,1),size(A,2)+1), c_can(size(c)+1)


!A_can(:,:size(A,2)) = ColMult(a0,A)
!A_can(:,size(A,2)+1) = -b

!c_can(:size(c)) = a0*c
!c_can(size(c)+1) =  0

END SUBROUTINE Transform

FUNCTION InvTransform(x_can,x0) RESULT(x)
	
REAL(16), INTENT(IN) ::  x_can(:), x0(:)

REAL(16) :: x(size(x_can)-1)


!x = (x0*x_can(:size(x0)))/x_can(size(x_can))

END FUNCTION InvTransform

END MODULE LPTools


MODULE LAOperators
IMPLICIT NONE


!PRIVATE
!PUBLIC :: DIAG, ONES, COLMULT, DOT, ENORM, GEMV, GEMM, SPDINV, TRANS, UPPER, LOWER, SPDLUD

CONTAINS


FUNCTION DIAG(x) RESULT(D)

REAL(16), INTENT(IN) :: x(:)
REAL(16) :: D(size(x), size(x))

INTEGER :: i


D=0
DO i = 1,size(x)
D(i,i) = x(i)
END DO

END FUNCTION DIAG

FUNCTION ONES(n) RESULT(D)

INTEGER, INTENT(IN) :: n
REAL(16) :: D(n,n)

REAL(16) :: e(n)


!e = real(1.Q0,realKind)
!D = DIAG(e)

END FUNCTION ONES


FUNCTION COLMULT(c,A) RESULT(cA)

REAL(16), INTENT(IN) :: c(:),A(:,:)
REAL(16) :: cA(size(A,1),size(A,2))

INTEGER :: i

!IF(size(c) /= size(A,2)) STOP "COLMULT ERROR: Invalid size"

!DO i = 1,size(c)
!	cA(:,i) = c(i)*A(:,i)
!END DO

END FUNCTION COLMULT


FUNCTION ADD(v) RESULT(a)

REAL(16), INTENT(IN) :: v(:)
REAL(16) :: a

INTEGER :: i


!a = 0
!DO i = 1, size(v)
!	a = a + v(i)
!END DO

END FUNCTION ADD


FUNCTION DOT(b,c) RESULT(bTc)

REAL(16), INTENT(IN) :: b(:),c(:)
REAL(16) :: bTc


!bTc = ADD(b(:)*c(:))

END FUNCTION DOT


FUNCTION ENORM(u) RESULT(n)

REAL(16) :: u(:)
REAL(16) :: n

!n = SQRT(DOT(u,u))

END FUNCTION ENORM

FUNCTION UPPER(A) RESULT(U)

REAL(16), INTENT(IN) :: A(:,:)
REAL(16) :: U(size(A,1),size(A,2))

INTEGER :: i

!IF(size(A,1) /= size(A,2)) STOP "UPPER ERROR: Invalid size"

!U = 0
!DO i=1,size(A,1)
!U( :i,i) = A( :i,i)
!END DO 

END FUNCTION UPPER

FUNCTION LOWER(A) RESULT(L)

REAL(16), INTENT(IN) :: A(:,:)
REAL(16) :: L(size(A,1),size(A,2))

INTEGER :: i

!IF(size(A,1) /= size(A,2)) STOP "LOWER ERROR: Invalid size"

!L = 0
!DO i=1,size(A,1)
!L(:i,i) = A(:i,i)
!END DO 

END FUNCTION LOWER

FUNCTION CholeskyDecomp(A) RESULT(L)

REAL(16):: A(:,:)
REAL(16) :: L(size(A,1),size(A,2))
INTEGER :: i

REAL(16) :: summ


!IF(size(A,1) /= size(A,2)) STOP "CHOLESKY ERROR: Invalid size"

!L = 0

!DO i = 1,size(A,1) 
!  summ = A(i,i) - DOT_PRODUCT(A(i,:i-1),A(i,:i-1))
!  IF(summ <= 0.) STOP "CHOLESKY ERROR: Invalid Matrix Input"
!  L(i,i) = SQRT(summ)
!  A(i+1:,i)=(A(i,i+1:)-MATMUL(A(i+1:,:i-1),A(i,:i-1)))/L(i,i)
!  L(i+1:,i) = A(i+1:,i)
!END DO

END FUNCTION CholeskyDecomposition

SUBROUTINE SPDLUD(A, L,U)

REAL(16), INTENT(IN) :: A(:,:)
REAL(16), INTENT(OUT):: L(size(A,1),size(A,2)), U(size(A,1),size(A,2))

INTEGER :: q, n, i, j
REAL(16) :: D(size(A,1)), Dia(size(A,1),size(A,2))

!IF(size(A,1) /= size(A,2))  STOP "SLUDecomposition ERROR: Invalid Matrix Input"

!n = size(A,1)

!U = UPPER(A)

!L = ONES(n)

!DO q = 1, n-1
!  DO i = q+1, n
!    U(i,i) = U(i,i) - U(q,i)*U(q,i)/U(q,q) 
!    DO j = i+1, n
!      U(i,j) = U(i,j) - U(q,j)*U(q,i)/U(q,q)	
!    END DO
! END DO
!  L(q+1:,q) = U(q,q+1:)/U(q,q)
!END DO

END SUBROUTINE SPDLUD

FUNCTION ForSubstitution(L, b) RESULT(x) 

REAL(16) :: L(:,:), b(:)
REAL(16) :: x(size(L,1))
INTEGER :: i


!IF(size(L,2) /= size(b)) STOP "BackSubstitution ERROR: Invalid size input"

!DO i  = 1,size(L,1)
!x(i) = (b(i) - SUM(L(i,:i-1)*x(:i-1)))/L(i,i)
!END DO

END FUNCTION ForSubstitution

FUNCTION BackSubstitution(U, b) RESULT(x) 

REAL(16) :: U(:,:), b(:)
REAL(16) :: x(size(U,1))
INTEGER :: i


!IF(size(U,2) /= size(b)) STOP "ForSubstitution ERROR: Invalid size input"

!DO i  = size(U,1),1,-1
!x(i) = (b(i) - SUM(U(i,i+1:)*x(i+1:)))/U(i,i)
!END DO

END FUNCTION BackSubstitution


FUNCTION GEMV(M,v) RESULT(Mv)

REAL(16) :: M(:,:),v(:)
REAL(16) :: Mv(size(M,1))
INTEGER :: i


!IF(size(M,2) /= size(v)) STOP "MatVecMult ERROR: Invalid size input"

!DO i = 1,size(M,1)
!	Mv(i) = DOT(M(i,:),v(:))
!END DO

END FUNCTION GEMV

FUNCTION GEMM(A,B) RESULT(AB)

REAL(16), INTENT(IN) :: A(:,:),B(:,:)
REAL(16) :: AB(size(A,1),size(B,2))
INTEGER :: i, j


!DO i = 1,size(A,1)
!	DO j = 1,size(B,2)
!		AB(i,j) = DOT(A(i,:),B(:,j))
!	END DO
!END DO

END FUNCTION GEMM

!FUNCTION SPDINV(A, y)  RESULT(x)

!REAL(16), INTENT(IN) :: A(:,:), y(:)
!REAL(16) :: x(size(A,2))

!REAL(16) :: L(size(A,1),size(A,1)), u(size(A,1))

!!IF(size(A, 1) /= size(A, 2) .OR. size(A,1) /= size(y)) STOP "PDLSS ERROR: Invalid Input"

!!L = CholeskyDecomposition(A)

!!u = ForSubstitution(L, y)

!!x = BackSubstitution(TRANS(L), u)

!!open( 25, file = './4.Analysis/EigenGraphs/Abs.txt',position='append')
!!write(25,*),i, ",", ENORM(GEMV(A,x)-y)
!!close(25)

!!open( 35, file = './4.Analysis/EigenGraphs/Rel.txt',position='append')
!!write(35,*),i, ",", ENORM(GEMV(A,x)-y)/ENORM(y)
!close(35)
!!i=i+1
!!print*, 'ABS', ENORM(GEMV(A,x)-y)
!!print*, "REL", ENORM(GEMV(A,x)-y)/ENORM(y) 

!END FUNCTION SPDINV


FUNCTION TRANS(A) RESULT(AT)

REAL(16), INTENT(IN) :: A(:,:)
REAL(16) :: AT(size(A,2),size(A,1))

INTEGER :: i, j

!FORALL(i = 1:size(A,1) , j = 1:size(A,2))
!	AT(j,i) = A(i,j)
!END FORALL

END FUNCTION TRANS

END MODULE LAOperators

MODULE LAPACKOperators
IMPLICIT NONE


!PRIVATE
!PUBLIC :: DOT, ENORM, DPOINV, EIGEN, LAGEMV, LAGEMM

CONTAINS


FUNCTION DOT(SX,SY) RESULT(uTv)

REAL(8), INTENT(IN) :: SX(:), SY(:)

REAL(8) :: uTv

INTEGER(8) :: N, INCX, INCY
!REAL(8), EXTERNAL :: DDOT


!INCX = 1D0; INCY = 1D0

!IF( size(SX) /= size(SY) ) STOP "DOT ERROR: Input Argument dimensions do not matchs"
!N = SIZE(SX)

!uTv = DDOT(N, SX, INCX, SY, INCY)

END FUNCTION DOT


FUNCTION ENORM(X) RESULT(NormV)

REAL(8), INTENT(IN) :: X(:)

REAL(8) :: NormV

INTEGER(8) :: N, INCX
!REAL(8), EXTERNAL :: DNRM2


!INCX = 1D0
!N = SIZE(X)

!NormV = DNRM2(N, X, INCX)

END FUNCTION ENORM


FUNCTION DPOINV(A) RESULT(AINV)

REAL(8), INTENT(IN) :: A(:,:)
    
REAL(8) :: AINV(size(A,1),size(A,1))

CHARACTER :: UPLO = 'L'
INTEGER :: N, LDA, IPIV(size(A,1)), INFO, i, j


!N = size(A,1)
!LDA = N	
!AINV = A

!IF( SIZE(A,1) /= SIZE(A,2) ) STOP "INVALID SIZE" 

!CALL DPOTRF( UPLO, N, AINV, LDA, INFO )

!IF(INFO /= 0) STOP "DPOINV : MATRIX INVERSION ERROR"

!CALL DPOTRI( UPLO, N, AINV, LDA, INFO )

!IF(INFO /= 0) STOP "DPOINV : MATRIX INVERSION ERROR"

!FORALL( i = 1:size(A,1) , j = 1:size(A,2) , j < i  )
!	AINV(j,i) = AINV(i,j)
!END FORALL

END FUNCTION DPOINV


FUNCTION EIGEN(A) RESULT(E)

REAL(8) :: A(:,:)

REAL(8) :: E(size(A,1))

CHARACTER :: JOBZ = 'N', UPLO = 'L'
INTEGER :: N, LDA, LWORK, INFO
REAL(8), ALLOCATABLE :: WORK(:)

!N = size(A,1)
!LDA = N

!LWORK = -1
!ALLOCATE(WORK(1))

!CALL DSYEV(JOBZ, UPLO, N, A, LDA, E, WORK, LWORK, INFO)

!LWORK = WORK(1)

!DEALLOCATE(WORK)
!ALLOCATE(WORK(LWORK))

!CALL DSYEV(JOBZ, UPLO, N, A, LDA, E, WORK, LWORK, INFO)

!DEALLOCATE(WORK)

END FUNCTION EIGEN


FUNCTION LAGEMV(A,X) RESULT(AX)

REAL(8), INTENT(IN) :: A(:,:), X(:)

REAL(8) :: AX(size(A,1))

CHARACTER :: TRANS = 'N'
INTEGER(8) :: M, N, LDA, INCX = 1, INCY = 1
REAL(8) :: ALPHA = 1.0, BETA = 0.0


!IF( SIZE(A,2) /= SIZE(X) ) STOP "GEMV ERROR: Input Argument dimensions do not match" 

!M = size(A,1)
!N = size(A,2)
!LDA = M

!CALL DGEMV(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, AX, INCY)

END FUNCTION LAGEMV

FUNCTION LAGEMM(A,B) RESULT(C)

REAL(8), INTENT(IN) :: A(:,:), B(:,:)

REAL(8) :: C(size(A,1),size(B,2))

CHARACTER :: TRANSA = 'N', TRANSB = 'N'
INTEGER(8) :: M, N, K, LDA, LDB, LDC
REAL(8) :: ALPHA = 1.0, BETA = 0.0


!IF( SIZE(A,2) /= SIZE(B,1) ) STOP "GEMM ERROR: Input Argument dimensions do not match" 

!M = size(A,1)
!N = size(B,2)
!K = size(A,2)
!LDA = M
!LDB = K
!LDC = M

!CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)

END FUNCTION LAGEMM

END MODULE LAPACKOperators

MODULE Augment
IMPLICIT NONE


!PRIVATE
!PUBLIC :: OPERATOR(.HAUG.), OPERATOR(.VAUG.)


!INTERFACE OPERATOR(.HAUG.)
!	MODULE PROCEDURE H_Augment_vv
!	MODULE PROCEDURE H_Augment_Mv
!	MODULE PROCEDURE H_Augment_vM
!	MODULE PROCEDURE H_Augment_MM
!END INTERFACE(.HAUG.)

!INTERFACE OPERATOR(VAUG)
!	MODULE PROCEDURE V_Augment_ss
!	MODULE PROCEDURE V_Augment_sv
!	MODULE PROCEDURE V_Augment_vs
!	MODULE PROCEDURE V_Augment_vv
!	MODULE PROCEDURE V_Augment_Mv
!	MODULE PROCEDURE V_Augment_vM
!	MODULE PROCEDURE V_Augment_MM
!END INTERFACE(VAUG)


CONTAINS


FUNCTION HAugment_vv(a,b) RESULT(ab)

	REAL(16), INTENT(IN) :: a(:), b(:)
	REAL(16) :: ab(size(a),2)
	
!	IF (size(a) /= size(b) ) STOP "Incorrect Dimensions to operator .HAUG."

	ab(:,1) = a(:)
	ab(:,2) = b(:)

END FUNCTION HAugment_vv

FUNCTION HAugment_Mv(A,b) RESULT(Ab)

	REAL(16), INTENT(IN) :: A(:,:), b(:)
	REAL(16) :: Ab(size(b),size(A,2)+1)

!	IF (size(A,1) /= size(b) ) STOP "Incorrect Dimensions to operator .HAUG."

	Ab(:,:size(A,2)) = A
	Ab(:,size(A,2)+1) = b

END FUNCTION HAugment_Mv

FUNCTION HAugment_vM(a,B) RESULT(aB)

	REAL(16), INTENT(IN) :: a(:), B(:,:)
	REAL(16) :: aB(size(a),1+size(B,2))


!	IF (size(a) /= size(B,1) ) STOP "Incorrect Dimensions to operator .HAUG."

	aB(:,1) = a
	aB(:,2:) = B	

END FUNCTION HAugment_vM

FUNCTION HAugment_MM(A,B) RESULT(AB)

	REAL(16), INTENT(IN) :: A(:,:), B(:,:)
	REAL(16) :: AB(size(A,1),size(A,2)+size(B,2))


!	IF (size(A,1) /= size(B,1) ) STOP "Incorrect DImensions to operator .HAUG."

	AB(:,:size(A,2)) = A
	Ab(:,size(A,2)+1:size(A,2)+size(B,2)) = b

END FUNCTION HAugment_MM


FUNCTION VAugment_ss(a,b) RESULT(ab)

	REAL(16), INTENT(IN) :: a,b
	REAL(16) :: ab(2)

	ab(1) = a
	ab(2) = b

END FUNCTION VAugment_ss

FUNCTION VAugment_sv(s,v) RESULT(sv)

	REAL(16), INTENT(IN) :: s, v(:)
	REAL(16) :: sv(1+size(v))

	sv(1) = s
	sv(2:) = v

END FUNCTION VAugment_sv

FUNCTION VAugment_vs(v,s) RESULT(vs)

	REAL(16), INTENT(IN) :: v(:), s
	REAL(16) :: vs(size(v)+1)

	vs(:size(v)) = v
	vs(size(v)+1) = s

END FUNCTION VAugment_vs


FUNCTION VAugment_vv(a,b) RESULT(ab)

	REAL(16), INTENT(IN) :: a(:), b(:)
	REAL(16) :: ab(size(a)+size(b))

	ab(:size(a)) = a
	ab(size(a)+1:size(a)+size(b)) = b

END FUNCTION VAugment_vv

FUNCTION VAugment_Mv(A,b_T) RESULT(Ab_T)

	REAL(16), INTENT(IN) :: A(:,:), b_T(:)
	REAL(16) :: Ab_T(size(A,1)+1,size(A,2))


!	IF (size(A,2) /= size(b_T) ) STOP "Incorrect Dimensions to operator .HAUG."

	Ab_T(:size(A,1),:) = A
	Ab_T(size(A,1)+1,:) = b_T

END FUNCTION VAugment_Mv

FUNCTION VAugment_vM(a_T,B) RESULT(a_TB)

	REAL(16), INTENT(IN) :: a_T(:), B(:,:)
	REAL(16) :: a_TB(1+size(B,1),size(B,2))


!	IF (size(a_T) /= size(B,2) ) STOP "Incorrect Dimensions to operator .HAUG."

	a_TB(1,:) = a_T
	a_TB(2:1+size(B,1),:) = B	

END FUNCTION VAugment_vM

FUNCTION VAugment_MM(A,B) RESULT(AB)

	REAL(16), INTENT(IN) :: A(:,:), B(:,:)
	REAL(16) :: AB(size(A,1)+size(B,1),size(A,2))


!	IF (size(A,2) /= size(B,2) ) STOP "Incorrect Dimensions to operator .HAUG."

	AB(:size(A,1),:) = A
	AB(size(A,1)+1:size(A,1)+size(B,1),:) = B

END FUNCTION VAugment_MM

END MODULE Augment

MODULE GeoGebra
IMPLICIT NONE


!PRIVATE
!PUBLIC :: GGBPlot, GGBCommand, GGBPlotPoint, GGBPlotVector, GGBInit, GGBPlotSystem

CONTAINS

SUBROUTINE GGBInit()

REAL(16) :: X(3) = (/1,0,0/), Y(3) = (/0,1,0/), Z(3) = (/0,0,1/)
INTEGER :: i, j

!open (1000, file = './7.GeoGebra/GGBCommands.js',status = 'replace')
!close(1000)

!CALL GGBPlotPoint(X,"X","FF00FF")
!CALL GGBPlotPoint(Y,"Y","FF00FF")
!CALL GGBPlotPoint(Z,"Z","FF00FF")
!CALL GGBCommand("P = Polygon(X,Y,Z)")
!CALL GGBCommand('SetColor(P,"#FF00FF")')
!CALL GGBCommand('ZoomIn(5,O)')
!CALL GGBCommand('SetViewDirection((1,1,1))')

END SUBROUTINE GGBInit

SUBROUTINE GGBPlot()

!CALL SYSTEM('firefox -new-tab ./7.GeoGebra/GGB.html')

END SUBROUTINE GGBPlot

SUBROUTINE GGBPlotSystem(A,c, b)

REAL(16) :: A(:,:), c(:)
REAL(16):: b(:)
INTEGER :: i
CHARACTER(len=10) :: str(3)
CHARACTER(len=100) :: vstr

!IF(size(A,2) /= 3) STOP "GGBPlotSystem ERROR: Invalid Input"

!10 FORMAT (F10.5)
!IF(PRESENT(b)) THEN
!IF(size(A,1) /= size(b)) STOP "GGBPlotSystem ERROR: Invalid Input"
!DO i=1,size(A,1)	
!CALL GGBPlotPlane(A(i,:),b(i))
!END DO
!write(str , 10) real(c)
!vstr =  'Vector(('//TRIM(str(1))//","//TRIM(str(2))//","//TRIM(str(3))//'))'
!CALL GGBCommand(TRIM(vstr))
!ELSE
!DO i=1,size(A,1)
!CALL GGBPlotPlane(A(i,:),real(0,realKind))
!END DO
!write(str , 10) real(c)
!vstr =  'Vector('//TRIM(str(1))//","//TRIM(str(2))//","//TRIM(str(3))//')'
!CALL GGBCommand(TRIM(vstr))
!END IF

END SUBROUTINE GGBPlotSystem

SUBROUTINE GGBPlotPoint(v, label,color)

REAL(16) :: v(3)
CHARACTER(len=10) :: label
CHARACTER(len=6) :: color

CHARACTER(len=10) :: str(3)
CHARACTER(len=100) :: vstr

!IF ( SIZE(v) /= 3 ) STOP "GGBPlotPoint ERROR: Input Array exceeded limit"

!10 FORMAT (F10.5)
!write(str , 10) real(v)

!IF(PRESENT(label)) THEN
!	vstr = label//"="//"("//TRIM(str(1))//","//TRIM(str(2))//","//TRIM(str(3))//")"
!	CALL GGBCommand(TRIM(vstr))
!	CALL GGBLabel(label)
!	IF(PRESENT(color)) THEN
!	vstr = 'SetColor('//label//',"#'//color//'")'
!	CALL GGBCommand(vstr)
!END IF
!ELSE 
!	vstr = "("//TRIM(str(1))//","//TRIM(str(2))//","//TRIM(str(3))//")"
!	CALL GGBCommand(vstr)
!END IF

END SUBROUTINE GGBPlotPoint

SUBROUTINE GGBPlotVector(v)

REAL(16) :: v(3)

CHARACTER(len=10) :: str(3)
CHARACTER(len=100) :: vstr

!IF ( SIZE(v) /= 3 ) STOP "GGBPlotPoint ERROR: Input Array exceeded limit"

!10 FORMAT (F10.5)
!write(str , 10) real(v)
!vstr =  'Vector(('//TRIM(str(1))//","//TRIM(str(2))//","//TRIM(str(3))//'))'
!CALL GGBCommand(TRIM(vstr))

END SUBROUTINE GGBPlotVector

SUBROUTINE GGBPlotPlane(c,b)

REAL(16) :: c(3), b
CHARACTER(len=10) :: str(4)
CHARACTER(len=50) :: vstr

!10 FORMAT (F10.5)
!write(str , 10) real(c), real(b)
!vstr = TRIM(str(1))//"x+"//TRIM(str(2))//"y+"//TRIM(str(3))//"z="//TRIM(str(4))
!CALL GGBCommand(vstr)

END SUBROUTINE GGBPlotPlane

SUBROUTINE GGBLabel(label)

CHARACTER(len=10) :: label

!open (1000, file = './7.GeoGebra/GGBCommands.js',access = 'append')
!	write(1000,*) "ggbApplet.setLabelVisible('"//label//"',true);"
!close(1000)


END SUBROUTINE GGBLabel

SUBROUTINE GGBCommand(str)

CHARACTER(len=10), INTENT(IN) :: str

!open (1000, file = './7.GeoGebra/GGBCommands.js',access = 'append')
!	write(1000,*) "ggbApplet.evalCommand('",TRIM(str),"');"
!close(1000)

END SUBROUTINE GGBCommand

END MODULE GeoGebra
