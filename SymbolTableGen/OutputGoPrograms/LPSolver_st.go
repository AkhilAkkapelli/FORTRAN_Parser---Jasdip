program testLPSolver

    use LPPGeneration
    use LinearProblemSolver

    implicit none


end program testLPSolver

module ProbabilityDistribution

    implicit none

    contains

        function UniformDist ( seed ) result( u )

            implicit none

            integer(8) :: seed
            integer(8),parameter :: IA = 16807,IM = 2147483647,IQ = 127773,IR = 2836
            real(16),save :: am
            integer(8),save :: ix = - 1,iy = - 1,k
            real(16) :: u


        end function UniformDist

        function NormalDist ( seed ) result( n )

            implicit none

            integer(8) :: seed
            real(16),parameter :: PI = 4
            real(16) :: u1,u2
            real(16) :: n

            u1 = Uniform(seed)
            u2 = Uniform(seed)
            n = sqrt(- 2 * log(u1)) * cos(2 * PI * u2)
            n = variance * n + mean

        end function NormalDist


end module ProbabilityDistribution

module ProbabilityGeneration

    use ProbabilityDistribution

    implicit none

    interface ProbGen

        module procedure ProbVecGen
        module procedure ProbMatGen

    end interface ProbGen


    contains

        subroutine ProbVecGen ( v )

            implicit none

            real(16) :: v(:)
            integer(8) :: seed = 54321,count
            integer :: i


            do i = 1,size(v)

                v(i) = ProbDist(seed)

            end do


        end subroutine ProbVecGen

        subroutine ProbMatGen ( M )

            implicit none

            real(16) :: M(:,:)
            integer(8) :: seed = 54321,count
            integer :: i,j


            do i = 1,size(M,1)


                do j = 1,size(M,2)

                    M(i,j) = ProbDist(seed)

                end do


            end do


        end subroutine ProbMatGen


end module ProbabilityGeneration

module LPPGeneration

    use ProbabilityDistribution

    implicit none

    contains

        subroutine LPPGen (  )

            implicit none

            real(16) :: A(row,column),b(row),c(column)
            integer(8) :: seed = 123456,count
            integer :: i,j


        end subroutine LPPGen


end module LPPGeneration

module LinearProblemSolver

    use ProbabilityGeneration
    use Algorithm
    use LPTools
    use LAOperators
    use Augment
    use GeoGebra
    use HistogramPlot
    use PostProcessor

    implicit none

    contains

        subroutine CanonicalForm (  )

            implicit none

            real(16) :: A(row,column),c(column),x_opt(column)
            integer :: i,j


        end subroutine CanonicalForm

        subroutine EqualityForm (  )

            implicit none

            real(16) :: A_eq(row,column),b_eq(row),c_eq(column),x0(column),x_eq(column),eps,A_can(size(A_eq,1),size(A_eq,2) + 1),c_can(size(c_eq) + 1),x_can(size(A_eq,2) + 1),A_can1(size(A_eq,1),size(A_eq,2) + 2),c_can1(size(c_eq) + 2),x_can1(size(A_eq,2) + 2),A(row,column + 1),b(row),c(column + 1),x01(column + 1),x_opt1(column + 1),lambda
            integer :: i,j


        end subroutine EqualityForm

        subroutine StandardForm (  )

            implicit none

            real(16) :: A(row,column),b(row),c(column),x_opt(column),A_can(size(A,1) + size(A,2) + 1,2 * ( size(A,1) + size(A,2) + 1 )),c_can(2 * ( size(A,1) + size(A,2) + 1 )),x_can(2 * ( size(A,1) + size(A,2) + 1 )),x(2 * ( size(A,1) + size(A,2) ) + 1),a0(2 * ( size(A,1) + size(A,2) ) + 1)
            integer :: i,j


        end subroutine StandardForm

        subroutine LeastNegativeForm (  )

            implicit none

            real(16) :: A_ln(row,column),b_ln(row),c_ln(column),y0(row),x0(column),x_ln(column),eps,e(column),A_aug(row,column + 1),b_aug(row),c_aug(column + 1),x0_aug(column + 1),x_aug(column + 1),A_augcan(size(A_ln,1),size(A_ln,2) + 2),c_augcan(size(c_ln) + 2),x_augcan(size(A_ln,2) + 2),A_can(size(A_ln,1),size(A_ln,2) + 1),c_can(size(c_ln) + 1),x_can(size(A_ln,2) + 1)
            integer :: i,j,opt = 0,greater = 0,num = 0,feas = 0,fail = 0,ini_feas = 0


        end subroutine LeastNegativeForm


end module LinearProblemSolver

module Algorithm

    use Augment
    use LAPACKOperators
    use LAOperators

    implicit none

    contains

        function ProjectiveMethod ( A,c ) result( x_can )

            implicit none

            real(16),intent(in) :: A(:,:),c(:)
            real(16) :: x_can(size(c))
            real(16) :: x_p(size(A,2)),x(size(A,2)),f_p,f,eps,x0_aug(column + 1),x_aug(column + 1)
            integer :: n,iter,iter_limit,i,j,iter_num = 1,iterz = 0

            contains

                subroutine Optimize ( x_p,x )

                    implicit none

                    real(16),intent(in) :: x_p(:)
                    real(16) :: x(size(x_p))
                    real(16) :: e(size(x_p)),Ad(size(A,1),size(x_p)),B(size(A,1) + 1,size(x_p)),v(size(A,1) + 1),c_p(size(c)),c_unit(size(c)),x0(size(x_p)),alpha


                end subroutine Optimize


        end function ProjectiveMethod


end module Algorithm

module Statistics

    implicit none

    interface Arithmetic_Mean

        module procedure arithmetic_vector
        module procedure arithmetic_matrix

    end interface Arithmetic_Mean

    interface Geometric_Mean

        module procedure geometric_vector
        module procedure geometric_matrix

    end interface Geometric_Mean

    interface Harmonic_Mean

        module procedure harmonic_vector
        module procedure harmonic_matrix

    end interface Harmonic_Mean


    contains

        function arithmetic_vector ( vector ) result( amean )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16) :: amean

            amean = SUM(vector) / size(vector)

        end function arithmetic_vector

        function arithmetic_matrix ( Matrix ) result( amean )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16) :: amean

            amean = arithmetic_vector(RESHAPE(Matrix,(/ size(Matrix) /)))

        end function arithmetic_matrix

        function geometric_vector ( vector ) result( gmean )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16) :: gmean

            gmean = EXP(arithmetic_vector(LOG(vector)))

        end function geometric_vector

        function geometric_matrix ( Matrix ) result( gmean )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16) :: gmean

            gmean = geometric_vector(RESHAPE(Matrix,(/ size(Matrix) /)))

        end function geometric_matrix

        function harmonic_vector ( vector ) result( hmean )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16) :: hmean

            hmean = size(vector) / SUM(1 / vector)

        end function harmonic_vector

        function harmonic_matrix ( Matrix ) result( hmean )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16) :: hmean

            hmean = harmonic_vector(RESHAPE(Matrix,(/ size(Matrix) /)))

        end function harmonic_matrix


end module Statistics

module HistogramPlot

    implicit none

    interface Histogram

        module procedure histogram_vector
        module procedure histogram_matrix

    end interface Histogram


    contains

        subroutine histogram_vector ( vector )

            implicit none

            real(16),intent(in) :: vector(:)
            integer(8) :: i,s,n = 0
            real(16) :: limit,interval
            character( len = 6 ) :: lowerbound,upperbound
            character( len = 16 ) :: filename
            real(16),allocatable :: group(:,:)
            integer(8),allocatable :: p(:)


        end subroutine histogram_vector

        subroutine histogram_matrix ( Matrix )

            implicit none

            real(16),intent(in) :: Matrix(:,:)


        end subroutine histogram_matrix

        subroutine Plot ( Y,Z )

            implicit none

            real(16) :: Y(:)
            real(16) :: Z(:)
            real(16) :: X(size(Y))
            integer :: i


        end subroutine Plot


end module HistogramPlot

module Postprocessor

    use LAPACKOperators
    use HistogramPlot

    implicit none

    interface Stats

        module procedure Stats_vector
        module procedure Stats_matrix

    end interface Stats


    contains

        subroutine Stats_vector ( vector,M,StdDev )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16),intent(out) :: M,StdDev
            real(16) :: s,ssq
            integer :: i,n


        end subroutine Stats_vector

        subroutine Stats_matrix ( Matrix,M,StdDev )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16),intent(out) :: M,StdDev


        end subroutine Stats_matrix

        subroutine EIGENRECORD ( M )

            implicit none

            real(16),intent(in) :: M(:,:)
            real(16) :: E(size(M,1)),COND
            integer :: i = 1


        end subroutine EIGENRECORD

        subroutine EigenAnalysis (  )

            implicit none

            real(16) :: E(row + 1,50),COND(50)
            integer :: IOstatus,i,n


        end subroutine EigenAnalysis


end module Postprocessor

module LPTools

    use Augment
    use ProbabilityGeneration
    use LAOperators

    implicit none

    contains

        function Potential ( c,x ) result( f )

            implicit none

            real(16),intent(in) :: c(column + 1),x(column + 1)
            real(16) :: f


        end function Potential

        function zero_ratio ( n,c_unit ) result( alpha )

            implicit none

            integer,intent(in) :: n
            real(16),intent(in) :: c_unit(n)
            real(16) :: alpha,a
            real(16) :: beta
            integer :: idx


        end function zero_ratio

        function min_ratio ( n,c_unit ) result( alpha )

            implicit none

            integer,intent(in) :: n
            real(16),intent(in) :: c_unit(n)
            real(16) :: alpha
            real(16) :: beta
            integer :: idx


        end function min_ratio

        subroutine Dual ( A,b,c,A_dual,b_dual,c_dual )

            implicit none

            real(16),intent(in) :: A(:,:),b(:),c(:)
            real(16),intent(out) :: A_dual(size(A,2),size(A,1)),b_dual(size(c)),c_dual(size(b))


        end subroutine Dual

        subroutine StdToCan ( A_std,b_std,c_std,A_can,c_can,a0 )

            implicit none

            real(16),intent(in) :: A_std(:,:),b_std(:),c_std(:)
            real(16),intent(out) :: A_can(size(A_std,1) + size(A_std,2) + 1,2 * ( size(A_std,1) + size(A_std,2) + 1 )),c_can(2 * ( size(A_std,1) + size(A_std,2) + 1 )),a0(2 * ( size(c_std) + size(b_std) ) + 1)
            real(16) :: A(size(A_std,1) + size(A_std,2) + 1,2 * ( size(A_std,1) + size(A_std,2) ) + 1),b(size(A_std,1) + size(A_std,2) + 1),c(2 * ( size(A_std,1) + size(A_std,2) ) + 1)
            real(16) :: x0(size(A_std,2)),y0(size(A_std,1)),u0(size(A_std,1)),v0(size(A_std,2)),lambda_0
            integer :: m,n,i,j


        end subroutine StdToCan

        subroutine Transform ( A,b,c,A_can,c_can,a0 )

            implicit none

            real(16),intent(in) :: A(:,:),b(:),c(:),a0(:)
            real(16),intent(out) :: A_can(size(A,1),size(A,2) + 1),c_can(size(c) + 1)


        end subroutine Transform

        function InvTransform ( x_can,x0 ) result( x )

            implicit none

            real(16),intent(in) :: x_can(:),x0(:)
            real(16) :: x(size(x_can) - 1)


        end function InvTransform


end module LPTools

module LAOperators

    implicit none

    contains

        function DIAG ( x ) result( D )

            implicit none

            real(16),intent(in) :: x(:)
            real(16) :: D(size(x),size(x))
            integer :: i

            D = 0

            do i = 1,size(x)

                D(i,i) = x(i)

            end do


        end function DIAG

        function ONES ( n ) result( D )

            implicit none

            integer,intent(in) :: n
            real(16) :: D(n,n)
            real(16) :: e(n)


        end function ONES

        function COLMULT ( c,A ) result( cA )

            implicit none

            real(16),intent(in) :: c(:),A(:,:)
            real(16) :: cA(size(A,1),size(A,2))
            integer :: i


        end function COLMULT

        function ADD ( v ) result( a )

            implicit none

            real(16),intent(in) :: v(:)
            real(16) :: a
            integer :: i


        end function ADD

        function DOT ( b,c ) result( bTc )

            implicit none

            real(16),intent(in) :: b(:),c(:)
            real(16) :: bTc


        end function DOT

        function ENORM ( u ) result( n )

            implicit none

            real(16) :: u(:)
            real(16) :: n


        end function ENORM

        function UPPER ( A ) result( U )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16) :: U(size(A,1),size(A,2))
            integer :: i


        end function UPPER

        function LOWER ( A ) result( L )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16) :: L(size(A,1),size(A,2))
            integer :: i


        end function LOWER

        function CholeskyDecomp ( A ) result( L )

            implicit none

            real(16) :: A(:,:)
            real(16) :: L(size(A,1),size(A,2))
            integer :: i
            real(16) :: summ


        end function CholeskyDecomp

        subroutine SPDLUD ( A,L,U )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16),intent(out) :: L(size(A,1),size(A,2)),U(size(A,1),size(A,2))
            integer :: q,n,i,j
            real(16) :: D(size(A,1)),Dia(size(A,1),size(A,2))


        end subroutine SPDLUD

        function ForSubstitution ( L,b ) result( x )

            implicit none

            real(16) :: L(:,:),b(:)
            real(16) :: x(size(L,1))
            integer :: i


        end function ForSubstitution

        function BackSubstitution ( U,b ) result( x )

            implicit none

            real(16) :: U(:,:),b(:)
            real(16) :: x(size(U,1))
            integer :: i


        end function BackSubstitution

        function GEMV ( M,v ) result( Mv )

            implicit none

            real(16) :: M(:,:),v(:)
            real(16) :: Mv(size(M,1))
            integer :: i


        end function GEMV

        function GEMM ( A,B ) result( AB )

            implicit none

            real(16),intent(in) :: A(:,:),B(:,:)
            real(16) :: AB(size(A,1),size(B,2))
            integer :: i,j


        end function GEMM

        function TRANS ( A ) result( AT )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16) :: AT(size(A,2),size(A,1))
            integer :: i,j


        end function TRANS


end module LAOperators

module LAPACKOperators

    implicit none

    contains

        function DOT ( SX,SY ) result( uTv )

            implicit none

            real(8),intent(in) :: SX(:),SY(:)
            real(8) :: uTv
            integer(8) :: N,INCX,INCY


        end function DOT

        function ENORM ( X ) result( NormV )

            implicit none

            real(8),intent(in) :: X(:)
            real(8) :: NormV
            integer(8) :: N,INCX


        end function ENORM

        function DPOINV ( A ) result( AINV )

            implicit none

            real(8),intent(in) :: A(:,:)
            real(8) :: AINV(size(A,1),size(A,1))
            character :: UPLO = 'L'
            integer :: N,LDA,IPIV(size(A,1)),INFO,i,j


        end function DPOINV

        function EIGEN ( A ) result( E )

            implicit none

            real(8) :: A(:,:)
            real(8) :: E(size(A,1))
            character :: JOBZ = 'N', UPLO = 'L'
            integer :: N,LDA,LWORK,INFO
            real(8),allocatable :: WORK(:)


        end function EIGEN

        function LAGEMV ( A,X ) result( AX )

            implicit none

            real(8),intent(in) :: A(:,:),X(:)
            real(8) :: AX(size(A,1))
            character :: TRANS = 'N'
            integer(8) :: M,N,LDA,INCX = 1,INCY = 1
            real(8) :: ALPHA = 1.0,BETA = 0.0


        end function LAGEMV

        function LAGEMM ( A,B ) result( C )

            implicit none

            real(8),intent(in) :: A(:,:),B(:,:)
            real(8) :: C(size(A,1),size(B,2))
            character :: TRANSA = 'N', TRANSB = 'N'
            integer(8) :: M,N,K,LDA,LDB,LDC
            real(8) :: ALPHA = 1.0,BETA = 0.0


        end function LAGEMM


end module LAPACKOperators

module Augment

    implicit none

    contains

        function HAugment_vv ( a,b ) result( ab )

            implicit none

            real(16),intent(in) :: a(:),b(:)
            real(16) :: ab(size(a),2)

            ab(:,1) = a(:)
            ab(:,2) = b(:)

        end function HAugment_vv

        function HAugment_Mv ( A,b ) result( Ab )

            implicit none

            real(16),intent(in) :: A(:,:),b(:)
            real(16) :: Ab(size(b),size(A,2) + 1)

            Ab(:,:size(A,2)) = A
            Ab(:,size(A,2) + 1) = b

        end function HAugment_Mv

        function HAugment_vM ( a,B ) result( aB )

            implicit none

            real(16),intent(in) :: a(:),B(:,:)
            real(16) :: aB(size(a),1 + size(B,2))

            aB(:,1) = a
            aB(:,2:) = B

        end function HAugment_vM

        function HAugment_MM ( A,B ) result( AB )

            implicit none

            real(16),intent(in) :: A(:,:),B(:,:)
            real(16) :: AB(size(A,1),size(A,2) + size(B,2))

            AB(:,:size(A,2)) = A
            Ab(:,size(A,2) + 1:size(A,2) + size(B,2)) = b

        end function HAugment_MM

        function VAugment_ss ( a,b ) result( ab )

            implicit none

            real(16),intent(in) :: a,b
            real(16) :: ab(2)

            ab(1) = a
            ab(2) = b

        end function VAugment_ss

        function VAugment_sv ( s,v ) result( sv )

            implicit none

            real(16),intent(in) :: s,v(:)
            real(16) :: sv(1 + size(v))

            sv(1) = s
            sv(2:) = v

        end function VAugment_sv

        function VAugment_vs ( v,s ) result( vs )

            implicit none

            real(16),intent(in) :: v(:),s
            real(16) :: vs(size(v) + 1)

            vs(:size(v)) = v
            vs(size(v) + 1) = s

        end function VAugment_vs

        function VAugment_vv ( a,b ) result( ab )

            implicit none

            real(16),intent(in) :: a(:),b(:)
            real(16) :: ab(size(a) + size(b))

            ab(:size(a)) = a
            ab(size(a) + 1:size(a) + size(b)) = b

        end function VAugment_vv

        function VAugment_Mv ( A,b_T ) result( Ab_T )

            implicit none

            real(16),intent(in) :: A(:,:),b_T(:)
            real(16) :: Ab_T(size(A,1) + 1,size(A,2))

            Ab_T(:size(A,1),:) = A
            Ab_T(size(A,1) + 1,:) = b_T

        end function VAugment_Mv

        function VAugment_vM ( a_T,B ) result( a_TB )

            implicit none

            real(16),intent(in) :: a_T(:),B(:,:)
            real(16) :: a_TB(1 + size(B,1),size(B,2))

            a_TB(1,:) = a_T
            a_TB(2:1 + size(B,1),:) = B

        end function VAugment_vM

        function VAugment_MM ( A,B ) result( AB )

            implicit none

            real(16),intent(in) :: A(:,:),B(:,:)
            real(16) :: AB(size(A,1) + size(B,1),size(A,2))

            AB(:size(A,1),:) = A
            AB(size(A,1) + 1:size(A,1) + size(B,1),:) = B

        end function VAugment_MM


end module Augment

module GeoGebra

    implicit none

    contains

        subroutine GGBInit (  )

            implicit none

            real(16) :: X(3) = (/ 1,0,0 /),Y(3) = (/ 0,1,0 /),Z(3) = (/ 0,0,1 /)
            integer :: i,j


        end subroutine GGBInit

        subroutine GGBPlot (  )

            implicit none


        end subroutine GGBPlot

        subroutine GGBPlotSystem ( A,c,b )

            implicit none

            real(16) :: A(:,:),c(:)
            real(16) :: b(:)
            integer :: i
            character( len = 10 ) :: str(3)
            character( len = 100 ) :: vstr


        end subroutine GGBPlotSystem

        subroutine GGBPlotPoint ( v,label,color )

            implicit none

            real(16) :: v(3)
            character( len = 10 ) :: label
            character( len = 6 ) :: color
            character( len = 10 ) :: str(3)
            character( len = 100 ) :: vstr


        end subroutine GGBPlotPoint

        subroutine GGBPlotVector ( v )

            implicit none

            real(16) :: v(3)
            character( len = 10 ) :: str(3)
            character( len = 100 ) :: vstr


        end subroutine GGBPlotVector

        subroutine GGBPlotPlane ( c,b )

            implicit none

            real(16) :: c(3),b
            character( len = 10 ) :: str(4)
            character( len = 50 ) :: vstr


        end subroutine GGBPlotPlane

        subroutine GGBLabel ( label )

            implicit none

            character( len = 10 ) :: label


        end subroutine GGBLabel

        subroutine GGBCommand ( str )

            implicit none

            character( len = 10 ),intent(in) :: str


        end subroutine GGBCommand


end module GeoGebra

program testLPSolver

    use LPPGeneration
    use LinearProblemSolver

    implicit none


end program testLPSolver

module ProbabilityDistribution

    implicit none

    contains

        function UniformDist ( seed ) result( u )

            implicit none

            integer(8) :: seed
            integer(8),parameter :: IA = 16807,IM = 2147483647,IQ = 127773,IR = 2836
            real(16),save :: am
            integer(8),save :: ix = - 1,iy = - 1,k
            real(16) :: u


        end function UniformDist

        function NormalDist ( seed ) result( n )

            implicit none

            integer(8) :: seed
            real(16),parameter :: PI = 4
            real(16) :: u1,u2
            real(16) :: n

            u1 = Uniform(seed)
            u2 = Uniform(seed)
            n = sqrt(- 2 * log(u1)) * cos(2 * PI * u2)
            n = variance * n + mean

        end function NormalDist


end module ProbabilityDistribution

module ProbabilityGeneration

    use ProbabilityDistribution

    implicit none

    interface ProbGen

        module procedure ProbVecGen
        module procedure ProbMatGen

    end interface ProbGen


    contains

        subroutine ProbVecGen ( v )

            implicit none

            real(16) :: v(:)
            integer(8) :: seed = 54321,count
            integer :: i


            do i = 1,size(v)

                v(i) = ProbDist(seed)

            end do


        end subroutine ProbVecGen

        subroutine ProbMatGen ( M )

            implicit none

            real(16) :: M(:,:)
            integer(8) :: seed = 54321,count
            integer :: i,j


            do i = 1,size(M,1)


                do j = 1,size(M,2)

                    M(i,j) = ProbDist(seed)

                end do


            end do


        end subroutine ProbMatGen


end module ProbabilityGeneration

module LPPGeneration

    use ProbabilityDistribution

    implicit none

    contains

        subroutine LPPGen (  )

            implicit none

            real(16) :: A(row,column),b(row),c(column)
            integer(8) :: seed = 123456,count
            integer :: i,j


        end subroutine LPPGen


end module LPPGeneration

module LinearProblemSolver

    use ProbabilityGeneration
    use Algorithm
    use LPTools
    use LAOperators
    use Augment
    use GeoGebra
    use HistogramPlot
    use PostProcessor

    implicit none

    contains

        subroutine CanonicalForm (  )

            implicit none

            real(16) :: A(row,column),c(column),x_opt(column)
            integer :: i,j


        end subroutine CanonicalForm

        subroutine EqualityForm (  )

            implicit none

            real(16) :: A_eq(row,column),b_eq(row),c_eq(column),x0(column),x_eq(column),eps,A_can(size(A_eq,1),size(A_eq,2) + 1),c_can(size(c_eq) + 1),x_can(size(A_eq,2) + 1),A_can1(size(A_eq,1),size(A_eq,2) + 2),c_can1(size(c_eq) + 2),x_can1(size(A_eq,2) + 2),A(row,column + 1),b(row),c(column + 1),x01(column + 1),x_opt1(column + 1),lambda
            integer :: i,j


        end subroutine EqualityForm

        subroutine StandardForm (  )

            implicit none

            real(16) :: A(row,column),b(row),c(column),x_opt(column),A_can(size(A,1) + size(A,2) + 1,2 * ( size(A,1) + size(A,2) + 1 )),c_can(2 * ( size(A,1) + size(A,2) + 1 )),x_can(2 * ( size(A,1) + size(A,2) + 1 )),x(2 * ( size(A,1) + size(A,2) ) + 1),a0(2 * ( size(A,1) + size(A,2) ) + 1)
            integer :: i,j


        end subroutine StandardForm

        subroutine LeastNegativeForm (  )

            implicit none

            real(16) :: A_ln(row,column),b_ln(row),c_ln(column),y0(row),x0(column),x_ln(column),eps,e(column),A_aug(row,column + 1),b_aug(row),c_aug(column + 1),x0_aug(column + 1),x_aug(column + 1),A_augcan(size(A_ln,1),size(A_ln,2) + 2),c_augcan(size(c_ln) + 2),x_augcan(size(A_ln,2) + 2),A_can(size(A_ln,1),size(A_ln,2) + 1),c_can(size(c_ln) + 1),x_can(size(A_ln,2) + 1)
            integer :: i,j,opt = 0,greater = 0,num = 0,feas = 0,fail = 0,ini_feas = 0


        end subroutine LeastNegativeForm


end module LinearProblemSolver

module Algorithm

    use Augment
    use LAPACKOperators
    use LAOperators

    implicit none

    contains

        function ProjectiveMethod ( A,c ) result( x_can )

            implicit none

            real(16),intent(in) :: A(:,:),c(:)
            real(16) :: x_can(size(c))
            real(16) :: x_p(size(A,2)),x(size(A,2)),f_p,f,eps,x0_aug(column + 1),x_aug(column + 1)
            integer :: n,iter,iter_limit,i,j,iter_num = 1,iterz = 0

            contains

                subroutine Optimize ( x_p,x )

                    implicit none

                    real(16),intent(in) :: x_p(:)
                    real(16) :: x(size(x_p))
                    real(16) :: e(size(x_p)),Ad(size(A,1),size(x_p)),B(size(A,1) + 1,size(x_p)),v(size(A,1) + 1),c_p(size(c)),c_unit(size(c)),x0(size(x_p)),alpha


                end subroutine Optimize


        end function ProjectiveMethod


end module Algorithm

module Statistics

    implicit none

    interface Arithmetic_Mean

        module procedure arithmetic_vector
        module procedure arithmetic_matrix

    end interface Arithmetic_Mean

    interface Geometric_Mean

        module procedure geometric_vector
        module procedure geometric_matrix

    end interface Geometric_Mean

    interface Harmonic_Mean

        module procedure harmonic_vector
        module procedure harmonic_matrix

    end interface Harmonic_Mean


    contains

        function arithmetic_vector ( vector ) result( amean )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16) :: amean

            amean = SUM(vector) / size(vector)

        end function arithmetic_vector

        function arithmetic_matrix ( Matrix ) result( amean )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16) :: amean

            amean = arithmetic_vector(RESHAPE(Matrix,(/ size(Matrix) /)))

        end function arithmetic_matrix

        function geometric_vector ( vector ) result( gmean )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16) :: gmean

            gmean = EXP(arithmetic_vector(LOG(vector)))

        end function geometric_vector

        function geometric_matrix ( Matrix ) result( gmean )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16) :: gmean

            gmean = geometric_vector(RESHAPE(Matrix,(/ size(Matrix) /)))

        end function geometric_matrix

        function harmonic_vector ( vector ) result( hmean )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16) :: hmean

            hmean = size(vector) / SUM(1 / vector)

        end function harmonic_vector

        function harmonic_matrix ( Matrix ) result( hmean )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16) :: hmean

            hmean = harmonic_vector(RESHAPE(Matrix,(/ size(Matrix) /)))

        end function harmonic_matrix


end module Statistics

module HistogramPlot

    implicit none

    interface Histogram

        module procedure histogram_vector
        module procedure histogram_matrix

    end interface Histogram


    contains

        subroutine histogram_vector ( vector )

            implicit none

            real(16),intent(in) :: vector(:)
            integer(8) :: i,s,n = 0
            real(16) :: limit,interval
            character( len = 6 ) :: lowerbound,upperbound
            character( len = 16 ) :: filename
            real(16),allocatable :: group(:,:)
            integer(8),allocatable :: p(:)


        end subroutine histogram_vector

        subroutine histogram_matrix ( Matrix )

            implicit none

            real(16),intent(in) :: Matrix(:,:)


        end subroutine histogram_matrix

        subroutine Plot ( Y,Z )

            implicit none

            real(16) :: Y(:)
            real(16) :: Z(:)
            real(16) :: X(size(Y))
            integer :: i


        end subroutine Plot


end module HistogramPlot

module Postprocessor

    use LAPACKOperators
    use HistogramPlot

    implicit none

    interface Stats

        module procedure Stats_vector
        module procedure Stats_matrix

    end interface Stats


    contains

        subroutine Stats_vector ( vector,M,StdDev )

            implicit none

            real(16),intent(in) :: vector(:)
            real(16),intent(out) :: M,StdDev
            real(16) :: s,ssq
            integer :: i,n


        end subroutine Stats_vector

        subroutine Stats_matrix ( Matrix,M,StdDev )

            implicit none

            real(16),intent(in) :: Matrix(:,:)
            real(16),intent(out) :: M,StdDev


        end subroutine Stats_matrix

        subroutine EIGENRECORD ( M )

            implicit none

            real(16),intent(in) :: M(:,:)
            real(16) :: E(size(M,1)),COND
            integer :: i = 1


        end subroutine EIGENRECORD

        subroutine EigenAnalysis (  )

            implicit none

            real(16) :: E(row + 1,50),COND(50)
            integer :: IOstatus,i,n


        end subroutine EigenAnalysis


end module Postprocessor

module LPTools

    use Augment
    use ProbabilityGeneration
    use LAOperators

    implicit none

    contains

        function Potential ( c,x ) result( f )

            implicit none

            real(16),intent(in) :: c(column + 1),x(column + 1)
            real(16) :: f


        end function Potential

        function zero_ratio ( n,c_unit ) result( alpha )

            implicit none

            integer,intent(in) :: n
            real(16),intent(in) :: c_unit(n)
            real(16) :: alpha,a
            real(16) :: beta
            integer :: idx


        end function zero_ratio

        function min_ratio ( n,c_unit ) result( alpha )

            implicit none

            integer,intent(in) :: n
            real(16),intent(in) :: c_unit(n)
            real(16) :: alpha
            real(16) :: beta
            integer :: idx


        end function min_ratio

        subroutine Dual ( A,b,c,A_dual,b_dual,c_dual )

            implicit none

            real(16),intent(in) :: A(:,:),b(:),c(:)
            real(16),intent(out) :: A_dual(size(A,2),size(A,1)),b_dual(size(c)),c_dual(size(b))


        end subroutine Dual

        subroutine StdToCan ( A_std,b_std,c_std,A_can,c_can,a0 )

            implicit none

            real(16),intent(in) :: A_std(:,:),b_std(:),c_std(:)
            real(16),intent(out) :: A_can(size(A_std,1) + size(A_std,2) + 1,2 * ( size(A_std,1) + size(A_std,2) + 1 )),c_can(2 * ( size(A_std,1) + size(A_std,2) + 1 )),a0(2 * ( size(c_std) + size(b_std) ) + 1)
            real(16) :: A(size(A_std,1) + size(A_std,2) + 1,2 * ( size(A_std,1) + size(A_std,2) ) + 1),b(size(A_std,1) + size(A_std,2) + 1),c(2 * ( size(A_std,1) + size(A_std,2) ) + 1)
            real(16) :: x0(size(A_std,2)),y0(size(A_std,1)),u0(size(A_std,1)),v0(size(A_std,2)),lambda_0
            integer :: m,n,i,j


        end subroutine StdToCan

        subroutine Transform ( A,b,c,A_can,c_can,a0 )

            implicit none

            real(16),intent(in) :: A(:,:),b(:),c(:),a0(:)
            real(16),intent(out) :: A_can(size(A,1),size(A,2) + 1),c_can(size(c) + 1)


        end subroutine Transform

        function InvTransform ( x_can,x0 ) result( x )

            implicit none

            real(16),intent(in) :: x_can(:),x0(:)
            real(16) :: x(size(x_can) - 1)


        end function InvTransform


end module LPTools

module LAOperators

    implicit none

    contains

        function DIAG ( x ) result( D )

            implicit none

            real(16),intent(in) :: x(:)
            real(16) :: D(size(x),size(x))
            integer :: i

            D = 0

            do i = 1,size(x)

                D(i,i) = x(i)

            end do


        end function DIAG

        function ONES ( n ) result( D )

            implicit none

            integer,intent(in) :: n
            real(16) :: D(n,n)
            real(16) :: e(n)


        end function ONES

        function COLMULT ( c,A ) result( cA )

            implicit none

            real(16),intent(in) :: c(:),A(:,:)
            real(16) :: cA(size(A,1),size(A,2))
            integer :: i


        end function COLMULT

        function ADD ( v ) result( a )

            implicit none

            real(16),intent(in) :: v(:)
            real(16) :: a
            integer :: i


        end function ADD

        function DOT ( b,c ) result( bTc )

            implicit none

            real(16),intent(in) :: b(:),c(:)
            real(16) :: bTc


        end function DOT

        function ENORM ( u ) result( n )

            implicit none

            real(16) :: u(:)
            real(16) :: n


        end function ENORM

        function UPPER ( A ) result( U )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16) :: U(size(A,1),size(A,2))
            integer :: i


        end function UPPER

        function LOWER ( A ) result( L )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16) :: L(size(A,1),size(A,2))
            integer :: i


        end function LOWER

        function CholeskyDecomp ( A ) result( L )

            implicit none

            real(16) :: A(:,:)
            real(16) :: L(size(A,1),size(A,2))
            integer :: i
            real(16) :: summ


        end function CholeskyDecomp

        subroutine SPDLUD ( A,L,U )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16),intent(out) :: L(size(A,1),size(A,2)),U(size(A,1),size(A,2))
            integer :: q,n,i,j
            real(16) :: D(size(A,1)),Dia(size(A,1),size(A,2))


        end subroutine SPDLUD

        function ForSubstitution ( L,b ) result( x )

            implicit none

            real(16) :: L(:,:),b(:)
            real(16) :: x(size(L,1))
            integer :: i


        end function ForSubstitution

        function BackSubstitution ( U,b ) result( x )

            implicit none

            real(16) :: U(:,:),b(:)
            real(16) :: x(size(U,1))
            integer :: i


        end function BackSubstitution

        function GEMV ( M,v ) result( Mv )

            implicit none

            real(16) :: M(:,:),v(:)
            real(16) :: Mv(size(M,1))
            integer :: i


        end function GEMV

        function GEMM ( A,B ) result( AB )

            implicit none

            real(16),intent(in) :: A(:,:),B(:,:)
            real(16) :: AB(size(A,1),size(B,2))
            integer :: i,j


        end function GEMM

        function TRANS ( A ) result( AT )

            implicit none

            real(16),intent(in) :: A(:,:)
            real(16) :: AT(size(A,2),size(A,1))
            integer :: i,j


        end function TRANS


end module LAOperators

module LAPACKOperators

    implicit none

    contains

        function DOT ( SX,SY ) result( uTv )

            implicit none

            real(8),intent(in) :: SX(:),SY(:)
            real(8) :: uTv
            integer(8) :: N,INCX,INCY


        end function DOT

        function ENORM ( X ) result( NormV )

            implicit none

            real(8),intent(in) :: X(:)
            real(8) :: NormV
            integer(8) :: N,INCX


        end function ENORM

        function DPOINV ( A ) result( AINV )

            implicit none

            real(8),intent(in) :: A(:,:)
            real(8) :: AINV(size(A,1),size(A,1))
            character :: UPLO = 'L'
            integer :: N,LDA,IPIV(size(A,1)),INFO,i,j


        end function DPOINV

        function EIGEN ( A ) result( E )

            implicit none

            real(8) :: A(:,:)
            real(8) :: E(size(A,1))
            character :: JOBZ = 'N', UPLO = 'L'
            integer :: N,LDA,LWORK,INFO
            real(8),allocatable :: WORK(:)


        end function EIGEN

        function LAGEMV ( A,X ) result( AX )

            implicit none

            real(8),intent(in) :: A(:,:),X(:)
            real(8) :: AX(size(A,1))
            character :: TRANS = 'N'
            integer(8) :: M,N,LDA,INCX = 1,INCY = 1
            real(8) :: ALPHA = 1.0,BETA = 0.0


        end function LAGEMV

        function LAGEMM ( A,B ) result( C )

            implicit none

            real(8),intent(in) :: A(:,:),B(:,:)
            real(8) :: C(size(A,1),size(B,2))
            character :: TRANSA = 'N', TRANSB = 'N'
            integer(8) :: M,N,K,LDA,LDB,LDC
            real(8) :: ALPHA = 1.0,BETA = 0.0


        end function LAGEMM


end module LAPACKOperators

module Augment

    implicit none

    contains

        function HAugment_vv ( a,b ) result( ab )

            implicit none

            real(16),intent(in) :: a(:),b(:)
            real(16) :: ab(size(a),2)

            ab(:,1) = a(:)
            ab(:,2) = b(:)

        end function HAugment_vv

        function HAugment_Mv ( A,b ) result( Ab )

            implicit none

            real(16),intent(in) :: A(:,:),b(:)
            real(16) :: Ab(size(b),size(A,2) + 1)

            Ab(:,:size(A,2)) = A
            Ab(:,size(A,2) + 1) = b

        end function HAugment_Mv

        function HAugment_vM ( a,B ) result( aB )

            implicit none

            real(16),intent(in) :: a(:),B(:,:)
            real(16) :: aB(size(a),1 + size(B,2))

            aB(:,1) = a
            aB(:,2:) = B

        end function HAugment_vM

        function HAugment_MM ( A,B ) result( AB )

            implicit none

            real(16),intent(in) :: A(:,:),B(:,:)
            real(16) :: AB(size(A,1),size(A,2) + size(B,2))

            AB(:,:size(A,2)) = A
            Ab(:,size(A,2) + 1:size(A,2) + size(B,2)) = b

        end function HAugment_MM

        function VAugment_ss ( a,b ) result( ab )

            implicit none

            real(16),intent(in) :: a,b
            real(16) :: ab(2)

            ab(1) = a
            ab(2) = b

        end function VAugment_ss

        function VAugment_sv ( s,v ) result( sv )

            implicit none

            real(16),intent(in) :: s,v(:)
            real(16) :: sv(1 + size(v))

            sv(1) = s
            sv(2:) = v

        end function VAugment_sv

        function VAugment_vs ( v,s ) result( vs )

            implicit none

            real(16),intent(in) :: v(:),s
            real(16) :: vs(size(v) + 1)

            vs(:size(v)) = v
            vs(size(v) + 1) = s

        end function VAugment_vs

        function VAugment_vv ( a,b ) result( ab )

            implicit none

            real(16),intent(in) :: a(:),b(:)
            real(16) :: ab(size(a) + size(b))

            ab(:size(a)) = a
            ab(size(a) + 1:size(a) + size(b)) = b

        end function VAugment_vv

        function VAugment_Mv ( A,b_T ) result( Ab_T )

            implicit none

            real(16),intent(in) :: A(:,:),b_T(:)
            real(16) :: Ab_T(size(A,1) + 1,size(A,2))

            Ab_T(:size(A,1),:) = A
            Ab_T(size(A,1) + 1,:) = b_T

        end function VAugment_Mv

        function VAugment_vM ( a_T,B ) result( a_TB )

            implicit none

            real(16),intent(in) :: a_T(:),B(:,:)
            real(16) :: a_TB(1 + size(B,1),size(B,2))

            a_TB(1,:) = a_T
            a_TB(2:1 + size(B,1),:) = B

        end function VAugment_vM

        function VAugment_MM ( A,B ) result( AB )

            implicit none

            real(16),intent(in) :: A(:,:),B(:,:)
            real(16) :: AB(size(A,1) + size(B,1),size(A,2))

            AB(:size(A,1),:) = A
            AB(size(A,1) + 1:size(A,1) + size(B,1),:) = B

        end function VAugment_MM


end module Augment

module GeoGebra

    implicit none

    contains

        subroutine GGBInit (  )

            implicit none

            real(16) :: X(3) = (/ 1,0,0 /),Y(3) = (/ 0,1,0 /),Z(3) = (/ 0,0,1 /)
            integer :: i,j


        end subroutine GGBInit

        subroutine GGBPlot (  )

            implicit none


        end subroutine GGBPlot

        subroutine GGBPlotSystem ( A,c,b )

            implicit none

            real(16) :: A(:,:),c(:)
            real(16) :: b(:)
            integer :: i
            character( len = 10 ) :: str(3)
            character( len = 100 ) :: vstr


        end subroutine GGBPlotSystem

        subroutine GGBPlotPoint ( v,label,color )

            implicit none

            real(16) :: v(3)
            character( len = 10 ) :: label
            character( len = 6 ) :: color
            character( len = 10 ) :: str(3)
            character( len = 100 ) :: vstr


        end subroutine GGBPlotPoint

        subroutine GGBPlotVector ( v )

            implicit none

            real(16) :: v(3)
            character( len = 10 ) :: str(3)
            character( len = 100 ) :: vstr


        end subroutine GGBPlotVector

        subroutine GGBPlotPlane ( c,b )

            implicit none

            real(16) :: c(3),b
            character( len = 10 ) :: str(4)
            character( len = 50 ) :: vstr


        end subroutine GGBPlotPlane

        subroutine GGBLabel ( label )

            implicit none

            character( len = 10 ) :: label


        end subroutine GGBLabel

        subroutine GGBCommand ( str )

            implicit none

            character( len = 10 ),intent(in) :: str


        end subroutine GGBCommand


end module GeoGebra

