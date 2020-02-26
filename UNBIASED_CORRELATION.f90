  SUBROUTINE UNBIASED_CORRELATION( N, A, B, msg, lag, r, t, m)

  ! Computes the correlation of two series A and B of length
  ! N as a function of lag. The correlation is unbiased
  ! because the sums in the covariance and standard devia-
  ! tions are divided by m, not m-1, where m is the number
  ! of overlapping grid points.

  ! The correlation r is defined as 
  !
  !           cov(A,B)
  !     r = -------------
  !          s(A) * s(B)
  !
  ! where cov() is covariance and s() is standard deviation.

  ! The series A and B must be prepared such that they
  ! are sent to this routine with zero lag. If necessary, this
  ! is accomplished by padding the beginning or ends (or both)
  ! of each time series with missing values (msg) so that their
  ! elements correspond one to one. The resultant time series
  ! will be of equal length N. Also, missing values (msg)
  ! distributed thoughout each time series is perfectly
  ! acceptable. Time series B will be shifted by the amount
  ! lag relative to time series A:

  ! RETURNS: r, the unbiased correlation, t, the significance
  ! of the unbiased correlation ( t is set to msg if B = A
  ! at lag 0, i.e. an autocorrelation at lag 0, or if the
  ! correlation is 1.0 in general), and m, the number of
  ! overlapping grid points.

  ! A:      t1  t2  t3  t4        ...       tN
  ! B:              t1  t2  t3  t4        ...       tN 

  !         \_ _/
  !           V
  !           lag = 2 (Defined to be > 0.) 

  IMPLICIT NONE

  INTEGER, INTENT(IN)             :: N
  REAL, DIMENSION(N), INTENT(IN)  :: A 
  REAL, DIMENSION(N), INTENT(IN)  :: B           
  REAL, INTENT(IN)                :: msg 
  INTEGER, INTENT(IN)             :: lag 
  REAL, INTENT(OUT)               :: r
  REAL, INTENT(OUT)               :: t
  INTEGER, INTENT(OUT)            :: m


  REAL, DIMENSION(:), ALLOCATABLE :: x
  REAL, DIMENSION(:), ALLOCATABLE :: y
  REAL, DIMENSION(:), ALLOCATABLE :: xdev
  REAL, DIMENSION(:), ALLOCATABLE :: ydev
  REAL, DIMENSION(:), ALLOCATABLE :: xdevydev
  REAL, DIMENSION(:), ALLOCATABLE :: xdevxdev
  REAL, DIMENSION(:), ALLOCATABLE :: ydevydev

  REAL                            :: xmn
  REAL                            :: ymn
  REAL                            :: COVxy
  REAL                            :: Sx
  REAL                            :: Sy


  ALLOCATE( x(1:3*N) )
  ALLOCATE( y(1:3*N) )

  x(:) = msg
  y(:) = msg

  x(N+1:2*N) = A(:)
  y(N+1:2*N) = B(:)

  y = EOSHIFT( y, SHIFT = -lag, BOUNDARY = msg )

  WHERE ( x == msg ) y = msg
  WHERE ( y == msg ) x = msg

  m = COUNT( x /= msg )

  IF ( m < 3 ) THEN

   WRITE (*,'(A40, I1)') "The number of overlapping points is m = ", m
   WRITE (*,'(A35)')     "The value of m should be 3 or more."
   WRITE (*,'(A17)')     "Decrease the lag."
   WRITE (*,'(A52)')     "Execution halted in SUBROUTINE UNBIASED_CORRELATION."
   STOP

  END IF

  xmn = SUM( x, DIM = 1, MASK = x /= msg ) / REAL(m)
  ymn = SUM( y, DIM = 1, MASK = y /= msg ) / REAL(m)

  ALLOCATE( xdev(1:3*N) )
  ALLOCATE( ydev(1:3*N) )

  xdev(:) = x(:) - xmn
  WHERE ( x == msg ) xdev(:) = msg

  ydev(:) = y(:) - ymn
  WHERE ( y == msg ) ydev(:) = msg

  ALLOCATE( xdevydev(1:3*N) )

  xdevydev(:) = xdev(:) * ydev(:)
  WHERE ( x == msg ) xdevydev(:) = msg
  
  COVxy = SUM( xdevydev, DIM = 1, MASK = xdevydev /= msg ) / REAL(m)

  ALLOCATE( xdevxdev(1:3*N) )

  xdevxdev(:) = xdev(:) * xdev(:)
  WHERE ( x == msg ) xdevxdev(:) = msg

  Sx = SQRT( SUM( xdevxdev, DIM = 1, MASK = xdevxdev /= msg ) / REAL(m) )

  ALLOCATE( ydevydev(1:3*N) )

  ydevydev(:) = ydev(:) * ydev(:)
  WHERE ( y == msg ) ydevydev(:) = msg

  Sy = SQRT( SUM( ydevydev, DIM = 1, MASK = ydevydev /= msg ) / REAL(m) )

  r = COVxy / ( Sx * Sy )

  IF ( r /= 1.0 ) THEN

    t = r * SQRT( (m - 2) / ( 1 - r*r ) )

  ELSE

    t = msg

  END IF

  DEALLOCATE( x, y, xdev, ydev, xdevydev, xdevxdev, ydevydev )

  END SUBROUTINE UNBIASED_CORRELATION