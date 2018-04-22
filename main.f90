PROGRAM Matrices
    IMPLICIT NONE
    INTEGER :: n, m, i, j, k
    REAL(8), dimension(:,:), allocatable :: matriz

    ! get the data form user
    WRITE(6,*) "N ="
    READ(5,*) n

    WRITE(6,*) "M = "
    READ(5,*) m
    
    WRITE(6,'(a,i2,a,i2)') "Los datos introducidos son: ",n," // ",m


    !allocate de matrix
    allocate(matriz(n*m,2*n+1))


    matriz(:,:)= 0			!relleno la matriz de ceros
    do i=n+1,n*m
    	matriz(i,1) = 1
    enddo

    do j=1, m
    	i = (j-1)*n + 1
        matriz(i+1:i+n-2,n) = 2
        matriz(i+n-1,n) = 3
    enddo

    do i=1,n*m
    	if (i <= n) then
    		matriz(i,n+1) = 4
    	else
    		matriz(i,n+1) = 5
    	endif
    enddo

    do j=1,m
        i = (j-1)*n + 1
        matriz(i:i+n-2,n+2) = 6
    enddo

    do i=1,n*(m-1)
    	if (i<=n) then
    		matriz(i,2*n+1) = 7
    	else
    		matriz(i,2*n+1) = 8
    	endif

    enddo


    do i=1,n*m
    	write(6,'(*(f4.2,2x))') (matriz(i,j), j=1,2*n+1)
    enddo 
    
    READ(5,*)


END PROGRAM Matrices
