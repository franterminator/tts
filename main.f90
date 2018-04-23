PROGRAM Matrices
    IMPLICIT NONE
    INTEGER :: n, m, i, j, k
    real(8):: sum
    REAL(8), dimension(:,:), allocatable :: matriz
    real(8), dimension(:), allocatable:: b

    ! get the data form user
    WRITE(6,*) "N ="
    READ(5,*) n

    WRITE(6,*) "M = "
    READ(5,*) m
    
    WRITE(6,'(a,i2,a,i2)') "Los datos introducidos son: ",n," // ",m


    !allocate de matrix
    allocate(matriz(n*m,2*n+1),b(n*m))

    ! ---------------------------------------------------
    ! GENERACION
    ! ---------------------------------------------------

    ! ***************************** Generacion matriz
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
    		matriz(i,n+1) = 10
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

    write(6,*) "Matriz -------"
    do i=1,n*m
    	write(6,'(*(f5.2,2x))') (matriz(i,j), j=1,2*n+1)
    enddo 



    ! ***************************** Generacion vector independientes
    b(:) = 0

    do j=1,m
        i = 1 + (j-1)*n
        ! procedural 
        if (j==1) then
            b(i) = 1
            b((j-1)*n+2:n*j) = 2
        else if (j==m) then
            b(i) = 5
            b((j-1)*n+2:n*j) = 6
        else 
            b(i) = 3
            b((j-1)*n+2:n*j) = 4
        endif
    enddo

    write(6,*) "Vector -------"
    do i=1,n*m
        write(6,'(f4.2)') b(i)
    enddo 







    ! ---------------------------------------------------
    ! FACTORIZACION
    ! ---------------------------------------------------
    
! CROUT
    do k=1,n*m-1

        do i=max(2,k+1-u(k+1)), k
            sum = 0
            do j=max(i-l(i),k+1-u(k+1)), i-1
                sum = sum + matriz(i,band(i,j)) * matriz(j,band(j,k+1))
            enddo
            matriz(i,band(i,k+1)) = matriz(i,band(i,k+1)) - sum
        enddo
        do i=k+1-u(k+1), k
            matriz(i,band(i,k+1)) = matriz(i,band(i,k+1))/matriz(i,band(i,i))
        enddo

        do i=max(2,k+1-l(k+1)), k
            sum = 0
            do j=max(i-u(i),k+1-l(k+1)), i-1
                sum = sum + matriz(j,band(j,i)) * matriz(k+1,band(k+1,j))
            enddo
            matriz(k+1,band(k+1,i)) = matriz(k+1,band(k+1,i)) - sum
        enddo
        do i=k+1-l(k+1), k
            matriz(k+1,band(k+1,i)) = matriz(k+1,band(k+1,i)) / matriz(i,band(i,i))
        enddo

        sum = 0
        do j=max(k+1-l(k+1),k+1-u(k+1)), k
            sum = sum + matriz(k+1,band(k+1,j)) * matriz(j,band(j,j)) * matriz(j,band(j,k+1))
        enddo
        matriz(k+1,band(k+1,k+1)) = matriz(k+1,band(k+1,k+1)) - sum
    enddo


    ! system solution
    do i=2,n*m
        sum = 0
        do j=i-l(i),i-1
            sum = sum + matriz(i,band(i,j))*b(j)
        enddo
        b(i) = b(i) - sum
    enddo

    do i=1,n*m
        b(i) = b(i) / matriz(i,band(i,i))
    enddo

    do i=n*m,2,-1
        do j=i-u(i),i-1
            b(j) = b(j) - matriz(j,band(j,i)) * b(i)
        enddo
    enddo


    ! print the matrix after crout
    write(6,*) "*********** After crout"
    write(6,*) "MATRIX ->"
    do i=1,n*m
        write(6,'(*(f8.4,2x))') (matriz(i,j),j=1,n*m)
    enddo
    write(6,*) "B ->"
    write(6,'(*(f7.4,2x))') (b(i),i=1,n*m)




    ! THE END

    read(5,*)

    CONTAINS

    INTEGER FUNCTION l(i)
        integer:: i
        l = min(n,i-1)
        return
    END FUNCTION

    FUNCTION u(j)
        integer:: u,j
        u = min(n,j-1)
        return
    END FUNCTION

    FUNCTION band(i,j)
        integer:: band,i,j
        band = (j-i)+n+1
        return
    END FUNCTION
END PROGRAM

