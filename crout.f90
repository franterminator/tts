PROGRAM CROUT
	implicit none
	!real(8),allocatable,dimension(:,:):: matriz
	real(8),dimension(5,5):: matriz
	!real(8),allocatable,dimension(:):: b
	real(8),dimension(5):: b
	integer:: i,j,k,n,m,l,u
	real(8):: sum

	! define semiancho en las funciones



	! open the file
	open(unit=24,file="matriz.txt")

	! lookout we have to transpose the matrix, so the indexs switches
	read(24,*) m, n

	! look the memory
	!allocate(matriz(n,m),b(n))

	! fill the matrix with ceros
	matriz(:,:) = 0

	! read the matrix
	read(24,*) matriz 
	matriz = transpose(matriz)

	! blank line
	read(24,*)

	! b
	read(24,*) b 
	close(24)

	! print the matrix before CROUT
	write(6,*) "*********** Before crout"
	write(6,*) "MATRIX ->"
	do i=1,n
		write(6,'(*(f7.4,2x))') (matriz(i,j),j=1,m)
	enddo
	write(6,*) "B ->"
	write(6,'(*(f7.4,2x))') (b(i),i=1,n)


	! CROUT
	do k=1,n-1

		do i=max(2,k+1-u(k+1)), k
			sum = 0
			do j=max(i-l(i),k+1-u(k+1)), i-1
				sum = sum + matriz(i,j) * matriz(j,k+1)
			enddo
			matriz(i,k+1) = matriz(i,k+1) - sum
		enddo
		do i=k+1-u(k+1), k
			matriz(i,k+1) = matriz(i,k+1)/matriz(i,i)
		enddo

		do i=max(2,k+1-l(k+1)), k
			sum = 0
			do j=max(i-u(i),k+1-l(k+1)), i-1
				sum = sum + matriz(j,i) * matriz(k+1,j)
			enddo
			matriz(k+1,i) = matriz(k+1,i) - sum
		enddo
		do i=k+1-l(k+1), k
			matriz(k+1,i) = matriz(k+1,i) / matriz(i,i)
		enddo

		sum = 0
		do j=max(k+1-l(k+1),k+1-u(k+1)), k
			sum = sum + matriz(k+1,j) * matriz(j,j) * matriz(j,k+1)
		enddo
		matriz(k+1,k+1) = matriz(k+1,k+1) - sum
	enddo

	! system solution
	do i=2,n
		sum = 0
		do j=i-l(i),i-1
			sum = sum + matriz(i,j)*b(j)
		enddo
		b(i) = b(i) - sum
	enddo

	do i=1,n
		b(i) = b(i) / matriz(i,i)
	enddo

	do i=n,2,-1
		do j=i-u(i),i-1
			b(j) = b(j) - matriz(j,i) * b(i)
		enddo
	enddo


	! print the matrix after crout
	write(6,*) "*********** After crout"
	write(6,*) "MATRIX ->"
	do i=1,n
		write(6,'(*(f8.4,2x))') (matriz(i,j),j=1,m)
	enddo
	write(6,*) "B ->"
	write(6,'(*(f7.4,2x))') (b(i),i=1,n)

	read(5,*)
END PROGRAM

FUNCTION l(i)
	integer:: l,i
	l = min(2,i-1)
	return
END FUNCTION

FUNCTION u(j)
	integer:: u,j
	u = min(2,j-1)
	return
END FUNCTION