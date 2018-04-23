PROGRAM CROUT
	implicit none
	real(8),allocatable,dimension(:,:):: matriz
	real(8),allocatable,dimension(:):: b
	integer:: i,j,k,n,m
	real(8):: sum

	! define semiancho en las funciones



	! open the file
	open(unit=24,file="matriz_b.txt")

	! lookout we have to transpose the matrix, so the indexs switches
	read(24,*) m, n

	! look the memory
	allocate(matriz(n,m),b(n))

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
	! system solution
	do i=2,n
		sum = 0
		do j=i-l(i),i-1
			sum = sum + matriz(i,band(i,j))*b(j)
		enddo
		b(i) = b(i) - sum
	enddo

	do i=1,n
		b(i) = b(i) / matriz(i,band(i,i))
	enddo

	do i=n,2,-1
		do j=i-u(i),i-1
			b(j) = b(j) - matriz(j,band(j,i)) * b(i)
		enddo
	enddo


	! print the matrix after crout
	write(6,*) "*********** After crout"
	write(6,*) "MATRIX ->"
	do i=1,n
		write(6,'(*(f7.4,2x))') (matriz(i,j),j=1,m)
	enddo
	write(6,*) "B ->"
	write(6,'(*(f7.4,2x))') (b(i),i=1,n)

	read(5,*)


	CONTAINS

	integer function l(i)
		integer:: i
		l = min(n,i-1)
		return
	end function l

	integer FUNCTION u(j)
		integer:: j
		u = min(n,j-1)
		return
	END FUNCTION

	integer FUNCTION band(i,j)
		integer:: i,j
		band = (j-i)+n+1
		return
	END FUNCTION

END PROGRAM

