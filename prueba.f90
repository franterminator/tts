program prueba
	integer:: i,j,k,n
	n = 2
	i = 2
	j = 4
	k = suma(i,j)

	write(6,*) i,j,n,k

	read(5,*)

	contains

 	integer function suma(i,j)
	    integer:: a
	    write(6,*) n
	    suma = i + j
	    return
	end function suma

end program prueba