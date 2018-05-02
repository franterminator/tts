PROGRAM Matrices
    IMPLICIT NONE
    ! bucles
    INTEGER :: i, j, p, it
    !factorizacion
    integer:: n, m, u, l, band
    real(8):: sum
    ! resolucion matriz
    REAL(8), dimension(:,:), allocatable :: matriz
    real(8), dimension(:), allocatable:: z
    ! datos
    real(8):: deltax,deltay,a,b,c,d,w
    real(8):: long,ancho,h,k,dx,dy,te,tf,deltat,tfinal

    ! get the data form user
    call data(long,ancho,h,k,dx,dy,te,tf,tfinal,deltat,n,m)

    ! calcular a,b,c,d,w
    deltax = long / n
    deltay = ancho / m
    a = -dx/deltax**2
    b = -dy/deltay**2
    c = 1/deltat
    d = 1/deltat - 2 * a - 2 * b
    w = 2 * b * deltay * h / k
    write(6,'(a,e12.4,a,e12.4,a,e12.4,a,e12.4,a,e12.4,a)') "[A,B,C,D,W] = [",a,", ",b,", ",c,", ",d,", ",w,"]"

    
    WRITE(6,'(a,i2,a,i2)') "Los datos introducidos son: ",n," // ",m


    !allocate de matrix
    allocate(matriz(n*m,2*n+1), z(n*m))

    ! ---------------------------------------------------
    ! GENERACION MATRIZ
    ! ---------------------------------------------------

    call generacion_matriz(matriz,a,b,c,d,w,n,m)

    write(6,*) "Matriz -------"
    call print_matrix(6,matriz,n,m)


    ! ---------------------------------------------------
    ! GENERACION VECTOR INDEPENDIENTES
    ! ---------------------------------------------------
    z(:) = te
    write(6,*) "Temperatura ******* "
    call print_vector(6,z,n,m)
    
    call generacion_vector(z,a,b,c,w,te,tf,n,m)

    write(6,*) "Vector -------"
    do j=m,1,-1
        do i=1,n
            p = i + (j-1)*n
            write(6,'(e10.4,6x)',advance="no") z(p)
        enddo
        write(6,*)
    enddo
    
    write(6,*) 

    ! ---------------------------------------------------
    ! FACTORIZACION
    ! ---------------------------------------------------
    CALL CROUT(matriz,n,m)

    ! print the matrix after crout
    write(6,*) "*********** After crout"
    write(6,*) "MATRIX ->"
    call print_matrix(6,matriz,n,m)


    ! ---------------------------------------------------
    ! SOLVE
    ! ---------------------------------------------------
    
    it = 1
    do while(deltat*it <= tfinal)
        CALL SOLVE(matriz,z,n,m)
        

        write(6,'(a,i2,a,f7.2,a,f7.2)') "Temperaturas -> iteracion i = ",it," -- tiempo = ",deltat*it,"//",tfinal
        call print_vector(6,z,n,m)
        call write_file(it,z,n,m)

        CALL generacion_vector(z,a,b,c,w,te,tf,n,m)

        it = it + 1
    enddo


    
    




    ! THE END

    read(5,*)


    
END PROGRAM













INTEGER FUNCTION l(i,n)
    integer:: i,n
    l = min(n,i-1)
    return
END FUNCTION



FUNCTION u(j,n)
    integer:: u,j,n
    u = min(n,j-1)
    return
END FUNCTION



FUNCTION band(i,j,n)
    integer:: band,i,j,n
    band = (j-i)+n+1
    return
END FUNCTION




SUBROUTINE data(long,ancho,h,k,dx,dy,te,tf,tfinal,deltat,n,m)
    IMPLICIT none
    character(len=100):: option
    character(len=32):: label, equal
    real(8),intent(inout):: long,ancho,h,k,dx,dy,te,tf,tfinal,deltat
    integer,intent(inout):: n,m
    real(8):: datos
    integer:: sel
    integer:: error=0

    long=-1; ancho=-1;
    h=-1; k=-1; dx=-1; dy=-1
    deltat=-1; tfinal=-1
    te=-1; tf=-1
    n=-1; m=-1
    

    call getarg(1,option)

    if(option == "") then
        !pide los datos al usuario
        print *,"Introduzca los datos necesarios para ejecutar el programa: "
        print *,"---------- -------------------------------------------------"
        print *,"**************      DIMENSIONES      **********************"
        print *,"-----------------------------------------------------------"
        write(6,'(a)',advance="no") " -> numero de puntos en el largo (n) = "
        read(5,*) n
        write(6,'(a)',advance="no") " -> numero de puntos en el ancho (m) = "
        read(5,*) m
        write(6,'(a)',advance="no") " -> longitud de la placa [metros] = "
        read(5,*) long
        write(6,'(a)',advance="no") " -> ancho de la placa [metros] = "
        read(5,*) ancho

        print *,"-----------------------------------------------------------"
        print *,"**************      TEMPERATURAS      *********************"
        print *,"-----------------------------------------------------------"
        write(6,'(a)',advance="no") " -> temperatura exterior [grados Celsius] = "
        read(5,*) te
        write(6,'(a)',advance="no") " -> temperatura fuente [grados Celsius] = "
        read(5,*) tf
        write(6,'(a)',advance="no") " -> coeficiente  de disipacion (h) = "
        read(5,*) h
        write(6,'(a)',advance="no") " -> constante termica (k) = "
        read(5,*) k
        write(6,'(a)',advance="no") " -> coef de difusion a lo largo (dx) = "
        read(5,*) dx
        write(6,'(a)',advance="no") " -> coef de difusion a lo ancho (dy) = "
        read(5,*) dy

        print *,"-----------------------------------------------------------"
        print *,"**************         TIEMPO        **********************"
        print *,"-----------------------------------------------------------"
        write(6,'(a)',advance="no") " -> incremento de tiempo [s] = "
        read(5,*) deltat
        write(6,'(a)',advance="no") " -> tiempo final [s] = "
        read(5,*) tfinal

    else
        ! lee el archivo y pide los datos que faltan al usuario
        print *,"Se ha encontrado un archivo -> ",option
        open(unit=34, status="old", file=option)

        !LECTURA DEL ARCHIVO
        !primera linea comentario
        read(34,*,iostat=error) label

        !lectura de datos
        do while (error==0)
            ! texto = valor
            read(34,*,iostat=error) label,equal,datos
            ! escupe los datos
            write(6,*) "**************************"
            write(6,'(a,x,a8,2x,a,e11.4,x,a)') "|",trim(label)," -> ",datos,"|"


            ! parser
            if(label=="N" .or. label=="n") then
                n = int(datos)
            else if (label=="M" .or. label=="m") then
                m = int(datos)
            else if(label == "Long" .or. label == "long") then
                long = datos
            else if(label == "Ancho" .or. label == "ancho") then
                ancho = datos
            else if(label == "Te" .or. label == "te") then
                te = datos
            else if(label == "Tf" .or. label == "tf") then
                tf = datos
            else if(label == "H" .or. label == "h") then
                h = datos
            else if(label == "K" .or. label == "k") then
                k = datos
            else if(label == "Dx" .or. label == "dx") then
                dx = datos
            else if(label == "Dy" .or. label == "dy") then
                dy = datos
            else if(label == "DeltaT" .or. label == "deltaT" .or. label=="deltat") then
                deltat = datos
            else if(label == "TFinal" .or. label == "Tfinal" .or. label=="tfinal") then
                tfinal = datos
            endif
        end do !fin de bucle lectura de datos



        print *,"-----------------------------------------------------------"
        print *,"         -> SI FALTA ALGUN DATO SE PIDE A CONTINUACION"
        print *,"-----------------------------------------------------------"

        if(n==-1) then
            write(6,'(a)',advance="no") " -> numero de puntos en el largo (n) = "
            read(5,*) n
        endif
        if (m==-1) then
            write(6,'(a)',advance="no") " -> numero de puntos en el ancho (m) = "
            read(5,*) m
        endif
        if (long==-1) then
            write(6,'(a)',advance="no") " -> longitud de la placa [metros] = "
            read(5,*) long
        endif
        if (ancho==-1) then
            write(6,'(a)',advance="no") " -> ancho de la placa [metros] = "
            read(5,*) ancho
        endif
        if (te==-1) then
            write(6,'(a)',advance="no") " -> temperatura exterior [grados Celsius] = "
            read(5,*) te
        endif
        if (tf==-1) then
            write(6,'(a)',advance="no") " -> temperatura fuente [grados Celsius] = "
            read(5,*) tf
        endif
        if (k==-1) then
            write(6,'(a)',advance="no") " -> contante termica [1 / grado Celsisus] = "
            read(5,*) k
        endif
        if (h==-1) then
            write(6,'(a)',advance="no") " -> contante termica de transfusion = "
            read(5,*) h
        endif
        if (dx==-1) then
            write(6,'(a)',advance="no") " -> coef de difusion a lo largo = "
            read(5,*) dx
        endif
        if (dy==-1) then
            write(6,'(a)',advance="no") " -> coef de difusion a lo ancho = "
            read(5,*) dy
        endif
        if (deltat==-1) then
            write(6,'(a)',advance="no") " -> incremento de tiempo [s] = "
            read(5,*) deltat
        endif
        if (tfinal==-1) then
            write(6,'(a)',advance="no") " -> tiempo final [s] = "
            read(5,*) tfinal
        endif

        print *,"-----------------------------------------------------------"
        print *,"         -> SE HAN COMPLETADO TODOS LOS CAMPOS             "
        print *,"-----------------------------------------------------------"
        print *,"-----------------------------------------------------------"
        PRINT *,""

        close(34)
        
    endif


    ! pregunta por guardar los datos en un archivo de texto
    write(6,'(a,/,a,/,a,/,a)') "Desea guardar los datos introducidos?","1) Si","2) No","Introduzca la opcion 1 o 2:"
    read(5,*) sel
    if(sel==1) then
        error = 1
        do while(error /= 0)
            write(6,*) "    -> Nombre del archivo? (No olvide la extension):"
            read(5,*) label
            open(unit=40, status="new", iostat=error, file=label)
            if(error == 0) then
                write(6,*) "            Guardando........"
                write(40,'(a)',advance="no") "# Los espacios entre la etiqueta, el igual y el numero son importantes. " 
                write(40,'(a)',advance="no") "No debe haber espacio al principio de la linea. "
                write(40,'(a)') "El programa pedira los datos que faltan o estan incorrectos. "
                write(40,*) "n = ",n
                write(40,*) "m = ",m
                write(40,*) "Long = ",long
                write(40,*) "Ancho = ",ancho
                write(40,*) "Dx = ",dx
                write(40,*) "Dy = ",dy
                write(40,*) "h = ",h
                write(40,*) "K = ",k
                write(40,*) "Te = ",te
                write(40,*) "Tf = ",tf
                write(40,*) "deltaT = ",deltat
                write(40,*) "Tfinal = ",tfinal
            else
                write(6,'(/,a,/)') "!!!!Hay problemas al guardar el archivo. Comprueba que el archivo no exista antes!!!!"
                read(5,*)
            endif
        enddo
    endif

END SUBROUTINE








SUBROUTINE generacion_matriz (matriz,a,b,c,d,w,n,m) 
    integer,intent(in):: n,m
    real(8),intent(in):: a,b,c,d,w
    real(8),dimension(n*m,2*n+1),intent(inout):: matriz
    ! ---------------------------------------------------
    ! GENERACION
    ! ---------------------------------------------------

    ! ***************************** Generacion matriz
    matriz(:,:)= 0          !relleno la matriz de ceros
    ! b
    do i=n+1,n*m
        matriz(i,1) = b
    enddo

    ! 0,a...a,2a
    do j=1, m
        i = (j-1)*n + 1
        matriz(i+1:i+n-2,n) = a
        matriz(i+n-1,n) = 2*a
    enddo

    ! d-e ... d-e d ---- d
    do i=1,n*m
        if (i <= n) then
            matriz(i,n+1) = d-w
        else
            matriz(i,n+1) = d
        endif
    enddo

    ! a...a 0
    do j=1,m
        i = (j-1)*n + 1
        matriz(i:i+n-2,n+2) = a
    enddo

    ! 2b ... 2b b ... b
    do i=1,n*(m-1)
        if (i<=n) then
            matriz(i,2*n+1) = 2*b
        else
            matriz(i,2*n+1) = b
        endif

    enddo

END SUBROUTINE






SUBROUTINE generacion_vector(z,a,b,c,w,te,tf,n,m)
    integer,intent(in):: n,m
    real(8),intent(in):: a,b,c,w,te,tf
    real(8), dimension(n*m), intent(inout):: z


    do j=1,m
        i = 1 + (j-1)*n
        ! procedural 
        if (j==1) then
            ! c * u - a * tf - w * te
            z(i) = c*z(i) - a*tf - w*te
            ! c * u + w * te
            z(i+1:n*j) = c*z(i+1:n*j) - w*te

        else if (j==m) then
            ! c * u - a * tf - b * tf
            z(i) = c*z(i) - a*tf - b*tf
            ! c * u - b * tf
            z(i+1:n*j) = c*z(i+1:n*j) - b*tf
        else 
            ! c * u - a * tf
            z(i) = c*z(i) - a*tf
            ! c * u
            z(i+1:n*j) = c*z(i+1:n*j)
            
        endif
    enddo



END SUBROUTINE






SUBROUTINE CROUT(matriz,n,m)
    !variables SUBROUTINE
    integer,intent(in):: n,m
    real(8),dimension(n*m,n*m),intent(inout):: matriz
    integer:: i, j, k, u, l, band

    ! CROUT
    do k=1,n*m-1
        do i=max(2,k+1-u(k+1,n)), k
            sum = 0
            do j=max(i-l(i,n),k+1-u(k+1,n)), i-1
                sum = sum + matriz(i,band(i,j,n)) * matriz(j,band(j,k+1,n))
            enddo
            matriz(i,band(i,k+1,n)) = matriz(i,band(i,k+1,n)) - sum
        enddo
        do i=k+1-u(k+1,n), k
            matriz(i,band(i,k+1,n)) = matriz(i,band(i,k+1,n))/matriz(i,band(i,i,n))
        enddo

        do i=max(2,k+1-l(k+1,n)), k
            sum = 0
            do j=max(i-u(i,n),k+1-l(k+1,n)), i-1
                sum = sum + matriz(j,band(j,i,n)) * matriz(k+1,band(k+1,j,n))
            enddo
            matriz(k+1,band(k+1,i,n)) = matriz(k+1,band(k+1,i,n)) - sum
        enddo
        do i=k+1-l(k+1,n), k
            matriz(k+1,band(k+1,i,n)) = matriz(k+1,band(k+1,i,n)) / matriz(i,band(i,i,n))
        enddo

        sum = 0
        do j=max(k+1-l(k+1,n),k+1-u(k+1,n)), k
            sum = sum + matriz(k+1,band(k+1,j,n)) * matriz(j,band(j,j,n)) * matriz(j,band(j,k+1,n))
        enddo
        matriz(k+1,band(k+1,k+1,n)) = matriz(k+1,band(k+1,k+1,n)) - sum
    enddo

END SUBROUTINE CROUT







SUBROUTINE solve(matriz,b,n,m) 
    !variables subroutine
    integer,intent(in):: n,m
    real(8),dimension(n*m,2*n+1),intent(in):: matriz
    real(8),dimension(n*m),intent(inout):: b
    integer:: i, j, k, u, l, band


    ! system solution
    do i=2,n*m
        sum = 0
        do j=i-l(i,n),i-1
            sum = sum + matriz(i,band(i,j,n))*b(j)
        enddo
        b(i) = b(i) - sum
    enddo

    do i=1,n*m
        b(i) = b(i) / matriz(i,band(i,i,n))
    enddo

    do i=n*m,2,-1
        do j=i-u(i,n),i-1
            b(j) = b(j) - matriz(j,band(j,i,n)) * b(i)
        enddo
    enddo
END SUBROUTINE


SUBROUTINE print_matrix(unit,matriz,n,m)
    integer,intent(in):: unit,n,m
    real(8),dimension(n*m,2*n+1),intent(in):: matriz
    integer:: i,j
    
    do i=1,n*m
        write(unit,'(*(e10.4,2x))') (matriz(i,j),j=1,2*n+1)
    enddo

    write(unit,*)

END SUBROUTINE

SUBROUTINE print_vector(unit,vector,n,m)
    INTEGER,intent(in):: unit,n,m
    real(8), dimension(n*m), intent(in):: vector
    integer:: i,j,k

    do j=m,1,-1
        do i=1,n
            k = i + (j-1)*n
            write(unit,'(f6.2,2x)',advance="no") vector(k)
        enddo
        write(unit,*)
    enddo

    write(unit,*)

END SUBROUTINE

SUBROUTINE write_file(it,vector,n,m)
    integer,intent(in):: n,m,it
    real(8), dimension(n*m), intent(in):: vector
    logical:: existe
    character(len=20):: archivo="resultados.txt"

    inquire(file=archivo,exist=existe)
    if(.not. existe) then
        open(unit=55,file=archivo,status="new")
        write(55,*) "iteracion ---------------- ",it
        call print_vector(55,vector,n,m)
        close(55)
    else
        open(unit=55,file=archivo,status="old",access="append")
        write(55,*) "iteracion ---------------- ",it
        call print_vector(55,vector,n,m)
        close(55)
    endif

END SUBROUTINE