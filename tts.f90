
! El programa TTS (Temperature Time Solver) calcula la temperatura de una placa en contacto con una fuente de calor en cada instante de tiempo. 
! El usuario puede definir las propiedades de la placa, temperatura de la fuente, temperatura del medio exterior y tiempos de cálculo deseados.

PROGRAM TTS
	! ----------------------------------------------------------------------------------------------------- VARIABLES

    IMPLICIT NONE !elimina las variables implicitas

    INTEGER :: it 			!guarda la iteracion 
    integer:: n, m 			!numero de puntos a lo largo y a lo ancho
    integer:: u, l, band 	!matriz en banda
    					  	!band -> transformacion de matriz ordinaria a matriz en banda
    					  	!u(i) -> funcion que devuelve la posicion de banda superior
    					  	!l(i) -> funcion que devuelve la posicion de la banda inferior 

    REAL(8), dimension(:,:), allocatable :: matriz 	!matriz con las ecuaciones del sistema
    real(8), dimension(:), allocatable:: z			!vector de terminos independientes    
    real(8):: a,b,c,d,W 								!elementos de la matriz
    real(8):: deltax,deltay					!separacion entre los puntos a lo largo (x) y a lo ancho(y)

    ! datos del programa
    real(8):: long,ancho 					!dimensiones de la placa
    real(8):: h 							!coef de transmision superficial
    real(8):: k 							!conductividad termica
    real(8):: dx,dy 						!coef de difusividad en x e y
    real(8):: te,tf 						!temperatura medio exterior y fuente de calor
    real(8):: deltat,tfinal 				!intervalos de tiempo y tiempo final


	! ----------------------------------------------------------------------------------------------------- PROGRAMA

    ! abrimos el archivo de resultados
    open(unit=55,file="resultados.txt",status="replace")

    ! pedimos al usuario o leemos de un archivo los datos de entrada
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


    ! reservamos memoria para las matrices
    allocate(matriz(n*m,2*n+1), z(n*m))

    ! ---------------------------------------------------
    ! GENERACION MATRIZ
    ! ---------------------------------------------------

    call generacion_matriz(matriz,a,b,c,d,w,n,m)	!coloca los coeficientes de la matriz en su sitio

    write(6,*) "Matriz -------"
    call print_matrix(6,matriz,n,m) 				!imprime las coeficientes de la matriz


    ! ---------------------------------------------------
    ! GENERACION VECTOR INDEPENDIENTES
    ! ---------------------------------------------------
    z(:) = te 										!inicialmente la placa esta a temperatura exterior
    write(6,*) "Temperatura ******* "
    call print_temperatures(6,z,tf,n,m)		 		!imprime la temperatura de la placa
    
    call generacion_vector(z,a,b,c,w,te,tf,n,m)		!calcula y coloca los coeficientes de terminos independientes en su sitio

    write(6,*) "Vector -------"
    call print_vector(6,z,n,m)						!imprime el vector con un formato exponencial
    
    write(6,*) 

    ! ---------------------------------------------------
    ! FACTORIZACION
    ! ---------------------------------------------------
    
    CALL CROUT(matriz,n,m)

    ! print the matrix after crout
    write(6,*) "*********** Matriz factorizada"
    write(6,*) "MATRIX ->"
    call print_matrix(6,matriz,n,m)


    ! ---------------------------------------------------
    ! RESOLUCION DEL SISTEMA
    ! ---------------------------------------------------
    
    it = 1

    ! resuelve el sistema hasta que se alcance el tiempo indicado por el usuario
    do while(deltat*it <= tfinal)
    	! resuelve el sistema
        CALL SOLVE(matriz,z,n,m)

        ! imprime en pantalla los resultados
        write(6,'(a,i6,a,f10.2,a,f10.2)') "Temperaturas -> iteracion i = ",it," -- tiempo = ",deltat*it,"//",tfinal
        call print_temperatures(6,z,tf,n,m)
        ! imprime en archivo los resultados
        write(55,*) "-> iteracion ---------",it
        call print_temperatures(55,z,tf,n,m)

        ! regenera el vector con las nuevas temperaturas
        CALL generacion_vector(z,a,b,c,w,te,tf,n,m)

        ! siguiente iteracion
        it = it + 1
    enddo


    ! opcional para ejecutar el programa grapher.py desde fortran
    !write(6,*) "Ejecutando programa para mostrar resultados graficos."
    !call execute_command_line("python grapher.py resultados.txt", wait=.false.)



    ! THE END
    write(6,'(/,a,/,a)') "Gracias por usar el programa....."," Pulse ENTER para salir."
    read(5,*)


    
END PROGRAM TTS




! ----------------------------------------------------------------------------------------------------- SUBROUTINAS Y FUNCIONES







! **********************************************************************************
! FUNCION l(i,n)
! ---------> Argumentos:
!               i : posicion de un elemento de la diagonal
!               n : ancho de la matriz
! Descripcion:
! 	devuelve la posicion de la banda inferior para un elemento i de la diagonal
!
! ---------> Devuelve:
!               l : la posicion de la banda inferior 
! **********************************************************************************
INTEGER FUNCTION l(i,n)
    integer:: i,n
    l = min(n,i-1)
    return
END FUNCTION





! **********************************************************************************
! FUNCION u(j,n)
! ---------> Argumentos:
!               j : posicion de un elemento de la diagonal
!               n : ancho de la matriz
! Descripcion:
! 	devuelve la posicion de la banda superior para un elemento j de la diagonal
!
! ---------> Devuelve:
!               l : la posicion de la banda superior 
! **********************************************************************************
FUNCTION u(j,n)
    integer:: u,j,n
    u = min(n,j-1)
    return
END FUNCTION





! **********************************************************************************
! FUNCION band(i,j,n)
! ---------> Argumentos:
!               i : elemento i de la matriz 
!               j : elemento j de la matriz
!               n : ancho de la matriz
! Descripcion:
! 	dados los indices de una matriz en banda i y j, obtienes el indice j de la 
!	matriz en banda correspondiente
! 
! ---------> Devuelve:
!               band : la j de la matriz en banda
! **********************************************************************************
FUNCTION band(i,j,n)
    integer:: band,i,j,n
    band = (j-i)+n+1
    return
END FUNCTION








! **********************************************************************************
! SUBROUTINE data(long,ancho,h,k,dx,dy,te,tf,tfinal,deltat,n,m)
! ---------> Argumentos:
!             * long   : longitud de la placa
!             * ancho  : ancho de la placa
!             * h      : coef de transmision superficial
!             * k      : conductividad termica
!             * dx     : coef de difusividad en x (largo)
!             * dy     : coef de difusividad en y (ancho)
!             * te     : temperatura del medio exterior
!             * tf     : temperatura de la fuente de calor
!             * tfinal : tiempo final de cálculo
!             * deltat : avance de tiempo en cada iteracion
!             * n      : numero de puntos en x
!             * m      : numero de puntos en y
! Descipcion:
! 	pide al usuario los datos o los lee de un archivo y los devuelve 
!	al programa principal
!
! **********************************************************************************
SUBROUTINE data(long,ancho,h,k,dx,dy,te,tf,tfinal,deltat,n,m)
    IMPLICIT none
    character(len=100):: option			!opciones de ejecuccion	
    character(len=32):: label, equal 	!etiquetas y simbolo igual del arhivo de datos de entrada
    real(8),intent(inout):: long,ancho,h,k,dx,dy,te,tf,tfinal,deltat
    integer,intent(inout):: n,m
    real(8):: datos
    integer:: sel 						!elecciones del usuario en los menus
    integer:: error=0					!errores de lectura/escritura del archivo 

    ! iniciacion de las variables
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
        write(6,'(a)',advance="no") " -> temperatura exterior [grados Kelvin] = "
        read(5,*) te
        write(6,'(a)',advance="no") " -> temperatura fuente [grados Kelvin] = "
        read(5,*) tf
        write(6,'(a)',advance="no") " -> coeficiente  de transmision superficial [W/m2 K] (h) = "
        read(5,*) h
        write(6,'(a)',advance="no") " -> conductividad termica [W/m K] (k) = "
        read(5,*) k
        write(6,'(a)',advance="no") " -> coef de difusion a lo largo [m/s2] (dx) = "
        read(5,*) dx
        write(6,'(a)',advance="no") " -> coef de difusion a lo ancho [m/s2] (dy) = "
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
        ! se lee la primera linea del archivo y se descarta (linea comentario)
        read(34,*,iostat=error) label

        !lectura de datos
        ! se lee linea por linea hasta error o fin del programa
        do while (error==0)

        	! formato del archivo 
            ! texto = valor

            ! se lee cada linea
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


        ! puede que el archivo de datos este corrupto o falten datos, se pide al usuario lo que falta
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

        close(34) ! cierre del archivo con los datos de entrada
        
    endif


    ! pregunta por guardar los datos en un archivo de texto
    write(6,'(a,/,a,/,a,/,a)') "Desea guardar los datos introducidos?","1) Si","2) No","Introduzca la opcion 1 o 2:"
    read(5,*) sel
    ! se guarda el archivo

    if(sel==1) then
        error = 1	! posibles errores de i/o

        ! se escribe en el archivo hasta que haya error o se llegue al final del archivo
        do while(error /= 0) 

        	! se pide el nombre del archivo al usuario
            write(6,*) "    -> Nombre del archivo? (No olvide la extension):"
            read(5,*) label

            ! se abre y crea el archivo
            open(unit=40, status="new", iostat=error, file=label)

            ! si se ha abierto, se escriben los datos de entrada
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
                close(40)
            else
                write(6,'(/,a,/)') "!!!!Hay problemas al guardar el archivo. Comprueba que el archivo no exista antes!!!!"
                read(5,*)
            endif
        enddo
    endif


    ! guardar los datos en el archivo de resultados
    write(55,*) "################################ " 
    write(55,*) "#            DATOS             # "
    write(55,*) "################################ " 
    write(55,*) "* n = ",n
    write(55,*) "* m = ",m
    write(55,*) "* Long = ",long
    write(55,*) "* Ancho = ",ancho
    write(55,*) "# Dx = ",dx
    write(55,*) "# Dy = ",dy
    write(55,*) "# h = ",h
    write(55,*) "# K = ",k
    write(55,*) "# Te = ",te
    write(55,*) "# Tf = ",tf
    write(55,*) "# deltaT = ",deltat
    write(55,*) "# Tfinal = ",tfinal
    write(55,*) "################################ " 
    write(55,*) 

END SUBROUTINE










! **********************************************************************************
! SUBROUTINE generacion_matriz(matriz,a,b,c,d,w,n,m)
! ---------> Argumentos:
!             * matriz    : matriz con los coeficientes del sistema de ecuaciones
!               a,b,c,d,w : elementos de la matriz
!               n         : numero de puntos en x
!               m         : numero de puntos en y
! Descipcion:
! 	coloca los elementos de la matriz en su posicion
!
! **********************************************************************************
SUBROUTINE generacion_matriz (matriz,a,b,c,d,w,n,m) 
    IMPLICIT none
    integer,intent(in):: n,m
    real(8),intent(in):: a,b,c,d,w
    real(8),dimension(n*m,2*n+1),intent(inout):: matriz
    integer:: i,j 	!variables de bucle
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







! **********************************************************************************
! SUBROUTINE generacion_vector(z,a,b,c,w,te,tf,n,m)
! ---------> Argumentos:
!             * z       : vector con los terminos independientes del sistema
!               a,b,c,w : elementos de la matriz de coeficientes
!               te      : temperatura del medio exterior
!               tf      : temperatura de la fuente de calor
!               n       : numero de puntos en x
!               m       : numero de puntos en y
! Descipcion:
! 	calcula y coloca los elementos del vecto de independientes en su posicion
!
! **********************************************************************************
SUBROUTINE generacion_vector(z,a,b,c,w,te,tf,n,m)
    IMPLICIT none
    integer,intent(in):: n,m
    real(8),intent(in):: a,b,c,w,te,tf
    real(8), dimension(n*m), intent(inout):: z
    integer:: i,j 	!variables de bucle


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







! **********************************************************************************
! SUBROUTINE CROUT(matriz,n,m)
! ---------> Argumentos:
!             * matriz : matriz con los coeficientes del sistema de ecuaciones
!               n      : numero de puntos en x
!               m      : numero de puntos en y
! Descipcion:
! 	factoriza mediante el metodo de crout la matriz del sistema
!
! **********************************************************************************
SUBROUTINE CROUT(matriz,n,m)
    IMPLICIT none
    integer,intent(in):: n,m
    real(8),dimension(n*m,n*m),intent(inout):: matriz
    integer:: i, j, k 		!variables de bucles
    integer:: u, l, band 	!funcion posicion banda inferior, posicion banda superior y transformacion matriz en banda
    real(8):: sum 			!sumatorios en el algoritmo de crout

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










! **********************************************************************************
! SUBROUTINE solve(matriz,b,n,m)
! ---------> Argumentos:
!               matriz : matriz con los coeficientes del sistema de ecuaciones
!               b      : vector de terminos independientes del sistema original
!               n      : numero de puntos en x
!               m      : numero de puntos en y
! Descipcion:
! 	resuelve el sistema factorizado por crout
!
! **********************************************************************************
SUBROUTINE solve(matriz,b,n,m) 
    IMPLICIT none
    integer,intent(in):: n,m
    real(8),dimension(n*m,2*n+1),intent(in):: matriz
    real(8),dimension(n*m),intent(inout):: b
    integer:: i, j, k 		!variables de bucles
    integer:: u, l, band 	!funcion posicion banda inferior, posicion banda superior y transformacion matriz en banda
    real(8):: sum 			!sumatorios en el algoritmo de resolucion del sistema

    ! resolucion del sistema
    ! L * z = c
    do i=2,n*m
        sum = 0
        do j=i-l(i,n),i-1
            sum = sum + matriz(i,band(i,j,n))*b(j)
        enddo
        b(i) = b(i) - sum
    enddo

    ! D * y = z
    do i=1,n*m
        b(i) = b(i) / matriz(i,band(i,i,n))
    enddo

    ! U * x = y
    do i=n*m,2,-1
        do j=i-u(i,n),i-1
            b(j) = b(j) - matriz(j,band(j,i,n)) * b(i)
        enddo
    enddo

END SUBROUTINE






! **********************************************************************************
! SUBROUTINE print_matrix(unit,matriz,n,m)
! ---------> Argumentos:
!               unit   : unidad de lectura/escritura
!               matriz : matriz con los coeficientes del sistema de ecuaciones
!               n      : numero de puntos en x
!               m      : numero de puntos en y
! Descipcion:
! 	escribe en la unidad indica la matriz con formato exponencial
!
! **********************************************************************************
SUBROUTINE print_matrix(unit,matriz,n,m)
    implicit none
    integer,intent(in):: unit,n,m
    real(8),dimension(n*m,2*n+1),intent(in):: matriz
    integer:: i,j 	!variables de los bucles
    
    do i=1,n*m
        write(unit,'(*(e10.4,2x))') (matriz(i,j),j=1,2*n+1) !loop implicit
    enddo

    write(unit,*) !salto de linea

END SUBROUTINE







! **********************************************************************************
! SUBROUTINE print_vector(unit,vector,n,m)
! ---------> Argumentos:
!               unit   : unidad de lectura/escritura
!               vector : vector de terminos independientes
!               n      : numero de puntos en x
!               m      : numero de puntos en y
! Descipcion:
! 	escribe en la unidad indica el vector de terminos independientes
!	en formato exponencial
!
! **********************************************************************************
SUBROUTINE print_vector(unit,vector,n,m)
    implicit none
    INTEGER,intent(in):: unit,n,m
    real(8), dimension(n*m), intent(in):: vector
    integer:: i,j,k 	!variables de los bucles


    ! imprime los puntos de la placa de arriba a abajo
    do j=m,1,-1
        do i=1,n
            k = i + (j-1)*n
            write(unit,'(e11.4,2x)',advance="no") vector(k) !advance = no, asi no hace salto de linea al acabar de escribir
        enddo
        write(unit,*)
    enddo

    write(unit,*) !salto de linea

END SUBROUTINE






! **********************************************************************************
! SUBROUTINE print_temperatures(unit,vector,tf,n,m)
! ---------> Argumentos:
!               unit   : unidad de lectura/escritura
!               vector : vector de terminos independientes
!               tf     : temperatura de la fuente de calor
!               n      : numero de puntos en x
!               m      : numero de puntos en y
! Descipcion:
! 	escribe en la unidad indica el las temperaturas de la placa en cada punto
!	en formato doble
!
! **********************************************************************************
SUBROUTINE print_temperatures(unit,vector,tf,n,m)
    implicit none
    INTEGER,intent(in):: unit,n,m
    real(8), dimension(n*m), intent(in):: vector
    real(8), intent(in):: tf
    integer:: i,j,k !variables de los bucles

    ! lateral superior
    write(unit,'(*(f6.2,2x))') (tf,j=1,n+1)

    ! imprime los puntos de la placa de arriba a abajo
    do j=m,1,-1
        do i=1,n
            k = i + (j-1)*n

            if(i==1) then 
            	! lateral izquierdo
                write(unit,'(f6.2,2x)',advance="no") tf 	!advance = no, asi no hace salto de linea al acabar de escribir
            endif

            ! resto de puntos de la placa
            write(unit,'(f6.2,2x)',advance="no") vector(k) 	
        enddo
        write(unit,*) 	!salto de linea
    enddo
    write(unit,*)		!salto de linea

END SUBROUTINE