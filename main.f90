program projekt
	implicit none
    integer size,rowSwaps
    real,dimension(20,20):: matrix
 	integer choice
	logical matrixLoaded
    
	matrixLoaded = .false.
    choice = 0
    rowSwaps=0
    do while(choice.lt.1.or.choice.gt.4)
        write(*,'(a)')"Zespol nr.1"
        write(*,'(a)')"Obliczanie wyznacznika macierzy kwadratowej."
        write(*,'(a)')"Przeksztalcenie jej do rownowaznej macierzy trojkatnej."
        write(*,'(a)')"Zespol: Dominik Szpilski (lider), Daniel Oblak, Radoslaw Michalak."
        write(*,'(a)')"==== Menu Glowne ===="
        write(*,'(a)')"1. Wczytaj dane z pliku"      
        write(*,'(a)')"2. Generuj losowa macierz"
        write(*,'(a)')"3. Obliczanie wyznacznika"
        write(*,'(a)')"4. Wyjscie z programu"
        write(*,'(a)', advance="no")"Podaj wybrana opcje: "
        read(*,*)choice
        if(choice.lt.1.and.choice.gt.4) then
            write(*,"(a)")"Podano nieprawidlowa wartosc. Sproboj ponownie."
            call callForAction(.false.)
            continue
        end if
        call SYSTEM('cls')
        select case (choice)
            case (1)
                write(*,'(a)') "Wczytaj macierz z pliku"
                call loadMatrix(matrix,size)
                matrixLoaded = .true.
            case (2)
                write(*,'(a)')"Wygeneruj macierz"
                call generateMatrix(matrix,size)
                matrixLoaded = .true.
            case (3)
                write(*,'(a)')"Obliczanie wyznacznika macierzy"
                if(.not.matrixLoaded) then
                  write(*,"(a)")"Przed rozpoczeciem obliczen nalezy wczytac macierz"
                  call callForAction(.true.)
                else
                  call calculateMatrix(matrix,size,rowSwaps)  
				end if   
            case (4)
                EXIT
            case default
                write(*,'(a)')'Podano nieprawidowa wartosc, sprobuj jeszcze raz'
            end select
            choice = 0
            call SYSTEM("cls")
    end do
end program

subroutine calculateMatrix(matrix,size,rowSwaps)
    implicit none
    real,dimension(20,20)::matrix
    real det
    character*15 file
    integer :: size,i,rowSwaps
    logical checkRowsInMatrix
    
	write(*,"(a)", advance="no")"Podaj nazwe pliku wynikowego: "
    read(*,*)file
    write(*,*)
    open(10,FILE=file, STATUS='unknown')
    if(checkRowsInMatrix(matrix,size))then
        write(*,'(a)')"Dwa wiersze sa takie same, wyznacznik rowny 0."
        write(10,'(a)')"Dwa wiersze sa takie same, wyznacznik rowny 0."
        call callForAction(.true.)
        return
    end if
    call checkDiagonalsForZeros(matrix,size,rowSwaps)
    call getUpperTriangularMatrix(matrix,size,rowSwaps)
    det=1.0
    do i=1,size
    	det=det*matrix(i,i)
    end do
    if(mod(rowSwaps,2).eq.1) then
		det = -det
    end if
   	write(*,"(a,f30.6)")"Wyznacznik macierzy wynosi: ",det
    write(10,"(a,f30.6)")"Wyznacznik macierzy wynosi: ",det
    close(10)
    call callForAction(.true.)
end subroutine

subroutine getUpperTriangularMatrix(matrix,size,rowSwaps)
    implicit none
    real, dimension(20,20)::matrix
    integer, intent(in) :: size
    integer rowSwaps
    real diagonal,c;
    integer i,j,k
    write(*,"(a)")"Wyznaczanie macierzy trojkatnej gornej"
    write(10,"(a)")"Wyznaczanie macierzy trojkatnej gornej"
    do i=1,size-1
     diagonal=matrix(i,i)
     do j=i+1,size
      c = matrix(j,i)/diagonal
      call checkDiagonalsForZeros(matrix,size,rowSwaps)
      do k=i,size+1
      	matrix(j,k)=matrix(j,k)-c*matrix(i,k)   
      enddo
     enddo
   enddo
   write(*,"(/a/)")"Wynikowa macierz trojkatna:",
   write(10,"(/a/)")"Wynikowa macierz trojkatna:" 
   do i=1,size
  		write(*,'(16f10.2)')(matrix(i,j),j=1,size)
	 	write(10,'(16f10.2)')(matrix(i,j),j=1,size)
   enddo
   write(*,*)
   write(10,*) 
end subroutine


logical function checkRowsInMatrix(matrix,size)
   	implicit none
   	real, dimension(20,20), intent(in) :: matrix
   	integer, intent(in) :: size
    integer i,j,k,match
   
    match=0
    do i=1,size-1
   		do k=i+1,size
   			do j=1,size
   				if(matrix(i,j).eq.matrix(k,j))then
   					match=match+1
   				else
   					match=0
   				end if
   			end do!j
   		end do!k
   		if(match.ge.size)then
   			checkRowsInMatrix=.true.
   		else
   			checkRowsInMatrix=.false.
   		end if!dwa sa takie same
   	end do!i  
end function
    
subroutine checkDiagonalsForZeros(matrix,size,rowSwaps)
	implicit none
    real, dimension(20,20) :: matrix
    integer, intent(in) :: size
    integer :: rowSwaps
    integer i
    logical foundError

    foundError = .true.
    do while (foundError)
        foundError = .false.
        do i=1,size
            if(matrix(i,i).eq.0.0)then
               write(*,'(a,i2,a,i2)')"Zamieniono wiersz ",i," z wierszem ",i+1
               write(10,'(a,i2,a,i2)')"Zamieniono wiersz ",i," z wierszem ",i+1            
               call replaceRow(matrix,size,i)
               rowSwaps=rowSwaps+1
               foundError = .true.
            end if
        end do
    end do
end subroutine
    
subroutine replaceRow(matrix,size,i)
    implicit none
    real,dimension(20,20)::matrix
    real,dimension(20)::temp
    integer, intent(in)::size,i
    integer k
    do k=1,size
      if(i+1.gt.size)then
        temp(k)=matrix(i,k)
        matrix(i-1,k)=matrix(i,k)
        matrix(i-1,k)=temp(k)
      else
        temp(k)=matrix(i+1,k)
        matrix(i+1,k)=matrix(i,k)
        matrix(i,k)=temp(k)
      end if
    end do
end subroutine    

subroutine loadMatrix(matrix,size)
    implicit none
	character*15 file1
    integer size,i,j; 
    character*1 choice
    real, dimension(20,20) :: matrix
    
    write(*,"(a)", advance="no")"Podaj nazwe pliku wejciowego macierzy: "
    read(*,*)file1
    open(10,FILE=file1, STATUS='old')
    write(*,"(a)", advance="no")"Podaj rozmiar macierzy: "
    read(*,*)size
    do i=1,size
		read(10,*)(matrix(i,j),j=1,size)
    end do
   
    close(10)
    do while(choice.ne."t".and.choice.ne."n")  
    	write(*,"(a)", advance="no")"Macierz wczytana, czy wyswietlic? (t/n): "
    	read(*,*)choice
    	if(choice.ne."t".and.choice.ne."n") then
    		write(*,"(a)")"Podano nieprawidlowa wartosc. Sproboj ponownie"
        	continue
    	end if
    end do
    if(choice.eq."t") then
      call displayMatrix(matrix, size)
   	end if
    choice = ""
end subroutine

subroutine displayMatrix(matrix,size)
	implicit none
    real, dimension(20,20) :: matrix
    integer :: size,i,j
    
    do i=1,size
       	write(*,'(20f6.1)')(matrix(i,j),j=1,size)
    end do
    write(*,*)
    call callForAction(.true.) 
end subroutine

subroutine generateMatrix(matrix,size)
    implicit none
    real,dimension(20,20)::matrix
    integer :: size,temp,i,j
    
    size=0
    do while(size.eq.0)
    	write(*,'(a)',advance="no")"Podaj wymiar macierzy kwadratowej (max 20): "
        read(*,*)temp
        if((temp.gt.20).or.(temp.lt.2)) then
        	write(*,*)"Wymiar macierzy nie moze byc wiekszy niz 20 i mniejszy od 2."
        	continue
		endif
        size = temp
    end do
    call random_number(matrix)
	do i=1,size
    	do j=1,size
        	matrix(i,j)=200*matrix(i,j)-100
        	matrix(i,j)=AINT(matrix(i,j))/10.0
        end do !j
    end do !i
    call displayMatrix(matrix,size)
end subroutine
    
subroutine callForAction(menu)
	implicit none
    logical, intent(in) :: menu
    
    if(.not.menu) then
		write(*,"(a)")"Nacisnij dowolny przycisk zeby kontynuowac"
    else
      write(*,"(a)")"Nacisnij dowolny przycisk zeby wrocic do menu glownego"
    end if
	read(*,*)
    
end subroutine
    
