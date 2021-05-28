program projekt
    integer size,i,j,rowSwaps
    real,dimension(20,20):: matrix
    
    integer choice
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
                write(*,'(a)') "Wczytywanie z pliku"
                call loadMatrix(matrix,size)
            case (2)
                write(*,'(a)')"Generacja macierzy"
                call generateMatrix(matrix,size)
            case (3)
                write(*,'(a)')"Obliczanie wyznacznika macierzy"
!                call calculateMatrix(matrix,size,rowSwaps)
                 call decompose(matrix,size,P,rowSwaps)
                 det=matrix(1,1)
                 do i=1,size-1
                   det=det*matrix(i,i)
                 end do
                 write(*,*)det
                 call callForAction(.true.)
                   
            case (4)
                EXIT
            case default
                write(*,'(a)')'Podano nieprawidowa wartosc, sprobuj jeszcze raz'
            end select
            choice = 0
            call SYSTEM("cls")
    end do
end program
    
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
      call displaySimpleMatrix(matrix, size)
   	end if
    choice = ""
end subroutine

subroutine displaySimpleMatrix(matrix,size)
	implicit none
    real, dimension(20,20) :: matrix
    integer :: size,i,j
    
    do i=1,size
       	write(*,'(20f6.1)')(matrix(i,j),j=1,size)
    end do
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
    call displaySimpleMatrix(matrix,size)
    
    end subroutine
    
    logical function checkRowsInMatrix(matrix,size)
    implicit none
    real, dimension(20,20), intent(in) :: matrix
    real, dimension(20) :: temp
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
    
    
    subroutine calculateMatrix(matrix,size,rowSwaps)
    implicit none
    real,dimension(20,20)::matrix,x,y,z
    integer :: size,temp,i,j,rowSwaps
    logical checkRowsInMatrix
    
    if(checkRowsInMatrix(matrix,size))then
        write(*,'(a)')"Dwa wiersze sa takie same, wyznacznik rowny 0."
        call callForAction(.true.)
        return
    end if
    call checkDiagonalsForZeros(matrix,size,rowSwaps)
    call eliminationBase(matrix,size,rowSwaps)
    
    call callForAction(.true.)
    
    end subroutine
    
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
               write(10,'(a,i2,a,i2)')"Zamieniono wiersz ",i," z wiersze ",i+1            
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
    
    subroutine decompose(matrix,size,P,rowSwaps)
    real, dimension(20,20)::matrix
    real,dimension(20)::P,ptr
    integer, intent(in) :: size
    integer z
    real diagonal,det,maxA,absA
    integer i,j,k,rowSwaps,imax
    det=0
    
    do i=1,size
      P(i)=i
    end do

    do i=1,size-1
        maxA=0.0
        imax=i
            
        do k=i,size-1
          absA=abs(matrix(k,i))
          if(absA.gt.maxA)then
            maxA=absA
            imax=k
          end if
        end do!K
          if(imax.ne.i)then
            j=P(i)
            P(i)=P(imax)
            P(imax)=j
            do z=1,size
            ptr(z)=matrix(i,z)
            matrix(i,z)=matrix(imax,z)
            matrix(imax,z)=ptr(z)
            end do

            P(size)=P(size)+1
          end if
          
          do j=i+1,size-1
            matrix(j,i) = matrix(j,i) / matrix(i,i)
            do k=i+1,size-1
              matrix(j,k)=matrix(j,k)-(matrix(j,i)*matrix(i,k))
            end do
          end do
    end do!i
    call displaySimpleMatrix(matrix,size)
end subroutine




    
    
    subroutine multiplyByThemselves(a,b,c,size)
    implicit none
    real, dimension(20,20) :: a,b,c
    integer size,i,j,k
    
    write(*,*)"Tutaj tez cos kiedys bedzie"
    
    call callForAction(.true.)
    
    end subroutine