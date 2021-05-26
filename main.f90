program projekt
    implicit none
    integer size,i,j
    real,dimension(20,20):: matrix
    
    integer choice;
    choice = 0;
    
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
            write(*,"(a)")"Podano nieprawidlowa wartosc. Sproboj ponownie"
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
                call calculateMatrix(matrix,size)
            case (4)
                EXIT
            case default
                write(*,'(a)')'Podano nieprawidowa wartosc, sprobuj jeszcze raz'
            end select
            choice = 0
            call SYSTEM("cls")
    end do
end program

    
!subroutine getSize(size)
!    implicit none
!    integer size,temp;
!    
!    size=0;
!    do while(size.eq.0)
!        write(*,'(a)',advance="no")"Podaj wymiar macierzy kwadratowej (max 20): "
!        read(*,*)temp
!        if((temp.gt.20).or.(temp.lt.2)) then
!            write(*,*)"Wymiar macierzy nie moze byc wiekszy niz 20 i mniejszy od 2."
!            continue
!        endif
!        size = temp
!    end do
!    return
!    
!end subroutine
    
    
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
                end if
            end do!j
        end do!k
        if(match.ge.size)then
            checkRowsInMatrix=.true.
        end if!dwa s¹ takie same
    match=0
    end do!i  
    
    end function
    
    
    subroutine calculateMatrix(matrix,size)
    implicit none
    real,dimension(20,20)::matrix,x,y,z
    integer :: size,temp,i,j
    logical checkRowsInMatrix
    
    if(checkRowsInMatrix(matrix,size))then
        write(*,'(a)')"Dwa wiersze sa takie same, wyznacznik rowny 0."
        call callForAction(.true.)
        return
    end if
    
    
    call callForAction(.true.)
    
    end subroutine
    
    subroutine multiplyByThemselves(a,b,c,size)
    implicit none
    real, dimension(20,20) :: a,b,c
    integer size,i,j,k
    
    write(*,*)"Tutaj tez cos kiedys bedzie"
    
    call callForAction(.true.)
    
    end subroutine