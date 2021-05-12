
    program projekt
    implicit none
    integer size
    integer i
    integer j
    real,dimension(20,20):: tab
    
    
        call introduction()
        call getSize(size)
        call inputData(size,tab)
        
        
        
    pause
    end program

    subroutine introduction()
    implicit none
        write(*,*)"Zespol nr.1"
        write(*,*)"Obliczanie wyznacznika macierzy kwadratowej."
        write(*,*)"Przeksztalcenie jej do rownowaznej macierzy trojkatnej."
        write(*,*)"Zespol: Dominik Szpilski (szef), Daniel Oblak, Radoslaw Michalak."    
    end subroutine
    
    subroutine getSize(size)
    implicit none
    integer size
10      write(*,'(a)',advance="no")"Podaj wymiar macierzy (max 20):"
        read(*,*)size
        if((size.gt.20).or.(size.lt.2)) then
            write(*,*)"Wymiar macierzy nie moze byc wiekszy niz 20 i mniejszy od 2."
            goto 10
        endif
        call SYSTEM('cls')
    return
    end subroutine
    
    subroutine inputData(size,tab)
    integer size
    real,dimension(20,20):: tab
        
        write(*,*)
        do i=1, size
            do j=1, size
                call showMatrix(size,tab)
                write(*,*)
                write(*,'(a,i2,a,i2,a)',advance="no")"Wprowadz wartosc macierzy w miejscu A(",i,",",j,"):"
                read(*,*)tab(i,j)
                call SYSTEM('cls')
            enddo
            current=0
        enddo
        call showMatrix(size,tab)
        write(*,*)
        
    
    return
    end subroutine
    
    subroutine showMatrix(size,tab)
    real,dimension(20,20)::tab
    integer size
            write(*,'(a)',advance="no")' #'
        do k=1,size
            write(*,'(a)',advance="no")'========'
        enddo
        write(*,'(a)',advance="no")'#'
        write(*,*)
        do i=1,size
            do j=1, size
                write(*,'(a)',advance="no")" | "
                write(*,'(F6.2)',advance="no")tab(i,j)                
            enddo
            write(*,'(a)',advance="no")" | "
            write(*,*)
            write(*,'(a)',advance="no")" | "
            do j=1,size
                
                if((i.eq.1).and.(j.eq.1)) then
                write(*,'(a)',advance="no")"^^^^^^"
                else
                    write(*,'(a)',advance="no")"------"
                endif
                write(*,'(a)',advance="no")" | "    
            enddo
            write(*,*)
        enddo
        write(*,'(a)',advance="no")' #'
        do k=1,size
            write(*,'(a)',advance="no")'========'
        enddo
        write(*,'(a)',advance="no")'#'
    return
    end subroutine