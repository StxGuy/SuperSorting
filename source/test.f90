program test
    use SuperSorting
    
    integer :: a(34),a1(17),a2(17)
    integer :: i
    
    a = [1,3,11,6,2,15,54,12,23,66,30,7,8,65,17,12,14,4,6,67,38,8,27,28,18,43,15,2,9,8,23,45,15,23]
    
    ! Remove comment for testing
    !call insertionsort(a)
    !write(*,*) a
    
    ! Remove comment for testing
    !a = mergesort([1,3,6],[2,7,8])
    !write(*,*) a
    
    call timsort(a)
    
    do i = 1,size(a,1)
        write(*,*) i,a(i)
    end do
end program
