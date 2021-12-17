module SuperSorting

    contains
    
    ! Insertion Sorting
    subroutine insertionsort(A)
        implicit none
        
        integer,intent(inout)   :: A(:)
                
        integer :: i,j,t
        integer :: L
        
       
        i = 2
        do while(i < size(A,1))
            j = i
            do while(j > 1 .and. A(j-1) > A(j))
                t = A(j)
                A(j) = A(j-1)
                A(j-1) = t
                j = j - 1
            end do
            i = i + 1
        end do
    end subroutine
    
    
    ! Merge Sorting
    recursive function mergesort(left,right) result(merged)
        implicit none
        
        integer,intent(in)  :: left(:),right(:)
        integer             :: merged(size(left,1)+size(right,1))
        
        integer             :: sR, sL
        

        sR = size(right,1)
        sL = size(left,1)
                        
        if (sL .eq. 0) then
            merged = right
        elseif (sR .eq. 0) then
            merged = left
        elseif (left(1) < right(1)) then
            merged = [left(1),mergesort(left(2:),right)]
        else
            merged = [right(1),mergesort(left,right(2:))]
        end if
    end function
    
    ! Merge for Tim Sorting
    subroutine mergetim(A,l,m,r)
        implicit none
        
        integer,intent(inout)   :: A(:)
        integer,intent(in)      :: l,m,r
        
        integer :: i, j, k, len1, len2
        integer,allocatable :: left(:), right(:)
        
        
        len1 = m-l+1
        len2 = r-m
        
        allocate(left(len1),right(len2))
        
        left = A(l:m)
        right = A((m+1):r)
        
        i = 0
        j = 0
        k = l
        do while(i < len1 .and. j < len2)
            if (left(i) .le. right(j)) then
                A(k) = left(i)
                i = i + 1
            else
                A(k) = right(j)
                j = j + 1
            end if
            k = k + 1
        end do
        
        do while(i < len1)
            A(k) = left(i)
            k = k + 1
            i = i + 1
        end do
        do while(j < len2)
            A(k) = right(j)
            k = k + 1
            j = j + 1
        end do
        
        deallocate(left,right)
    end subroutine
        
    
    ! Tim Sorting
    subroutine timsort(A)
        implicit none
        
        integer,intent(inout)  :: A(:)
        
        integer :: length, n, r, st, en, mid
        integer :: minrun, left, right, sz
        
        ! Find minrun
        n = size(A,1)
        r = 0
        do while(n .ge. 32)
            r = ior(r,iand(n,1))
            n = ishft(n,-1)
        end do
        minrun = n + r
        n = size(A,1)
        
        ! Main run
        do st = 1,n,minrun
            en = minval([st + minrun - 1, n])
            call insertionsort(A(st:en))
        end do
        
        sz = minrun
        do while(sz < n)
            do left = 1,n,2*sz
                mid = minval([n,left+sz-1])
                right = minval([left+2*sz-1,n])
                
                call mergetim(A,left,mid,right)
            end do
            sz = sz*2
        end do
    end subroutine
            
    
end module
