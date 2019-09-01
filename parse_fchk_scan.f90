!! lzx@NJU 2019.08.30
!! Func     ::  read results from .fchk of a 2-dimension scan task
!!              write resulst into .txt file
!! Args     ::  1 > fchk_filename
!!              2 > 1st dimension, in fact, the step in .gjf file
!!              3 > 2nd dimension, in fact, the step in .gjf file

program main
    implicit none

    integer                     :: nargc

    character(len=80)           :: fchk_filename    !! arg(1)
    integer                     :: fchk_index
    character(len=80)           :: txt_filename
    integer                     :: io
    integer                     :: istat

    character(len=80)           :: chr_dim1, chr_dim2
    integer                     :: dim1, dim2       !! arg(2), arg(3)
    real(kind=8),allocatable    :: val(:,:)         !! value
    integer                     :: i, j

    character(len=80)           :: buf
    integer                     :: Nindex
    integer                     :: N
    integer                     :: k
    real(kind=8)                :: temp(5)

    nargc = iargc()
    if (nargc/=3) then
        write(*,*) "Args : 1 > fchk_filename"
        write(*,*) "       2 > 1st dimension, in fact, the step in .gjf file"
        write(*,*) "       3 > 2nd dimension, in fact, the step in .gjf file"
        stop
    end if

    call getarg(1,fchk_filename)
    call getarg(2,chr_dim1)
    call getarg(3,chr_dim2)

    read(chr_dim1,*) dim1
    read(chr_dim2,*) dim2
    dim1 = dim1 +1
    dim2 = dim2 +1
    allocate(val(dim1,dim2))

    open(NEWUNIT=io,file=trim(fchk_filename),status='old',iostat=istat,action='read')
    if (istat==0) then

        do
            read(io,"(A)") buf
            if (index(buf,'results')/=0) exit
        end do

        read(io,"(A)") buf

        do i=1,dim1
            !! Gaussian scan partten :: 1 > fix dim1, stepping dim2 until the end
            !!                          2 > fix dim2, change dim1 by one step
            !!                          3 > fix dim1, stepping dim2 but in *_opposite_* direction compared to 1
            !!                          4 > fix dim2, change dim1 by oen step
            !!                          5 > repeat 1~4, until exit
            if (mod(i,2)==1) then
                do j=1,dim2,1
                    do
                        read(io,"(A)") buf
                        if (index(buf,'Results')/=0) exit
                    end do
                    Nindex = index(buf,"N=")
                    Nindex = Nindex + 2
                    read(buf(Nindex:),*) N
                    do
                        !! at most 5 real value in one line
                        !! if you don't underrstand this, read an .fchk file
                        if (N<=6) then
                            read(io,*) (temp(k),k=1,N-1)
                            val(j,i) = temp(N-1)
                            exit
                        else
                            N = N-5
                            read(io,*)
                        end if
                    end do
                end do
            else !! mod(i,2)==0
                do j=dim2,1,-1
                    do
                        
                        read(io,"(A)") buf
                        if (index(buf,'Results')/=0) exit
                    end do
                    Nindex = index(buf,"N=")
                    Nindex = Nindex + 2
                    read(buf(Nindex:),*) N
                    do
                        !! at most 5 real value in one line
                        !! if you don't underrstand this, read an .fchk file
                        if (N<=6) then
                            read(io,*) (temp(k),k=1,N-1)
                            val(j,i) = temp(N-1)
                            exit
                        else
                            N = N-5
                            read(io,*)
                        end if
                    end do
                end do
            end if
        end do
    else
        write(*,*) "Open "//trim(fchk_filename)//" failed."
        stop
    end if
    close(io)

    fchk_index   = index(fchk_filename,".fchk")
    txt_filename = fchk_filename(1:fchk_index)//"txt"

    open(NEWUNIT=io,file=trim(txt_filename),status='replace',iostat=istat,action='write')
    if (istat==0) then
        do i=1,dim1
            write(io,"(*(F13.9,2X))") (val(j,i),j=1,dim2)
        end do
    else
        write(*,*) "Open new file "//trim(txt_filename)//" failed."
        stop
    end if

    close(io)
    deallocate(val)
    stop

end program main
!! contact  ::  flyaway333f@163.com