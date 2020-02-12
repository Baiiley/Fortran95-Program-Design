! A fortran95 program for GNU
! By WJB
program main
  implicit none
!  call ex0602()
  call ex0605()
end

subroutine ex0602()
  implicit none
  integer :: i,r=0
  do i=1,99,2
    r=r+i
  end do
  write(*,"('等差数列1+3+5+7+...+99结果为',I7)"),r
end subroutine

subroutine ex0604()
  implicit none
  integer::i,j,r1=0,r2=1
  do i=1,10,1
    do j=1,i,1
      r2=r2/j
    end do
    r1=r1+r2
    r2=1
  end do
  write(*,"('1/1！+1/2！+1/3！+...1/10!循环结果为',I7)"),r1
end subroutine

subroutine ex0605()
  implicit none
  integer::i,j=0,strlen
  character(len=50)::input,output
  write(*,*)"请输入句子"
  read(*,*) input
  strlen=len_trim(input) !不计尾部空格
  do i=1,strlen,1
    if (input(i:i).ne.'') then
      j=j+1
      output(j:j)=input(i:i)
    end if

  end do
  write(*,*) output
end subroutine
