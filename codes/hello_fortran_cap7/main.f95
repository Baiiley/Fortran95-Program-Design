! A fortran95 program for GNU
! By WJB
program main
  implicit none
  call ex0704()

end

subroutine ex0701()
  implicit none
  integer :: a(10)
  integer::i
  forall (i=1:10)
    !forall循环是对数组的，是同时运算的，很难用！！！里面如果有数的计算，比如s=s+a(i)会出错！
    a(i)=i*2
  end forall
  write(*,*) a(:),sum(a)/size(a)
end subroutine


subroutine ex0703()
  implicit none
  integer::f(0:10)
  integer::i
  data(f(i),i=0,1) /0,1/
  do i=2,10
    f(i)=f(i-1)+f(i-2)
  end do
  write(*,*) f(0:10)
end subroutine

subroutine ex0704()
  implicit none
  integer,parameter::s=10
  integer::a(s)=(/5,4,6,8,7,1,3,2,20,8/)
  integer::i,j,t
  do i=1,s-1
    do j=i+1,s
      if (a(i)<a(j)) then
      t=a(i)
      a(i)=a(j)
      a(j)=t
      end if
    end do
  end do
  write(*,*) a(:)
end subroutine
