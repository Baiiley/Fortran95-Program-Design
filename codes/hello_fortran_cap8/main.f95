! A fortran95 program for GNU
! By WJB

!program ex0801
!  implicit none
!  real::r,s
!  read(*,*) r
!  call area(r,s)
!  write(*,*) s
!end
!
!subroutine area(r,s)
!  implicit none
!  real::r,s
!  s=3.1415*r**2
!end subroutine

!-------------------------------------------------
!program ex0802
!  implicit none
!  real::r
!  real,external::area
!  read(*,*) r
!  write(*,*) area(r)
!end program
!
!function area(r)
!  implicit none
!  real::r,area
!  area=3.1415*r**2
!end function
!
!-------------------------------------------------
!program ex0803
!  implicit none
!  integer::n
!  read(*,*) n
!  call draw(n)
!end
!
!
!subroutine draw(n)
!  implicit none
!  integer::n,i
!  character(len=1)::a(10)
!  do i=1,n
!    a(i)='*'
!  end do
!  write(*,*) a(1:n)
!end subroutine
!
!-------------------------------------------------
!program ex0804
!  implicit none
!  integer::n=100
!  integer,external::sumup
!  write(*,*) sumup(n)
!end program
!
!
!recursive integer function sumup(n) result(ans)
!  implicit none
!  integer,intent(in)::n
!  if(n<0) then
!    ans=0
!    return
!  elseif (n<=1)then
!    ans=1
!    return
!  end if
!  ans=n+sumup(n-1)
!end function
!
!-------------------------------------------------
!program ex0805
!  implicit none
!  integer::a,b
!  integer,external::yinzi
!  read(*,*) a,b
!  write(*,*) yinzi(a,b)
!
!end program
!
!integer function yinzi(a,b)
!  implicit none
!  integer::a,b,minn,maxx,i
!  minn=min(a,b)
!  maxx=max(a,b)
!  do i=minn,1,-1
!    if ( (mod(minn,i)==0) .and. (mod(maxx,i)==0) ) then
!    yinzi=i
!    write(*,"('最大公因子是',I5)") i
!这行不能输出，fortran的function很差，只能当函数用，输入书，输出数
!    exit
!    end if
!  end do
!end function

!-------------------------------------------------
!利用subroutine写第五题
!program ex0805_2
!  implicit none
!  integer::a,b,f
!  read(*,*) a,b
!  call factor(a,b,f)
!  write(*,"(I3,'和',I3,'的最大公因子是',I3)") a,b,f
!
!end program
!
!subroutine factor(a,b,f)
!  implicit none
!  integer::a,b,f,maxx,minn,i
!  maxx=max(a,b)
!  minn=min(a,b)
!  write(*,*) "开始递减查找公因子"
!  do i=minn,1,-1
!    write(*,*) i
!    if ( (mod(maxx,i)==0) .and. (mod(minn,i)==0) ) then
!      f=i
!      exit
!    end if
!  end do
!end subroutine


!-------------------------------------------------
!利用subroutine和循环取余法写第五题
!循环取余法：a,b的最大公因数是c,则a=c*c1,b=c*c1
! 则mod(a,b)一定是c*c3,则b和mod(a,b)的最大公倍数和a,b的最大公倍数相同，
! 循环直到mod(a,b)=0 时 b是最小公倍数
! 通过这种方法不需要分辨a和b的大小，因为a<b时，mod(a,b)=a
! 最小公倍数即为a*b/最大公因数

program ex0805_2
  implicit none
  integer::a,b,f,m
  read(*,*) a,b
  call factor(a,b,f)
  m=a*b/f
  write(*,"(I5,'和',I5,'的最大公因子是',I5,'，最小公倍数是',I5)") a,b,f,m

end program

subroutine factor(a,b,f)
  implicit none
  integer::a,b,num1,num2,f,i
  num1=a
  num2=b
  do while(.true.)
    if (mod(num1,num2)/=0) then
      i=mod(num1,num2)
      num1=num2
      num2=i
    elseif (mod(num1,num2)==0) then
      f=num2
      exit
    end if

  end do
end subroutine



