! A fortran95 program for GNU
! By WJB
program main
  implicit none
!  call ex0501()
  call ex0504()
end

subroutine ex0501()
  implicit none
  real::i,o
  write(*,*) "TA的月收入为"
  read(*,*) i
  if (i<=1000) then
    o=i*0.03
  elseif ((i<=5000).and.(i>1000)) then
    o=i*0.1
  elseif (i>5000) then
    o=i*0.15
  end if
  write(*,"('TA应缴纳的税金为',F9.2)") o
end subroutine

subroutine ex0503()
  integer::age,income
  real::o
  write(*,*) "请输入纳税人的年龄和年收入"
  read(*,*) age,income
  if (age<=50) then
    if (income<=1000) then
      o=income*0.03
    elseif (income<=5000) then
      o=income*0.1
    elseif (income>5000) then
      o=income*0.15
    end if
  elseif (age>50) then
      if (income<=1000) then
      o=income*0.05
    elseif (income<=5000) then
      o=income*0.07
    elseif (income>5000) then
      o=income*0.1
    end if
  end if

  write(*,"('应缴纳税金为',F9.2)") o
end subroutine

subroutine ex0504()
  integer::y,t
  real,parameter::e=0.0001
  write(*,*)"请输入年份"
  read(*,*) y
  if ((mod(y,4)<=e).and.(mod(y,100)/=0))then
    t=1
  else
    t=0
  end if
  if (mod(y,400)<e) t=1

  select case(t)
    case(1)
      write(*,"(I4,'年是闰年，一年366天')") y
    case(0)
      write(*,"(I4,'年不是闰年，一年365天')") y
  end select

end subroutine



