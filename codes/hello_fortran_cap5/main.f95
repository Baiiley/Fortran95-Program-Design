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
  write(*,*) "TA��������Ϊ"
  read(*,*) i
  if (i<=1000) then
    o=i*0.03
  elseif ((i<=5000).and.(i>1000)) then
    o=i*0.1
  elseif (i>5000) then
    o=i*0.15
  end if
  write(*,"('TAӦ���ɵ�˰��Ϊ',F9.2)") o
end subroutine

subroutine ex0503()
  integer::age,income
  real::o
  write(*,*) "��������˰�˵������������"
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

  write(*,"('Ӧ����˰��Ϊ',F9.2)") o
end subroutine

subroutine ex0504()
  integer::y,t
  real,parameter::e=0.0001
  write(*,*)"���������"
  read(*,*) y
  if ((mod(y,4)<=e).and.(mod(y,100)/=0))then
    t=1
  else
    t=0
  end if
  if (mod(y,400)<e) t=1

  select case(t)
    case(1)
      write(*,"(I4,'�������꣬һ��366��')") y
    case(0)
      write(*,"(I4,'�겻�����꣬һ��365��')") y
  end select

end subroutine



