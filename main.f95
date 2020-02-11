! A fortran95 program for G95
! By WQY
program main
  implicit none
  call ex0401()
!  call ex0402()
!  call ex0403()
!  call ex0404()
  call ex0405()
end

subroutine ex0401()
  implicit none
  character(len=23)::a,b,c
  a="Have a good time."
  b="That's not bad."
  c="""Mary"" isn't my name."
!  write(*,"(1XA23)") a
!  write(*,*)b
!  write(*,*)c
  write(*,"(3(1XA23,/))") a,b,c
end subroutine

subroutine ex0402()
  real ::r,s,pi
  parameter (pi=3.1415)
  write(*,*) "请输入圆的半径"
  read(*,*) r
  s=pi*r**2
  write(*,"('半径为',F4.2,' 的圆的面积是',F9.2)") r,s
end subroutine

subroutine ex0403()
  real :: r,l
  read(*,*)  r
  write(6,"(1XF6.2)") sqrt(r)*10
end subroutine

subroutine ex0404()
  real  a,b
  a=2.0
  b=3.0
  write(*,*) b/a
end subroutine

subroutine ex0405()
    implicit none
    type::distance
    integer::m,cm,mm
    end type

    type(distance)::a
    write(*,*) "请输入长度，单位m"
    read(*,*) a%m
    a%cm=100*a%m
    a%mm=100000*a%m
    write(*,"('长度为',/I10,'米，',/I10,'厘米,',/I10,'毫米')") a%m,a%cm,a%mm

end subroutine


