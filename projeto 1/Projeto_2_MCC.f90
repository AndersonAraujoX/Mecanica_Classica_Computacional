program carlo
implicit real*8(a-h,o-z)
write(*,*)'velocidade de x, y respectivamente'
read(*,*)vx, vy
write(*,*)'posição de x,y respectivamente'
read(*,*)x,y
write(*,*)'O tamanho do circulo?'
read(*,*)L
e0=0.001d0
!primeiro e0
t=e0
write(1,*)x,y
write(2,*)vx,x
do i=0,100000
    y=y+vy*e0
    x=x+vx*e0
    t=t+e0
    if (sqrt(y*y+x*x)>L) then
       !definindo a nova dire��o
       r=sqrt(y*y+x*x)
       !vpe=(v.n)n
       v_pex=-(vx*(-x/r)+vy*(-y/r))*(-x/r)
       v_pey=-(vy*(-y/r)+vx*(-x/r))*(-y/r)
       !velocidade final
       vx=vx+v_pex+v_pex
       vy=vy+v_pey+v_pey
    endif
    write(1,*)x,y
    if (abs(y)<=abs(vy*e0)) then!caso seja menor que o valor 
        write(2,*)x,vx
        !write(*,*)x,y
      endif
enddo
end program
