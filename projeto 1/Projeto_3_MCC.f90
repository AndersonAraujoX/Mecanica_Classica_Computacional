program carlo
implicit real*4(a-h,o-z)
real*4::L
write(*,*)'velocidade de x, y respectivamente'
read(*,*)vx, vy
write(*,*)'posição de x,y respectivamente'
read(*,*)x,y
write(*,*)'O tamanho do semicirculo?'
read(*,*)L
soma=0
e0=0.001
!primeiro e0
t=e0
do i=0,400000
    y=y+vy*e0
    x=x+vx*e0
    t=t+e0
    if ((y*y+(abs(x)-0.2*L)**2)>(L*L) .and. abs(x)>0.2*L) then
       !definindo a nova direçao
       r=sqrt(y*y+(abs(x)-0.2*L)**2)
       !vpe=(v.n)n
       v_pex=-(vx*(-(x-0.2*sign(L,x))/r)+vy*(-y/r))*(-(x-0.2*sign(L,x))/r)!fun��o sing(L,y), para caso -y ent�o y+L e para caso +y ent�o (y-L)
       v_pey=-(vy*(-y/r)+vx*(-(x-0.2*sign(L,x))/r))*(-y/r)
       !velocidade final
       vx=vx+v_pex+v_pex
       vy=vy+v_pey+v_pey
    endif
    if(L-abs(y)<0.05) then
       vy=-vy
    endif
    write(1,*)x,y
    if (abs(y)<=abs(vy*e0)) then!caso seja menor que o valor 
      write(4,*)x,vx 
      !write(*,*)x,y
    endif
enddo
end program
