SUBROUTINE estadio(x,y,vx,vy,L,vetx,vety)
real*4, intent(inout) :: x,y,vx,vy,L
real*4, dimension(400010),intent(inout) :: vetx,vety
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
    vetx(i)=x
    vety(i)=y
    if (abs(y)<=abs(vy*e0)) then!caso seja menor que o valor 
      write(4,*)x,vx
      !write(*,*)x,y
    endif
enddo
END SUBROUTINE estadio
program carlo
implicit real*4(a-h,o-z)
real*4::L
real*4, dimension(400010) :: vetx,vet_ax,vety,vet_ay
write(*,*)'velocidade de x, y respectivamente'
read(*,*)vx, vy
write(*,*)'posição de x,y respectivamente'
read(*,*)x,y
write(*,*)'O tamanho do semicirculo?'
read(*,*)L
call estadio(x,y,vx,vy,L,vetx,vety)
vet_ax=vetx
vet_ay=vety
x=x+0.00001
call estadio(x,y,vx,vy,L,vetx,vety)
soma=0
e0=0.001
do i=0,400000
   soma=soma+sqrt((vet_ax(i)-vetx(i))**2+(vet_ay(i)-vety(i))**2)
   write(1,*)vetx(i),vety(i)
   write(2,*)vet_ax(i),vet_ay(i)
   write(3,*)i*e0,soma
enddo

end program
