program carlo
implicit real*8(a-h,o-z)
write(*,*)'velocidade de x, y respectivamente'
read(*,*)vx, vy
write(*,*)'posição de x,y respectivamente'
read(*,*)x,y
write(*,*)'O tamanho do quadrado?'
read(*,*)L
e0=0.005d0
!primeiro e0
t=e0
write(1,*)x,y
write(2,*)vx,x
do i=0,100000
    y=y+vy*e0
    x=x+vx*e0
    t=t+e0
    !vendo se está dentro da caixa
    if (L/2-abs(y)<0.05) then
       vy=-vy
    endif
    if(L/2-abs(x)<0.05) then
       vx=-vx
    endif
    write(1,*)x,y
    if (abs(y)<=abs(vy*e0)) then!caso seja menor que o valor 
      write(2,*)x,vx
      !write(*,*)x,y
    endif
enddo
end program
