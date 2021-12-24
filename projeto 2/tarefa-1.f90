program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1.dat",status="replace")
open(2,file="saida2.dat",status="replace")
write(*,*)"digite x_inicial"
read(*,*)x1
write(*,*)"digite y_inicial"
read(*,*)y1
write(*,*)"digite velocidadex"
read(*,*)vx1
write(*,*)"digite velocidadey"
read(*,*)vy1
write(*,*)"digite o valor espaçamento de tempo"
read(*,*)det
!condicoes iniciais
!det=0.00001
pi=acos(-1.0d0)
t=det
a=0
b=0
area=0
gms=4*pi**2!(x1**2+y1**2)**(1/2)**3/ano**2
!primeira iteração
x2=x1+vx1*det
y2=y1+vy1*det
do while(1<8)
    !mudando as coordenadas
    t=t+det
    vx0=vx1
    vy0=vy1
    x1=x2
    y1=y2
    r=sqrt(x1**2+y1**2)
    vx1=vx0-gms*x1/((r)**(3))*(det)
    vy1=vy0-gms*y1/((r)**(3))*(det)
    y2=y1+vy1*det
    x2=x1+vx1*det
    if(abs(x2-r)<=0.00001 .and. t>=0.1 .and. x2-x1<=0)then!quando planeta passar novamente x=0 dará um periodo
        exit
    endif
    write(1,*)x2,y2
enddo
end program