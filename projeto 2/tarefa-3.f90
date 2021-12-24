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
write(*,*)"digite o valor do espaçamento de tempo"
read(*,*)det
!condicoes iniciais
!det=0.0005
pi=acos(-1.0d0)
t=det
v_min=10000
v_max=0
r_min=10000
r_max=0
gms=4*pi**2
!primeiro det
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
    !para calcular a area da elipse na parte a2
    v=sqrt(vy1**2+vx1**2)
    !achando os parametros do problem
    if(v<v_min)then
        v_min=v
    endif
    if(v>v_max)then
        v_max=v
    endif
    if(r<r_min)then
        r_min=r
    endif
    if(r>r_max)then
        r_max=r
    endif
    if(abs(x2-r)<=0.00001 .and. t>=0.1 .and. x2-x1<=0)then!fizemos a consideracao que sempre usaremos o eixo x como inicial
        exit
    endif
    write(1,*)x2,y2
enddo
write(2,*)0.0d0,0.0d0!sol
write(*,*)'Velocidade mínima e máxima'
write(*,*)v_min,v_max
write(*,*)'raio mínimo e máximo'
write(*,*)r_min,r_max
write(*,*)"Terceira Lei de Kleper "
write(*,*)t**2/((r_min+r_max)/2.0d0)**3
Write(*,*)'Periodo'
write(*,*)t
end program