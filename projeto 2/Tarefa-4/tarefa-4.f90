program Qa
    implicit real*8(a-h,o-z)
    open(1,file="saida1.dat",status="replace")
    open(2,file="saida2.dat",status="replace")
    !entrada de dados
    write(*,*)"digite x_inicial"
    read(*,*)xi
    write(*,*)"digite y_inicial"
    read(*,*)yi
    write(*,*)"digite velocidadex"
    read(*,*)vxi
    write(*,*)"digite velocidadey"
    read(*,*)vyi
    write(*,*)'Digite o valor de alpha'
    read(*,*)alp
    !salvado condições iniciais
    alpha=0
    det=0.0001
    pi=acos(-1.0d0)
    gms=4*pi**2
    do i=1,10
        !resetando todos as constantes e realizando condições iniciais
        write(*,*)i
        r_max=0
        t=det
        !mudança de coordenadas
        x1=xi
        y1=yi
        vx1=vxi
        vy1=vyi
        x2=x1+vx1*det
        y2=y1+vy1*det
        N=0
        k=0
        alpha=i*alp
        do while(t<2.5)
            !alterando as coordenadas
            t=t+det
            vx0=vx1
            vy0=vy1
            x0=x1
            y0=y1
            x1=x2
            y1=y2
            !calculando o próximo ponto
            r=sqrt(x1**2+y1**2)
            vx1=vx0-((gms*x1/r**3)*(1+alpha/r**2))*(det)
            vy1=vy0-((gms*y1/r**3)*(1+alpha/r**2))*(det)
            y2=y1+vy1*det
            x2=x1+vx1*det
            write(i+10,*)x1,y1
            if (r>r_max)then
                r_max=r
                ang=datan(y1/x1)
            endif
            if(x1<=0.0 .and. x2>=0.0)then!quando planeta passar novamente x=0 dará um periodo
                if (ang<0 .and. flag==0)then!caso atan(y1/x1) for negativo fazer um reajuste
                    k=k+1
                    flag=1
                else if(ang>0)then
                    flag=0
                endif
                write(20+i,*)t,ang
                r_max=0
            endif
        enddo
    enddo
    
end program