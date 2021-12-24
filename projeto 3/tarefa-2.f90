program Qb23
    implicit real*8(a-h,o-z)
    real*8,dimension(4)::m
    
    m=(/1,10,100,1000/)
    write(*,*)"digite o tempo que o programa vai ficar rodando em anos"
    read(*,*)ano
    write(*,*)"Digite o incremento do tempo"
    read(*,*)det
    !leitura de entrada
    !read(1,*)
    
    !repeticao para mudar a massa de jupiter
    
    do i=1,4
        !condições iniciais
        open(1,file="entrada2.dat",status="old")
        write(*,*)"Jupiter massa",m(i),"vezes"
        read(1,*)xm1,ym1,vxm1,vym1
        !write(*,*)xt1,yt1,vxt1,vyt1
        read(1,*)xj1,yj1,vxj1,vyj1
        !write(*,*)xj1,yj1,vxj1,vyj1
        pi=acos(-1.0d0)
        t=det
        gms=4*pi**2
        gmm=gms/(3.3*10**7)
        gmj=gms/(10**3)*m(i)
        !primeiro det jupiter

        yj2=yj1+vyj1*det
        xj2=xj1+vxj1*det
        !primeiro det terra

        ym2=ym1+vym1*det
        xm2=xm1+vxm1*det
        !metodo de verlet
        do while(t<=ano)
            t=t+det
            !mudando as coordenadas terra
            vxm0=vxm1
            vym0=vym1

            xm1=xm2
            ym1=ym2
            !terra
            !calculandos o raio
            rmj=sqrt((xm1-xj1)**2+(ym1-yj1)**2)
            rms=sqrt(xm1**2+ym1**2)

            vxm1=vxm0-gms*xm1/((rms)**(3))*(det)-gmj*(xm1-xj1)*(det)/rmj**3
            vym1=vym0-gms*ym1/((rms)**(3))*(det)-gmj*(ym1-yj1)*(det)/rmj**3

            ym2=ym1+vym1*det
            xm2=xm1+vxm1*det
            !mudando as coordanadas de jupiter
            vxj0=vxj1
            vyj0=vyj1

            xj1=xj2
            yj1=yj2
            !jupiter
            rmj=sqrt((xm1-xj1)**2+(ym1-yj1)**2)
            rjs=sqrt(xj1**2+yj1**2)

            vxj1=vxj0-gms*xj1/((rjs)**(3))*(det)-gmm*(xj1-xm1)*(det)/rmj**3
            vyj1=vyj0-gms*yj1/((rjs)**(3))*(det)-gmm*(yj1-ym1)*(det)/rmj**3

            yj2=yj1+vyj1*det
            xj2=xj1+vxj1*det
           
            !Ver o raio para cada ano
            write(9+i*2,*)xm2,ym2
            write(10+i*2,*)xj2,yj2
        enddo
        close(1)
    enddo
    end program