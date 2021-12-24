program Qb23
implicit real*8(a-h,o-z)
real*8,dimension(4)::m
m=(/1,10,100,1000/)
write(*,*)"Digite o tempo que o programa vai ficar rodando em anos"
read(*,*)ano
write(*,*)"Digite o incremento do tempo"
read(*,*)det
!repeticao para mudar a massa de jupiter

do i=1,4
    !condições iniciais
    open(1,file="entrada1.dat",status="old")
    write(*,*)"Júpiter massa",m(i),"vezes"
    pi=acos(-1.0d0)
    t=det
    gms=4*pi**2
    gmt=gms/(3*10**6)
    gmj=gms/(10**3)*m(i)
    !leitura dos dados
    read(1,*)xt1,yt1,vxt1,vyt1
    read(1,*)xj1,yj1,vxj1,vyj1
    !primeiro det da terra
    xt2=xt1+vxt1*det
    yt2=yt1+vyt1*det
    !primeiro det de jupiter
    xj2=xj1+vxj1*det
    yj2=yj1+vyj1*det
    do while(t<=ano)
        t=t+det
        !mudando as coordenadas terra
        vxt0=vxt1
        vyt0=vyt1
        xt1=xt2
        yt1=yt2
        !terra
        !calculandos o raio
        rtj=sqrt((xt1-xj1)**2+(yt1-yj1)**2)
        rts=sqrt(xt1**2+yt1**2)
        vxt1=vxt0-gms*xt1/((rts)**(3))*(det)-gmj*(xt1-xj1)*(det)/rtj**3
        vyt1=vyt0-gms*yt1/((rts)**(3))*(det)-gmj*(yt1-yj1)*(det)/rtj**3
        yt2=yt1+vyt1*det
        xt2=xt1+vxt1*det
        !mudando as coordanadas de jupiter
        vxj0=vxj1
        vyj0=vyj1
        xj1=xj2
        yj1=yj2
        !jupiter
        rtj=sqrt((xt1-xj1)**2+(yt1-yj1)**2)
        rjs=sqrt(xj1**2+yj1**2)
        vxj1=vxj0-gms*xj1/((rjs)**(3))*(det)-gmt*(xj1-xt1)*(det)/rtj**3
        vyj1=vyj0-gms*yj1/((rjs)**(3))*(det)-gmt*(yj1-yt1)*(det)/rtj**3
        yj2=yj1+vyj1*det
        xj2=xj1+vxj1*det
       
        !Ver o raio para cada ano
        !if ((1-det)<=(t-a) .and. (t-a)<=(1+det) )then
        !a=a+1
        !    write(*,*)"raio da terra",(rts-rtsa)
        !    write(*,*)"Ano",a
        !    rtsa=rts!rtsa é raio da terra anterior para comparamos a variação da terra
        !endif
        write(9+i*2,*)xt2,yt2
        write(10+i*2,*)xj2,yj2
    enddo
    close(1)
enddo
end program