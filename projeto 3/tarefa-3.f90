program Qa
implicit real*8(a-h,o-z)
open(1,file="saida1.dat",status="replace")
open(2,file="saida2.dat",status="replace")
open(3,file="saida3.dat",status="replace")
open(4,file="saida4.dat",status="replace")
open(11,file="entrada3.dat",status="old")



read(11,*)xj1,yj1,vxj1,vyj1
read(11,*)xaI1,yaI1,vxaI1,vyaI1
read(11,*)xaII1,yaII1,vxaII1,vyaII1
read(11,*)xaIII1,yaIII1,vxaIII1,vyaIII1 
close(11)
write(*,*)"quanto tempo o sistema vai rodar"
read(*,*)ano
write(*,*)"Digite o incremento do tempo"
read(*,*)det
!condições iniciais
pi=acos(-1.0d0)
t=det
gms=4*pi**2
gmj=gms/(1000)
!jupiter
yj2=yj1+vyj1*det
xj2=xj1+vxj1*det

!asteoride I

yaI2=yaI1+vyaI1*det
xaI2=xaI1+vxaI1*det 

!asteoride II
yaII2=yaII1+vyaII1*det 
xaII2=xaII1+vxaII1*det 

!asteoride III
yaIII2=yaIII1+vyaIII1*det 
xaIII2=xaIII1+vxaIII1*det 

!metodo de verlet
do while (1<8)
    t=t+det
    !mudando as coordenadas de jupiter
    vxj0=vxj1
    vyj0=vyj1
    xj1=xj2
    yj1=yj2

    rjs=sqrt(xj1**2 + yj1**2)!raio jupiter
    vyj1=vyj0-gms*yj1*det/(rjs**3)
    vxj1=vxj0-gms*xj1*det/(rjs**3)
    yj2=yj1+vyj1*det
    xj2=xj1+vxj1*det

    
    !mudando as coordenadas do asteroide I
    vxaI0=vxaI1
    vyaI0=vyaI1
    xaI1=xaI2
    yaI1=yaI2

    raIs = sqrt(xaI1**2+yaI1**2)!raio aI
    raIj = sqrt((xaI1-xj1)**2+(yaI1-yj1)**2)
    vyaI1=vyaI0-gms*yaI1/((raIs)**(3))*(det)-gmj*(yaI1-yj1)*(det)/(raIj**3)
    vxaI1=vxaI0-gms*xaI1/((raIs)**(3))*(det)-gmj*(xaI1-xj1)*(det)/(raIj**3)
    yaI2=yaI1+vyaI1*det
    xaI2=xaI1+vxaI1*det 


    !mudando as coordenadas do asteroide II
    vxaII0=vxaII1
    vyaII0=vyaII1
    xaII1=xaII2
    yaII1=yaII2


    raIIs=sqrt(xaII1**2+yaII1**2)!raio aII
    raIIj=sqrt((xaII1-xj1)**2+(yaII1-yj1)**2)
    vyaII1=vyaII0- gms*yaII1/((raIIs)**(3))*(det)-gmj*(yaII1-yj1)*(det)/(raIIj**3)
    vxaII1=vxaII0- gms*xaII1/((raIIs)**(3))*(det) -gmj*(xaII1-xj1)*(det)/(raIIj**3)
    yaII2=yaII1+vyaII1*det 
    xaII2=xaII1+vxaII1*det 

    !mudando as coordenadas do asteroide III
    
    vxaIII0=vxaIII1
    vyaIII0=vyaIII1
    xaIII1=xaIII2
    yaIII1=yaIII2


    raIIIs=sqrt(xaIII1**2+yaIII1**2)!raio aIII
    raIIIj=sqrt((xaIII1-xj1)**2+(yaIII1-yj1)**2)

    vyaIII1=vyaIII0-gms*yaIII1/((raIIIs)**(3))*(det)-gmj*(yaIII1-yj1)*(det)/(raIIIj**3)
    vxaIII1=vxaIII0-gms*xaIII1/((raIIIs)**(3))*(det)-gmj*(xaIII1-xj1)*(det)/(raIIIj**3)
    yaIII2=yaIII1+vyaIII1*det 
    xaIII2=xaIII1+vxaIII1*det 

    
    if(t>=ano)then
        exit
    endif
    write(1,*)xaI2,yaI2
    write(2,*)xaII2,yaII2
    write(3,*)xaIII2,yaIII2
    write(4,*)xj2, yj2
enddo
end program