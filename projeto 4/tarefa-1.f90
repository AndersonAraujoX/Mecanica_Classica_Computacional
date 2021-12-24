program tarefaA
    implicit real*8(a-h,o-z)
    real*8, dimension(:,:),allocatable::v,s
    integer*4::d,t,flag
    write(*,*)'digite separadamente o tamanho do array[1:Nx,1:Ny]'
    read(*,*)Nx,Ny
    write(*,*)'digite o valor da velocidade inicial'
    read(*,*)v0
    write(*,*)'digite o valor de omega'
    read(*,*)omega
    write(*,*)'digite o valor do numero de Reynolds'
    read(*,*)R
    allocate(v(0:Nx,0:Ny),s(0:Nx,0:Ny)) 
    h=1
    d=Nx/4
    t=Ny/2
    !vamos considerar h=1
    open(1,file='corre')
    open(2,file='vorti')
    open(3,file='velocidade')
    !configurando as condições iniciais.
    do i=0,Nx!condições iniciais para as funções
      do j=0,Ny
          v(i,j)=0.0d0
          s(i,j)=j*h*v0
      enddo
    enddo
    do i=0,Nx!superficie G
      s(i,Ny)=s(i,Ny-1)+v0*h
      v(i,Ny-1)=0.0d0
    enddo
    do j=0,Ny!entrada F
      s(1,j)=s(0,j)
      v(0,j)=0.0d0
    enddo  
    do i=0,Nx!Parte de baixo A e E
      if (i<=d .or. 2*d<=i)then
        s(i,0)=0.0d0
        v(i,0)=0.0d0
      endif
    enddo
    do j=1,Ny-1!Saida H
      v(Nx,j)=v(Nx-1,j)
      s(Nx,j)=s(Nx-1,j)
    enddo
    soma_pc=0.0d0
    soma_pv=1000.0d0
    soma_fc=0.0d0
    soma_fv=0.0d0
    do i=1,Nx-1!Parametro de parada do programa
      do j=1,Ny-1
        soma_fc=soma_fc+s(i,j)
        soma_fv=soma_fv+v(i,j)
      enddo
    enddo
    do i=d,2*d
      do j=1,t 
      v(d,j)=-2.0d0*s(d-1,j)!D
      v(2*d,j)=-2.0d0*s(2*d+1,j)!B
      v(i,t-1)=-2.0d0*s(i,t)!C
      enddo
    enddo
    do i=d,2*d
      do j=1,t
        s(d,j)=0.0d0!D
        s(2*d,j)=0.0d0!B
        s(i,t)=0.0d0!C
      enddo
    enddo 
    flag=0
    do while(abs(soma_fc-soma_pc)>0.00001*(Nx*Ny-d*t) .or. abs(soma_fv-soma_pv)>0.00001*(Nx*Ny-d*t))!nossa parametro de incertaza será a região onde os valores podem ser alterados
      soma_pc=soma_fc
      soma_pv=soma_fv
      soma_fv=0
      soma_fc=0
      flag=flag+1
      !barra
      do i=d,2*d
        do j=1,t 
        v(d,j)=-2.0d0*s(d-1,j)!D
        v(2*d,j)=-2.0d0*s(2*d+1,j)!B
        v(i,t-1)=-2.0d0*s(i,t)!C
        enddo
      enddo
      do i=d,2*d
        do j=1,t
          s(d,j)=0.0d0!D
          s(2*d,j)=0.0d0!B
          s(i,t)=0.0d0!C
        enddo
      enddo     
        !relaxamento do vortice e da corrente
        do i=1,Nx-1
          do j=1,Ny-1!valor da corrente
            s(i,j)=s(i,j)+omega*((s(i+1,j)+s(i-1,j)+s(i,j+1)+s(i,j-1)+h*h*v(i,j))*0.25-s(i,j))
          enddo
        enddo
        do i=1,Nx-1
          do j=1,Ny-1!valor do vortice
            a=(s(i,j+1)-s(i,j-1))*(v(i+1,j)-v(i-1,j))!(ds/dy*dv/dx)
            b=(s(i+1,j)-s(i-1,j))*(v(i,j+1)-v(i,j-1))!(ds/dx*dv/dy)
            v(i,j)=v(i,j)+omega*((v(i+1, j)+v(i-1,j)+v(i,j+1)+v(i,j-1)-(a-b)*R*0.25)*0.25-v(i,j))
          enddo
        enddo
        do i=1,Nx-1
          do j=1,Ny-1!soma de todos os valores
            soma_fc=soma_fc+s(i,j)
            soma_fv=soma_fv+v(i,j)
          enddo
        enddo
    enddo
    do i=1,Nx-1!saida de dados
      do j=1,Ny-1
        write(1,*)i,j,s(i,j)
        write(2,*)i,j,v(i,j)
        write(3,*)i,j,(s(i,j+1)-s(i,j-1))/(2.0*h),(s(i-1,j)-s(i+1,j))/(2.0*h)!vx,vy
      enddo
    enddo
    write(*,*)"quantidade de iterações",flag
  end program
  