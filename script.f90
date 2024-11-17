module functions
implicit none
contains

function d1r(r,vr,theta,vtheta,t) result(d1ra)
    real r,vr,theta,vtheta,t,d1ra
    d1ra = vr
end function

function d1theta(r,vr,theta,vtheta,t) result(d1theta_)
    real r,vr,theta,vtheta,t,d1ra,d1theta_
    d1theta_ = vtheta
end function

function d2r(r,vr,theta,vtheta,t) result(d2ra)
    real r,vr,theta,vtheta,t,d2ra,M,G
    !G=6.6743015e-11
    G=0.1
    M=1e5
    d2ra = r*vtheta**2-G*M/r**2
end function

function d2theta(r,vr,theta,vtheta,t) result(d2theta_)
    real r,vr,theta,vtheta,t,d2ra,M,d2theta_
    d2theta_ = -2*vtheta*vr/r
end function

end module functions

program gravedad
use functions
implicit none
real theta,vtheta,r,vr,h,k1r,k1rv,k2r,k2rv,k3r,k3rv,k4r,k4rv,t
real k1te,k1tev,k2te,k2tev,k3te,k3tev,k4te,k4tev,rf,vrf,thetaf,vthetaf
integer i,j,k,pasos
character(len=20) t1,t2,t3,t4
pasos = 10000

r=100
vr=1
theta=0
vtheta=10
t=0
h=0.0001

open(10,file="data.txt")
do i=1,pasos
    !k1
    k1r =      d1r(r,vr,theta,vtheta,t)
    k1te = d1theta(r,vr,theta,vtheta,t)
    k1rv =     d2r(r,vr,theta,vtheta,t)
    k1tev =d2theta(r,vr,theta,vtheta,t)

    !k2
    k2r =      d1r(r+h*k1r/2,vr+h*k1rv/2,theta+h*k1te/2,vtheta+h*k1tev/2,t+h/2)
    k2te = d1theta(r+h*k1r/2,vr+h*k1rv/2,theta+h*k1te/2,vtheta+h*k1tev/2,t+h/2)
    k2rv =     d2r(r+h*k1r/2,vr+h*k1rv/2,theta+h*k1te/2,vtheta+h*k1tev/2,t+h/2)
    k2tev =d2theta(r+h*k1r/2,vr+h*k1rv/2,theta+h*k1te/2,vtheta+h*k1tev/2,t+h/2)

    !k3
    k3r =      d1r(r+h*k2r/2,vr+h*k2rv/2,theta+h*k2te/2,vtheta+h*k2tev/2,t+h/2)
    k3te = d1theta(r+h*k2r/2,vr+h*k2rv/2,theta+h*k2te/2,vtheta+h*k2tev/2,t+h/2)
    k3rv =     d2r(r+h*k2r/2,vr+h*k2rv/2,theta+h*k2te/2,vtheta+h*k2tev/2,t+h/2)
    k3tev =d2theta(r+h*k2r/2,vr+h*k2rv/2,theta+h*k2te/2,vtheta+h*k2tev/2,t+h/2)

    !k4
    k4r =      d1r(r+h*k3r,vr+h*k3rv,theta+h*k3te,vtheta+h*k3tev,t+h)
    k4te = d1theta(r+h*k3r,vr+h*k3rv,theta+h*k3te,vtheta+h*k3tev,t+h)
    k4rv =     d2r(r+h*k3r,vr+h*k3rv,theta+h*k3te,vtheta+h*k3tev,t+h)
    k4tev =d2theta(r+h*k3r,vr+h*k3rv,theta+h*k3te,vtheta+h*k3tev,t+h)

    rf =         r + h*(k1r+2*k2r+2*k3r+k4r)/6
    vrf =       vr + h*(k1rv+2*k2rv+2*k3rv+k4rv)/6
    thetaf = theta + h*(k1te+2*k2te+2*k3te+k4te)/6
    vthetaf=vtheta + h*(k1tev+2*k2tev+2*k3tev+k4tev)/6

    r = rf
    vr = vrf
    theta = thetaf
    vtheta = vthetaf

    print *, r,theta,vr,vtheta

    write(t1,'(F20.4)') r
    write(t2,'(F20.4)') theta
    write(t3,'(F20.4)') vr
    write(t4,'(F20.4)') vtheta

    write(10,*) trim(t1)//" "//trim(t2)//" "//trim(t3)//" "//trim(t3)
end do

!CALL execute_command_line('gnuplot > load graficar.gp' ) 

end program gravedad
