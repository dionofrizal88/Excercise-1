#Dio Agus Nofrizal (17523110)
#Faza Nur Azizi (17523112)


#                Excercise 1 (EX1)
library(Ryacas)


#------------Set
Q=c(-100:100)
Q
p=17523110
if((p %% 2) == 0) {
  print(paste(p,"Genap"))
} else {
  print(paste(p,"Ganjil"))
}
#no3
S=c(1,5,0,2,8)
R<-setdiff(Q,S)
R
#no4
u<-union(p,R)
u
#no5
h<-setdiff(Q,u)
h
#no6
a<-setdiff(Q,p)
b<-setdiff(Q,R)
d<-intersect(a,b)
d



#Function
#no1
f<-function(x,y){
  z<-sqrt(x)+y^2
  return(z)
}
f(4,2)
#no2
g<-function(a,b){
  g2<-a*b*(a^2+(b/3))
  return(g2)
}
g(4,2)
#no3
h<-function(x,y){
  h1<-sqrt(f(x,y)+3+g(x,y))
  return(h1)
}
h(4,2)
#no4
f1f2<-function(x){
  f1<-function(x){
    return(-x^3+x+1)
  }
  f2<-function(x){
    return(sqrt(x)-1)
  }
  return(f1(f2(x)))
}
f1f2(4)
#no5
f1<-function(x){
  return(1/x)
}
f2<-function(x){
  return(2/x)
}
f3<-function(x){
  return(3/x)
}
f4<-function(x){
  return(4/x)
}
f5<-function(x){
  return(5/x)
}
x<-c(-16:16)
plot(x, f1(x), ylab="y", type = "l", col="red")
lines(x, f2(x), col="orange")
lines(x, f3(x), col="yellow")
lines(x, f4(x), col="green")
lines(x, f5(x), col="blue")



#-------------Limit
library(Ryacas)
#No1
f<- function(x){
  fx<- ((1-cos(x))/x)
  return(fx)
}
x<-Sym("x")
Limit(f(x),x,0)

#No2
f<- function(h){
  fh<- ((2*(-3*h+h^2)^2)/(h))
  return(fh)
}
h<-Sym("h")
Limit(f(h),h,0)

#No3
f<- function(t){
  ft<- (t-sqrt(3*t+4))/(4-t)
  return(ft)
}
t<-Sym("t")
Limit(f(t),t,4)



#------------diferentiation
#No1
x <- Sym("x")
Simplify(deriv(sqrt(x)*(x+1),x))
#No2
x <- Sym("x")
Simplify(deriv((2*x^2-3)/(sqrt(x)),x))
#No3
x <- Sym("x")
Simplify(deriv((x-1)/(x+1),x))
#No4
intergrand <- function(x){
  return((2*x + (x+1))/(2*root(x,2)))
}
intergrand <- function(x){
  return((8*x^2+(-2*x^2+3))/(2*(root(x,2)*x)))
}
intergrand <- function(x){
  return(2/(x^2+2*x+1))
}



#--------------integration
library(Ryacas)
#No1
integrand <- function(x){
  return (2*x^3)
}
integrate(f=integrand,lower =0, upper=3)

#No2
integrand <- function(x){
  return (1-5*x^4)
}
integrate(f=integrand,lower =-1, upper=2)

#No3
integrand <- function(x){
  return (x^4-3*x^2+5)
}
integrate(f=integrand,lower =-2, upper=2)

#No4
integrand <- function(x){
  return (x^2+(1/2*sqrt(x)))
}
integrate(f=integrand,lower =1, upper=4)

#No5
integrand <- function(x){
  return ((2-3*x)^2)
}
integrate(f=integrand,lower =0, upper=2)

