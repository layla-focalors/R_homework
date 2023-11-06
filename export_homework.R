#1
x<-seq(-3,3,0.1)    
y<-x^3                  
plot(x,y,type='l')      

#2
x<-seq(O,8,by=O,1)      
y1<-x^2 ; y2<-x^3        
y3<-log(x) ; y4<-exp(x)
ytotal<-cbind(y1,y2,y3,y4)
par(mfrow=c(2,2))
plot(x,y1) ; plot(x,y2) ;plot(x,y3); plot(x,y4)
plot(x,y1,type='o') ; plot(x,y2,type='o')
plot(x,y3,type='o') ; plot(x,y4,type='o')

#3
x<-seq(-2*pi,2*pi,0.1)
par(mfrow=c(1,1))
ysin<-sin(x) ; ycos<-cos(x) ; ytan<-tan(x)
ytotal<-cbind(ysin,ycos,ytan)
matplot(x,ytotal,type='l',ylim=c(-2,2))	

#4
x<-seq(-2,2,by=0.1)
y<-log(abs(x))
plot(x,y,type='l')

#5
x<-seq(0,5,by=0.1)
y<-1/4*x*exp(-x^2)
plot(x,y,type='l')

#6
y<-dbinom(c(0:5),5,0.5)#각 질량함수 값
x<-c(0, 1, 2, 3, 4, 5)
plot(x,y,type='h', lwd=10)

#7
a.blood<-c(90,99,102,97,102,95,87,90,89,109) 
b.blood<-c(88,78,99,88,91,99,108,110,77,99)  
h1<-1:10                                     
h2<-1:10                                    
h<-rbind(h1,h2)                           
blood<-rbind(a.blood,b.blood)               
plot(blood~h, col=c('blue', 'pink'))  

#8
data(mtcars)
install.packages('rgl')
library(rgl)
plot3d(mtcars$mpg,mtcars$hp,mtcars$drat)
text3d(mtcars$mpg,mtcars$hp,mtcars$drat, texts=colnames(mtcars), col='red',adj=1)

#9
x1<-seq(-4,4,by=0.5)	
x2<-seq(-4,4,by=0.5)	
func<-function(x1,x2) {	
  answer<-x1^2+x1*x2+x2^2	
  return(answer)
}
y<-outer(x1,x2,FUN=func)
persp(x1,x2,y) 

#10
x1<-seq(-6,6,by=0.5)                            
x2<-seq(-6,6,by=0.5)                            
mu1<-1 ; s1<-1 ; r=0.5                   
mu2<-2 ; s2<-4;                               
func<-function(x1,x2) {                      
  pro1<-1/(2*pi*sqrt(s1*s2)*(1-r^2))          
  pro2<-((x1-mu1)/sqrt(s1))^2             
  pro3<-(2*r*(x1-mu1)*(x2-mu2))/(sqrt(s1*s2)) 
  pro4<-((x2-mu2)/sqrt(s2))^2               
  pro5<-(pro2-pro3+pro4)                     
  pro6<-pro1*exp(-pro5/(2*(1-r^2)))            
  return(pro6)                                  
}
y<-outer(x1,x2,FUN=func)                        
persp(x1,x2,y)                            
    
