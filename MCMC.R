# etude de l'estimateur en fonction de m
mm=1000;
xx=rcauchy(mm,0,1);
p1=cumsum(xx>2)/(1:mm)
x11()
plot(p1,type="l",col="blue")
p1[mm]
mp1=mean(p1)
mp1# mais pas de sens
varp1=var(p1)
varp1#mais pas de sens

# calcul de l'esperance et de la variance de l'estimateur
mm=100;
kk=100
pk=rep(0,kk)
for(i in 1:kk)
{
  xx=rcauchy(mm,0,1)
  pk[i]=mean(xx>2)
}
x11()
plot(pk,type="l",col="blue")
mpk=mean(pk)
mpk
varpk=var(pk)
varpk

#b) p_2
mm=1000;
xx=rcauchy(mm,0,1);
p2=0.5*cumsum(abs(xx)>2)/(1:mm)
x11()
plot(p2,type="l",col="blue")
p2[mm]

# calcul de l'esperance et de la variance de l'estimateur
mm=100;
kk=100
pk=rep(0,kk)
for(i in 1:kk)
{
  xx=rcauchy(mm,0,1)
  pk[i]=0.5*mean(abs(xx)>2)
}
x11()
plot(pk,type="l",col="blue")
mpk=mean(pk)
varpk=var(pk)

#c) p_3
mm=1000
xx=runif(mm,0,2)
p3=0.5-(cumsum(2 / (pi*(1+xx*xx)) )/(1:mm))
x11()
plot(p3,type="l",col="blue")
p3[mm]

# calcul de l'esperance et de la variance de l'estimateur
mm=100;
kk=100
pk=rep(0,kk)
for(i in 1:kk)
{
  xx=runif(mm,0,2)
  pk[i]=0.5-mean(2 / (pi*(1+xx*xx)) )
}
x11()
plot(pk,type="l",col="blue")
mpk=mean(pk)
varpk=var(pk)



# Exercice 4 : Intégration
#
###########################################################
h=function(x){(cos(50*x)+sin(20*x))^2}
x11()
par(mfrow=c(2,1))
curve(h,xlab="Function",ylab="")
vint=integrate(h,0,1)#calcul de l'integrale
nn=10^4#nb de aleatoire
x=h(runif(nn))#uniforme entre 0 et 1
estint=cumsum(x)/(1:nn)
esterr=sqrt(cumsum((x-estint)^2))/(1:nn)#j'estime la moyenne de empirique
plot(estint, xlab="Mean and error range",ylab="",col="blue",type="l")
abline(h=vint$value,col="red")
lines(estint+1.96*esterr,col="green")
lines(estint-1.96*esterr,col="green")

#-a : calcul analyitique - approximation directe par R
h1=function(theta,x=0){
  t1=theta/(1+theta*theta)
  t2=0.5*((x-theta)^2)
  t3=t1*exp(-t2)}

h2=function(theta,x=0){
  t1=1/(1+theta*theta)
  t2=0.5*((x-theta)^2)
  t3=t1*exp(-t2)}

x11()
dom1=seq(-5,5,by=0.01)
h1.v=h1(dom1)
h2.v=h2(dom1)
par(mfrow=c(2,1))
plot(dom1,h1.v,xlab="Function",ylab="",type="l",col="blue")
plot(dom1,h2.v,xlab="Function",ylab="",type="l",col="blue")

integrate(h1,-5,5)
integrate(h2,-5,5)

#-b : approximation Monte Carlo utilisant la loi uniforme
nn=10^4
dom2=10*runif(nn)-5 #runif(nn,-5,5)
x11()
par(mfrow=c(3,2))
x.u=10*h1(dom2)
est.h1.u=cumsum(x.u)/(1:nn)
esterr.u1=sqrt(cumsum((x.u-est.h1.u)^2))/(1:nn)
plot(est.h1.u, xlab="Integrand 1 : mean and error range - uniform",ylab="",col="blue",type="l")
abline(h=vint1$value,col="red")
lines(est.h1.u+2*esterr.u1,col="green")
lines(est.h1.u-2*esterr.u1,col="green")

x.u=10*h2(dom2)
est.h2.u=cumsum(x.u)/(1:nn)
esterr.u2=sqrt(cumsum((x.u-est.h2.u)^2))/(1:nn)
plot(est.h2.u, xlab="Integrand 2 : mean and error range - uniform",ylab="",col="blue",type="l")
abline(h=vint2$value,col="red")
lines(est.h2.u+2*esterr.u2,col="green")
lines(est.h2.u-2*esterr.u2,col="green")

# Exercice 5 : Intégration
#
###########################################################
#-a : calcul analyitique - approximation directe par R
h1=function(theta,x=0){
  t1=theta/(1+theta*theta)
  t2=0.5*((x-theta)^2)
  t3=t1*exp(-t2)}

h2=function(theta,x=0){
  t1=1/(1+theta*theta)
  t2=0.5*((x-theta)^2)
  t3=t1*exp(-t2)}

x11()
dom1=seq(-5,5,by=0.01)
h1.v=h1(dom1)
h2.v=h2(dom1)
par(mfrow=c(2,1))
plot(dom1,h1.v,xlab="Function",ylab="",type="l",col="blue")
plot(dom1,h2.v,xlab="Function",ylab="",type="l",col="blue")

vint1=integrate(h1,-5,5)
vint2=integrate(h2,-5,5)

#-b : approximation Monte Carlo utilisant la loi uniforme
nn=10^4
dom2=10*runif(nn)-5 #runif(nn,-5,5)
x11()
par(mfrow=c(3,2))
x.u=10*h1(dom2)
est.h1.u=cumsum(x.u)/(1:nn)
esterr.u1=sqrt(cumsum((x.u-est.h1.u)^2))/(1:nn)
plot(est.h1.u, xlab="Integrand 1 : mean and error range - uniform",ylab="",col="blue",type="l")
abline(h=vint1$value,col="red")
lines(est.h1.u+2*esterr.u1,col="green")
lines(est.h1.u-2*esterr.u1,col="green")

x.u=10*h2(dom2)
est.h2.u=cumsum(x.u)/(1:nn)
esterr.u2=sqrt(cumsum((x.u-est.h2.u)^2))/(1:nn)
plot(est.h2.u, xlab="Integrand 2 : mean and error range - uniform",ylab="",col="blue",type="l")
abline(h=vint2$value,col="red")
lines(est.h2.u+2*esterr.u2,col="green")
lines(est.h2.u-2*esterr.u2,col="green")

#-c : approximation Monte Carlo utilisant la loi de Cauchy
h1.c=function(theta,x=0){
  t1=pi*theta
  t2=0.5*((x-theta)^2)
  t3=t1*exp(-t2)}

h2.c=function(theta,x=0){
  t1=pi
  t2=0.5*((x-theta)^2)
  t3=t1*exp(-t2)}

dom3=rcauchy(nn)

#x11()
#par(mfrow=c(2,1))
x.c=h1.c(dom3)
est.h1.c=cumsum(x.c)/(1:nn)
esterr.c1=sqrt(cumsum((x.c-est.h1.c)^2))/(1:nn)
plot(est.h1.c, xlab="Integrand 1: mean and error range - Cauchy",ylab="",col="blue",type="l")
abline(h=vint1$value,col="red")
lines(est.h1.u+2*esterr.u1,col="green")
lines(est.h1.u-2*esterr.u1,col="green")

x.c=h2.c(dom3)
est.h2.c=cumsum(x.c)/(1:nn)
esterr.c2=sqrt(cumsum((x.c-est.h2.c)^2))/(1:nn)
plot(est.h2.c, xlab="Integrand 2: mean and error range - Cauchy",ylab="",col="blue",type="l")
abline(h=vint2$value,col="red")
lines(est.h2.c+2*esterr.c2,col="green")
lines(est.h2.c-2*esterr.c2,col="green")

#-d : approximation Monte Carlo utilisant la loi normale
h1.n=function(theta){
  t1=sqrt(2*pi)*theta/(1+theta*theta)
}

h2.n=function(theta){
  t1=sqrt(2*pi)/(1+theta*theta)
}

dom4=rnorm(nn)

x.n=h1.n(dom4)
est.h1.n=cumsum(x.n)/(1:nn)
esterr.n1=sqrt(cumsum((x.n-est.h1.n)^2))/(1:nn)
plot(est.h1.n, xlab="Integrand 1 : mean and error range - normal",ylab="",col="blue",type="l")
abline(h=vint1$value,col="red")
lines(est.h1.n+2*esterr.n1,col="green")
lines(est.h1.n-2*esterr.n1,col="green")

x.n=h2.n(dom4)
est.h2.n=cumsum(x.n)/(1:nn)
esterr.n2=sqrt(cumsum((x.n-est.h2.n)^2))/(1:nn)
plot(est.h2.n, xlab="Integrand 2 : mean and error range - normal",ylab="",col="blue",type="l")
abline(h=vint2$value,col="red")
lines(est.h2.n+2*esterr.n2,col="green")
lines(est.h2.n-2*esterr.n2,col="green")



x11()
par(mfrow=c(3,1))
plot(est.h1.u/est.h2.u,xlab="Integral estimate - uniform, Cauchy, normal",ylab="",type="l",col="blue")
lines(est.h1.c/est.h2.c,col="magenta")
lines(est.h1.n/est.h2.n,col="gold")

plot(esterr.u1,xlab="Error estimate 1 - uniform, Cauchy, normal;",ylab="",type="l",col="blue")
lines(esterr.c1,col="magenta")
lines(esterr.n1,ylab="",col="gold")

plot(esterr.u2,xlab="Error estimate 2 - uniform, Cauchy, normal",ylab="",type="l",col="blue")
lines(esterr.c2,col="magenta")
lines(esterr.n2,col="gold")

# #EX6 c
# nn=8
# x=rnorm(10^nn)
# #whole sample
# #bound=qnorm(c(.5,.75,.8,.9,.95,.99,.999,.9999))
# bound=c(0,0.5,0.67,1,1.65,2,3,4.5)
# res=matrix(0,ncol=length(bound),nrow=nn-1)
# for (i in 2:nn)
#   #lengthy loop!!
#   for (j in 1:length(bound))
#     res[i-1,j]=mean(x[1:10^i]<bound[j])
# matrix(as.numeric(format(res,digi=4)),ncol=length(bound))

# Exercice 7 : Echantillonnage pondere - approximation des queues des distributions
#
###################################################################################
nn=10^3
y=rexp(nn)+4.5
wgt=dnorm(y)/dexp(y-4.5)
est.p=cumsum(wgt)/(1:nn)
esterr.p=sqrt(cumsum((est.p-pnorm(-4.5))^2))/(1:nn)
x11()
plot(est.p,type="l",col="blue",xlab="",ylab="")
abline(a=pnorm(-4.5),b=0,col="red",lty=2)
lines(est.p+2*esterr.p,col="green")
lines(est.p-2*esterr.p,col="green"

      nsim=10^3
      x=runif(nsim)
      summary(x)

      #a et b
      x11()
      par(mfrow=c(1,2))
      hist(x,col="blue",freq=FALSE)
      x.sort=sort(x)
      lines(x.sort,dunif(x.sort),col="red")
      cdf.x=ecdf(x)
      plot(cdf.x,col="blue",main="Fonction de reparition empirique et theorique")
      x.theo=seq(from=min(x),to=max(x),by=0.001)
      cdf.theo=punif(x.theo)
      lines(x.theo,cdf.theo,col="red",lty=2)

      #c
      ks.test(x,punif,0,1)

      #d
      x1=x[-nsim]
      x2=x[-1]
      x11()
      par(mfrow=c(1,2))
      plot(x1,x2,cex=0.5,col="blue")
      acf(x)

      cor(x1,x2)


      # Exercice 12 : Simulation d'une variable de loi exponentielle de moyenne 0.5
      #
      ###################################################################################
      # a
      nsim=10^3
      lambda=2
      x=rexp(nsim,rate=lambda)
      summary(x)

      x11()
      par(mfrow=c(1,2))
      hist(x,col="blue",freq=FALSE)
      x.sort=sort(x)
      lines(x.sort,dexp(x.sort,rate=lambda),col="red")
      cdf.x=ecdf(x)
      plot(cdf.x,col="blue",main="Fonction de reparition empirique et theorique")
      x.theo=seq(from=min(x),to=max(x),by=0.001)
      cdf.theo=pexp(x.theo,rate=lambda)
      lines(x.theo,cdf.theo,col="red",lty=2)

      ks.test(x,pexp,lambda)

      #b et c
      lambda=2
      nsim=10^3
      u=runif(nsim)
      x.inv=-log(u)/lambda

      x11()
      par(mfrow=c(1,2))
      hist(x.inv,col="blue",freq=FALSE)
      x.sort=sort(x.inv)
      lines(x.sort,dexp(x.sort,rate=lambda),col="red")
      cdf.x.inv=ecdf(x.inv)
      plot(cdf.x.inv,col="blue",main="Fonction de reparition empirique et theorique")
      x.theo=seq(from=min(x.inv),to=max(x.inv),by=0.001)
      cdf.theo=pexp(x.theo,rate=lambda)
      lines(x.theo,cdf.theo,col="red",lty=2)

      ks.test(x.inv,pexp,lambda)

      #d
      ks.test(x.inv,x)

      x11()
      plot(cdf.x.inv,col="blue",main="Fonctions de reparition loi exponentielle")
      plot(cdf.x,col="red",lty=2,add=TRUE)

      # Exercice 13 : simulation d'une loi de Cauchy par la méthode de l'inversion ...
      #
      ################################################################################
      nn=10^3
      uu=runif(nn)
      ss=2
      mu=1
      xx=ss*tan(pi*(uu-0.5)) + mu

      x11()
      par(mfrow=c(1,2))
      hist(xx,col="blue",freq=FALSE)
      x.sort=sort(xx)
      lines(x.sort,dcauchy(x.sort,1,2),col="red")
      cdf.x=ecdf(xx)
      plot(cdf.x,col="blue",main="Fonction de reparition empirique et theorique")
      x.theo=seq(from=min(xx),to=max(xx),by=0.001)
      cdf.theo=pcauchy(x.theo,1,2)
      lines(x.theo,cdf.theo,col="red",lty=2)

      ks.test(xx,rcauchy,1,2)

      xx.test=rcauchy(nn,1,2)

      ks.test(xx,xx.test)

      # Examinez les lignes de code suivantes et ...
      #
      algo.mh = function(x0,n)
      {
        x=x0;
        for(i in 1:n)
        {
          y=q.prop(x);
          a.ratio=(p.density(y)*q.density(y,x))/(p.density(x)*q.density(x,y));
          u=runif(1,0,1);
          if(u<=a.ratio){ x=y; }
        }
        x;
      }

      q.prop = function (x)
      {
        delta=0.5;
        lim=0.5*delta;
        res=runif(1,x-lim,x+lim);
      }
      q.density = function (x,y)
      {
        delta=0.5;
        lim=0.5*delta;
        res=dunif(y,x-lim,x+lim);
      }
      p.density = function (y)
      {
        d1=100;
        d2=100;
        if(y>=0)
        { res=df(y,d1,d2); }
        else
        { res=0; }
      }

      x0=0.5;
      m=10;
      n=1000;
      x=1:n;
      for (i in 1:n)
      {
        x[i]=algo.mh(x0,m);
        x0=x[i];
      }


      x11()
      par(mfrow=c(3,1))
      plot(x,typ="l",col="blue")
      title("Echantillons Metropolis-Hastings")
      hist(x,proba=T,col="blue")
      x.theo=seq(min(x),max(x),by=0.01)
      lines(x.theo,df(x.theo,100,100),col="red")
      acf(x,lag=50)

