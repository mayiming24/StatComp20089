## -----------------------------------------------------------------------------
x<-runif(10,1,10)
y<-runif(10,2,6)
plot(x,y,xlab = "ten random values",ylab = "ten other random values",bg="blue",pch=22)

## -----------------------------------------------------------------------------
tab <- readxl::read_excel("C:/rlearn/生产总值.xls",sheet=1)
knitr::kable(tab)

## -----------------------------------------------------------------------------
n<-5000
u<-runif(n)
x<-2/(sqrt(1-u))
hist(x,breaks=100,prob=TRUE,xlim=c(0,40),main = expression(f(x)==8*x^-3))
y<-seq(0,100,.1)
lines(y,8*y^-3,col="red")

## -----------------------------------------------------------------------------
f<-function(){
u1<-runif(1,min = -1,max = 1)
u2<-runif(1,min = -1,max = 1)
u3<-runif(1,min = -1,max = 1)
x<-1
  if(abs(u3)>=abs(u2)&&abs(u3)>=abs(u1))
    {x=u2}else
    x=u3
}

## -----------------------------------------------------------------------------
x<-1:50000
for (i in 1:50000) {
  x[i]=f()
  i=i+1
}
hist(x,prob=TRUE,breaks = 90,main = expression(f(x)==0.75*(1-x^2)))
y<-seq(-1,1,0.1)
lines(y,0.75*(1-y^2),col="red")

## -----------------------------------------------------------------------------
set.seed(12345)
m<- 1e5
x<- runif(m,min=0,max= pi/3)
M<- mean(sin(x))*pi/3
print(c(M,1-cos(pi/3)))

## -----------------------------------------------------------------------------
set.seed(2333)
m1<- 2000000
m2<- 2000000
X<- runif(m1,min = 0,max = 1)
Y<- runif(m2,min = 0,max = 1)
X_1<-exp(X)
Y_1<-0.5*exp(Y)+0.5*exp(1-Y)
M1<- mean(exp(X))
M2<- mean(0.5*exp(Y)+0.5*exp(1-Y))
V1<- var(X_1)
V2<- var(Y_1)
cat("这是简单蒙特卡洛方法的结果和方差:",M1,V1)
cat("这是采用对偶变量的方法的结果和方差:",M2,V2)

## -----------------------------------------------------------------------------
x<- seq(1.05,3,.01)
g<- function(x){
  ((x^2)*exp(-0.5*x^2))/((2*pi)^0.5)*(x>1)
}
y<- g(x)
plot(x,y,lwd=2,type = "l")

## -----------------------------------------------------------------------------
set.seed(123)
zt<- rnorm(500000,0,1)
kf<- rchisq(500000,3)
fg_1<- g(zt)/dnorm(zt)
fg_2<- g(kf)/dchisq(kf,3)
g1<- mean(fg_1)
se1<- sd(fg_1)
g2<- mean(fg_2)
se2<- sd(fg_2)
cat("重要函数取正态分布的结果和标准差:" ,g1 ,se1)
cat("重要函数取卡方分布的结果和标准差:" ,g2 ,se2)
print(integrate(g,1,Inf)) #这是标准的结果

## -----------------------------------------------------------------------------
g1<- function(x){
  (exp(-x)/(1+x^2))*(x>0)*(x<1)
}
result<- matrix(0,5,2)
m<- 2500
for (i in 1:5) {
  rn<- runif(m,0,1)
  q<- -log(1-((1-exp(-1))*(rn + i -1)/5))
  fg<- g1(q)/((5*(exp(-q)))/(1-exp(-1)))
  result[i,1]<- mean(fg)
  result[i,2]<- var(fg)
}
cat("分层重要抽样的结果是:",sum(result[,1])) 
cat("分层重要抽样的方差是:",sum(result[,2]))
print(integrate(g1,0,1)) #求一个积分的标准值

## -----------------------------------------------------------------------------
w<- 0
n<- 20
alpha<- 0.05
t<-0
s<- matrix(0,10000,2)
for (i in 1:10000) {
  x<- rlnorm(n,0,1)
  s[i,1]<- mean(log(x))+sd(log(x))*qt(alpha/2,n-1)/sqrt(n-1)
  s[i,2]<- mean(log(x))-sd(log(x))*qt(alpha/2,n-1)/sqrt(n-1)
  if(s[i,2]>0 & s[i,1]< 0) t<-t+1
}
t/10000

## -----------------------------------------------------------------------------
w1<- 0
n1<- 20
alpha<- 0.05
t<-0
s<- matrix(0,10000,2)
for (i in 1:10000) {
  x<- rchisq(n1,2)
  s[i,1]<- mean(x)+sd(x)*qt(alpha/2,n-1)/sqrt(n-1)
  s[i,2]<- mean(x)-sd(x)*qt(alpha/2,n-1)/sqrt(n-1)
  if(s[i,2]>2 & s[i,1]< 2) t<-t+1
}
t/10000

## -----------------------------------------------------------------------------
sk<- function(x){
  xbar<- mean(x)
  m3<- mean((x-xbar)^3)
  m2<- mean((x-xbar)^2)
  return(m3/m2^1.5)
}

## -----------------------------------------------------------------------------
a<- 0.05
n<- 30
m<- 2000
alpha<- c(seq(1,20,0.2))
N<- length(alpha)
sd<- sqrt(6*(n-2)/((n+1)*(n+3)))
cv<- qnorm(1-a/2,0,sd)
pwrbeta<- numeric(N)#储存不同参数的功效值
for (i in 1:N) {
  al<- alpha[i]
  sktests<- numeric(m)
  for (j in 1:m) {
    x<- rbeta(n,al,al)
    sktests[j]<- as.integer(abs(sk(x)) >= cv)
  }
  pwrbeta[i]<- mean(sktests)
}
plot(alpha,pwrbeta,type = "l",xlim = c(0,20),ylim = c(0,0.06))
abline(h=0.05,col="blue")

## -----------------------------------------------------------------------------
v<- seq(1,100,1)
M<- length(v)
pwrt<- numeric(M)
for (i in 1:M) {
  vi<- v[i]
  sktests<- numeric(m)
  for (j in 1:m) {
    x<- rt(n,vi)
    sktests[j]<- as.integer(abs(sk(x)) >= cv)
  }
  pwrt[i]<- mean(sktests)
}
plot(v,pwrt,type = "l",ylim = c(0,0.5))
abline(h=0.05,col="blue")

## -----------------------------------------------------------------------------
curve(dbeta(x, 2, 2), from = 0, to = 1)
curve(dbeta(x, 4, 4), from = 0, to = 1)
curve(dbeta(x, 16, 16), from = 0, to = 1)
curve(dnorm(x,0.5,0.1),from =0,to =1,col="red")

## -----------------------------------------------------------------------------
curve(dt(x,1),from = -4,to =4)
curve(dt(x,2),from = -4,to =4)
curve(dt(x,16),from = -4,to =4)
curve(dnorm(x),from = -4,to =4,col="red")

## -----------------------------------------------------------------------------
count5test<- function(x,y){
  X<- x-mean(x)
  Y<- y-mean(y)
  outx<- sum(X>max(Y))+sum(X<min(Y))
  outy<- sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outy,outx))>5))
}

## -----------------------------------------------------------------------------
Ftest<- function(x,y){
  xl<- length(x)
  yl<- length(y)
  sx<- var(x)
  sy<- var(y)
  T<- sy/sx
  return(as.integer(T<qf(0.055/2,xl-1,yl-2)|T>qf(1-0.055/2,xl-1,yl-1)))
}

## -----------------------------------------------------------------------------
n1<-10
rnum<-2000
sig1<- 1
sig2<- 1.5
minpowercount5<-mean(
  replicate(rnum,expr = {
    x<- rnorm(n1,0,sig1)
    y<- rnorm(n1,0,sig2)
    count5test(x,y)
  })
)
minpowerF<-mean(replicate(rnum,expr = {
  x<- rnorm(n1,0,sig1)
  y<- rnorm(n1,0,sig1)
  Ftest(x,y)
})
)
cat("这是小样本count5检验的功效",minpowercount5)
cat("这是小样本F检验的功效",minpowerF)

## -----------------------------------------------------------------------------
n2<-30
rnum<-2000
sig1<- 1
sig2<- 1.5
midpowercount5<-mean(
  replicate(rnum,expr = {
    x<- rnorm(n2,0,sig1)
    y<- rnorm(n2,0,sig2)
    count5test(x,y)
  })
)
midpowerF<-mean(replicate(rnum,expr = {
  x<- rnorm(n2,0,sig1)
  y<- rnorm(n2,0,sig2)
  Ftest(x,y)
})
)
cat("这是中样本count5检验的功效",midpowercount5)
cat("这是中样本F检验的功效",midpowerF)

## -----------------------------------------------------------------------------
n3<-100
rnum<-200
sig1<- 1
sig2<- 1.5
maxpowercount5<-mean(
  replicate(rnum,expr = {
    x<- rnorm(n3,0,sig1)
    y<- rnorm(n3,0,sig2)
    count5test(x,y)
  })
)
maxpowerF<-mean(replicate(rnum,expr = {
  x<- rnorm(n3,0,sig1)
  y<- rnorm(n3,0,sig2)
  Ftest(x,y)
})
)
cat("这是中样本count5检验的功效",maxpowercount5)
cat("这是中样本F检验的功效",maxpowerF)

## -----------------------------------------------------------------------------
n=c(10,20,30,50,100)
cv<- matrix(0,5,2)
cv[,2]<- (6/n)*qchisq(0.975,4)
cv[,1]<- (6/n)*qchisq(0.025,4)
cv<-data.frame(cv,row.names = c(10,20,30,50,100))
colnames(cv)<- c("l","r")
cv

## -----------------------------------------------------------------------------
library(MASS)
msk<- function(x){#x是一个n*d的矩阵，代表n个d维样本
  n=nrow(x)
  mlsig<- cov(x)*(n-1)/n
  cmlsig<- solve(mlsig)
  xbar<- colMeans(x)
  n2bd<- 0
  for (i in 1:n) {
    for (j in 1:n) {
      n2bd<-n2bd+(t(x[i,]-xbar)%*%cmlsig%*%(x[j,]-xbar))^3
    } 
  }
  return(n2bd/(n^2))
}

## -----------------------------------------------------------------------------
n<-c(10,20,30,50,100)
pM<- 1:5
Sigma <- matrix(c(1,0,0,1),2,2)
for (i in 1:5) {
  msktests<- 1:1000
  for (j in 1:1000) {
    x<- mvrnorm(n[i], rep(0, 2), Sigma)
    v<- msk(x)
    if(v<cv[i,1]|v>cv[i,2])
      msktests[j]<-1 else
        msktests[j]<-0
  }
  pM[i]<- mean(msktests)
}
cat("这是对应的模拟置信系数",pM)

## -----------------------------------------------------------------------------
n<-30
m<-200
Sigma <- matrix(c(1,0,0,1),2,2)
epsilon<-seq(0,1,0.01)
el<- length(epsilon)
mskpwr<-1:el
cv<-c((6/n)*qchisq(0.05,4),(6/n)*qchisq(0.95,4))
for (i in 1:el) {
  e<-epsilon[i]
  mskt<- numeric(m)
  for (j in 1:m) {
    k<- sample(c(0,1),replace = TRUE,size = n,prob = c(e,1-e))
    z<-matrix(0,20,2)
    x<- mvrnorm(n, rep(0, 2), Sigma)
    y<- mvrnorm(n, rep(0,2), 4*Sigma)
    for (h in 1:20) {
      if(k[h]==0)
        z[h,]<-x[h,]else
          z[h,]<-y[h,]
    }
    v<- msk(z)
    if(v<cv[1]|v>cv[2])
      mskt[j]<-1 else
        mskt[j]<- 0
  }
  mskpwr[i]<- mean(mskt)
}
 plot(epsilon,mskpwr,type = "l",ylim = c(0,0.5))
 abline(h= 0.1,col="blue")

## -----------------------------------------------------------------------------
#所以进行检验，发现其0.95的置信区间为
S<- (0.651*(1-0.651)+(1-0.676)*0.676)/10000
cat(qnorm(0.025,0.025,S),qnorm(0.975,0.025,S))

## -----------------------------------------------------------------------------
library(boot)
library(bootstrap)
corhat<- cor(law$LSAT,law$GPA)
n<- nrow(law)
corjack<- 1:n 
for (i in 1:n) {
  LAST<- law$LSAT[-i]
  GPA<- law$GPA[-i]
  corjack[i]<- cor(LAST,GPA)
}
corbias<- (n-1)*(mean(corjack)-corhat)
se<- sqrt((n-1)*mean((corjack-mean(corjack))^2))
cat("This is jacknife's bias and standard error:",corbias,se)

## -----------------------------------------------------------------------------
f <- function(x,i){
  t<- x[i]
  12/sum(t)
}
air<- as.vector(aircondit$hours)
obj<- boot(data = air,statistic = f,R= 2000)
obj
print(boot.ci(obj,type = c("basic","norm","perc","bca")))

## -----------------------------------------------------------------------------
scorm<- as.matrix(scor)
M<-cov(scorm)
t<- eigen(M)$val
val<- t[order(t,decreasing = TRUE)]
thetahat<-val[1]/sum(val)
n<- nrow(scor)
thetajack<-1:n
for (i in 1:n) {
  newscor<- scorm[-i,]
  M<-cov(newscor)
  t<- eigen(M)$val
  val<- t[order(t,decreasing = TRUE)]
  thetajack[i]<- val[1]/sum(val)
}
bias<- (n-1)*(mean(thetajack)-thetahat)
se<- sqrt((n-1)*mean((thetajack-mean(thetajack))^2))
cat("this is bias and se:",bias,se)

## -----------------------------------------------------------------------------
library(DAAG)
magnetic<- ironslag[,2]
chemical<- ironslag[,1]
n<- length(magnetic)
e1 <- e2 <- e3 <- e4<- matrix(0,n,n)
for (i in 2:n) {
  for (j in 1:i-1){
    y<- magnetic[-c(i,j)]
    x<- chemical[-c(i,j)]
    
    J1<-lm(y ~ x)
    yhat1i<- J1$coef[1]+J1$coef[2] * chemical[i]
    yhat1j<- J1$coef[1]+J1$coef[2] * chemical[j]
    e1[i,j]<- ((magnetic[i]-yhat1i)^2+(magnetic[j]-yhat1j)^2)/2
    
    J2<- lm(y ~ x + I(x^2))
    yhat2i<- J2$coef[1]+J2$coef[2]*chemical[i]+J2$coef[3]*chemical[i]^2
    yhat2j<- J2$coef[1]+J2$coef[2]*chemical[j]+J2$coef[3]*chemical[j]^2
    e2[i,j]<- ((magnetic[i]-yhat2i)^2+(magnetic[j]-yhat2j)^2)/2

    J3<- lm(log(y) ~ x)
    logyhat3i<- J3$coef[1]+J3$coef[2]*chemical[i]
    logyhat3j<- J3$coef[1]+J3$coef[2]*chemical[j]
    yhat3i<-exp(logyhat3i)
    yhat3j<-exp(logyhat3j)
    e3[i,j]<- ((magnetic[i]-yhat3i)^2+(magnetic[j]-yhat3j)^2)/2

    J4<- lm(log(y) ~ log(x))
    logyhat4i<- J4$coef[1]+J4$coef[2]*log(chemical[i])
    logyhat4j<- J4$coef[1]+J4$coef[2]*log(chemical[j])
    yhat4i<- exp(logyhat4i)
    yhat4j<- exp(logyhat4j)
    e4[i,j]<- ((magnetic[i]-yhat4i)^2+(magnetic[j]-yhat4j)^2)/2
  }
}
cat("these are four modoel's estimate error:",sum(e1)/1378,sum(e2)/1378,sum(e3)/1378,
    sum(e4)/1378)

## -----------------------------------------------------------------------------
count5test<- function(x,y){
  X<- x-mean(x)
  Y<- y-mean(y)
  outx<- sum(X>max(Y))+sum(X<min(Y))
  outy<- sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outy,outx))>5))
}
x<- rnorm(40,0,1)
y<- rnorm(30,0,1)
count5<- function(x,y,B){
  I<- numeric(B)
  xl<-length(x)
  yl<-length(y)
  for (i in 1:B) {
    xi<- sample(1:xl,20,replace = F)
    yi<- sample(1:yl,20,replace = F)
    newx<- x[xi]
    newy<- y[yi]
    I[i]<- count5test(newx,newy)
  }
  return((1+sum(I))/(B+1))
}
count5(x,y,1000)

## -----------------------------------------------------------------------------
library(Ball)
library(energy)
library(boot)
library(RANN)
m<- 100
k<- 3
p<- 2
mu<- 0.3
set.seed(233)
n1<-n2<- 50
R<- 999
n<- n1+n2
N<- c(n1,n2)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}
p1.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0,1.5),ncol=p);
  y <- cbind(rnorm(n2),rnorm(n2,mean=0.3));
  z <- rbind(x,y)
  p1.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p1.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p1.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow1 <- colMeans(p1.values<alpha)
pow1#上课的示例
p2.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0,1.6),ncol=p);
  y <- matrix(rnorm(n2*p,0,1),ncol=p);
  z <- rbind(x,y)
  p2.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p2.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p2.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1
pow2 <- colMeans(p2.values<alpha)
pow2#等均值不等方差

p3.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0.3,1.4),ncol=p);
  y <- matrix(rnorm(n2*p,0,1),ncol=p);
  z <- rbind(x,y)
  p3.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p3.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p3.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1
pow3 <- colMeans(p3.values<alpha)
pow3#不等均值不等方差

p4.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rt(n1*p,1) ,ncol=p)
  yy<- 1:n2*p
  a<- matrix(0,2,n2*p)
  a[1,]<- rnorm(n2*p,mean = -.1,2)
  a[2,]<- rnorm(n2*p,mean = .1,2)
  index<- sample(c(1,2),p*n2,replace = T,prob = c(.5,.5))
  for (j in 1:100) {
    yy[j]<-a[index[j],j]
  }
  y<-matrix(yy,ncol = p)
  z <- rbind(x,y)
  p4.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p4.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p4.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1
pow4 <- colMeans(p4.values<alpha)
pow4#自由度为1的t分布

p5.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rt(n1*p,1) ,ncol=p)
  yy<- 1:n2*p
  a<- matrix(0,2,n2*p)
  a[1,]<- rnorm(n2*p,mean = -0.01,2)
  a[2,]<- rnorm(n2*p,mean = 0.01,2)
  index<- sample(c(1,2),p*n2,replace = T,prob = c(.5,.5))
  for (j in 1:100) {
    yy[j]<-a[index[j],j]
  }
  y<-matrix(yy,ncol = p)
  z <- rbind(x,y)
  p5.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p5.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p5.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1
pow5 <- colMeans(p5.values<alpha)
pow5 #自由度为1的t分布和双正态

p6.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(100*p,0,1.5),ncol=p);
  y <- cbind(rnorm(10),rnorm(10));
  z <- rbind(x,y)
  p6.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p6.values[i,2] <- eqdist.etest(z,sizes=c(100,10),R=R)$p.value
  p6.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow6 <- colMeans(p6.values<alpha)
pow6#不平衡样本


## -----------------------------------------------------------------------------
la.Me<- function(sigma,x_0,N){
  x<- numeric(N)#链
  x[1]<- x_0
  u<- runif(N)
  k<- 0#拒绝
  for (i in 2:N) {
    y<- rnorm(1,x[i-1],sigma)
    if(u[i]<= (exp(-abs(y))/exp(-abs(x[i-1]))))
      x[i]<- y else{
        x[i]<- x[i-1]
        k<- k+1
      }
  }
  return(list(x=x,k=N-k))
}
la1<- la.Me(0.05,2,2000)
la2<- la.Me(0.5,2,2000)
la3<- la.Me(1,2,2000)
la4<- la.Me(2,2,2000)
plot(la1$x,type = "l",col=1)
plot(la2$x,type = "l",col=2)
plot(la3$x,type = "l",col=3)
plot(la4$x,type = "l",col=4)
print(c(la1$k/2000,la2$k/2000,la3$k/2000,la4$k/2000))

## -----------------------------------------------------------------------------
#定义Gelman-Rubin函数
Gel<- function(psi){
  psi<- as.matrix(psi)
  n<-nrow(psi)
  k<-ncol(psi)
  psi.means<- rowMeans(psi)
  B<- n*var(psi.means)
  psi.w<- apply(psi, 1,var)
  W<- mean(psi.w)
  v.hat<- W*(n-1)/n + (B/n)
  r.hat<- v.hat/W
  return(r.hat)
}
las<- function(n,sigma,x_0,N){
  data<- matrix(0,nrow = n,ncol = N)
  for (i in 1:n) {
    data[i,]<- la.Me(sigma,x_0,N)$x
  }
  return(data)
}
r<- seq(500,10000,by= 100)
l<- length(r)
Gelresult<-numeric(l)
for (i in 1:l) {
  newla<- las(20,0.2,20,r[i])
  psi<- t(apply(newla,1,cumsum))
  for (j in 1:nrow(psi)) {
    psi[j,]<- psi[j,]/(1:ncol(psi))
  }
  Gelresult[i]<- Gel(psi)
}
plot(r,Gelresult,type = 'l')
abline(h=1.2,col=2)

## -----------------------------------------------------------------------------
a<- c(4:25,100,500,1000)
result<- numeric(length(a))
for (i in 1:25) {
  k<-a[i]
  s<- function(a){pt(sqrt((k-1)*a^2/(k-a^2)),k-1)-pt(sqrt(k*a^2/(k+1-a^2)),k)}
  result[i]<- uniroot(s,interval = c(0.01,sqrt(k)-0.01))$root
}
data.frame(cbind(a,result))


## -----------------------------------------------------------------------------
L <- c(0.01,0.1) #initial estimation
tol <- .Machine$double.eps^0.5
L.old<- L
N<-5 #max iter
like<- 1:N
pq<-matrix(0,N,2)
for (i in 1:N) {
  LL <- function(lam,y){
    lam3<- 1- lam[1]-lam[2]
    f1<- 2*444*log(lam[1]) + 2*132*log(lam[2])+ 2*361*log(lam3)+63*log(lam[1]*lam[2])
    L3<- 1-L[1]-L[2]
    f2<- (2*L3/(L[1]+2*L3))*444*log(lam3/lam[1]) + (2*L3/(L[2]+2*L3))*132*log(lam3/lam[2])
    f<- f1+f2
    return(-f)
  }
  opt<- optim(L,LL,y=y)
  L<- opt$par
  like[i]<- -opt$value
  pq[i,]<-L
  if(sum(abs(L-L.old))<tol)break
  L.old<-L
}
for (j in 1:i) {
  cat(j,'次')
  print('似然比：')
  print(like[j])
  print('p,q的估计')
  print(pq[j,])
}

f<- function(lam){
  lam3<- 1-lam[1]-lam[2]
  f1<- 444*log(lam[1]^2+2*lam[1]*lam3)+132*log(lam[2]^2+2*lam[2]*lam3)
  f2<- 63*log(lam[1]*lam[2])+ 361*2*log(lam3)
  return(-(f1+f2))
}
#极大似然估计
optim(c(0.3,0.1),f)$par

## -----------------------------------------------------------------------------
f <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
for (i in 1:4) {
  p <- parse(text = f[i])[[1]]
  fit<- lm(p,data=mtcars)
  print(fit)
}

lapply(f,function(x){lm(x,data = mtcars)})

## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
sapply(trials, function(x){return(x$p.value)})

## -----------------------------------------------------------------------------
lapply1<-function(X,funct,t){
  Map(function(x){vapply(x, funct,t)},X)
} 
#其中t为输出类型，X是输入列表的列表，funct是函数
trials <- replicate(
  6,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
trials1 <- replicate(
  5,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
X<- list(trials,trials1)
lapply1(X,function(x){return(x$p.value)},numeric(1))
#函数输出效果

## -----------------------------------------------------------------------------
library(StatComp20089)

sigma=1
x_0=0
N = 2000

la.Me<- function(sigma,x_0,N){
  x<- numeric(N)#链
  x[1]<- x_0
  u<- runif(N)
  k<- 0#拒绝
  for (i in 2:N) {
    y<- rnorm(1,x[i-1],sigma)
    if(u[i]<= (exp(-abs(y))/exp(-abs(x[i-1]))))
      x[i]<- y else{
        x[i]<- x[i-1]
        k<- k+1
      }
  }
  return(list(x=x,k=N-k))
}
x1=la.Me(1,0,2000)$x

x2=cla(sigma,x_0,N)

plot(x1,col="red",type="l",ylab="point")
plot(x2,col="green",type="l",ylab="point")

## -----------------------------------------------------------------------------
Rf = x1[-(1:500)]
Rcppf = x2[-(1:500)]
qqplot(Rf,Rcppf)
abline(a=0,b=1,col='red')

## -----------------------------------------------------------------------------
library(microbenchmark)

(time = microbenchmark(Rf=la.Me(2,0,2000)$x,Rcppf=cla(sigma,x_0,N)))

