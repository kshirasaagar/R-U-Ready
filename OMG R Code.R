
#Installing Necessary R Packages

install.packages('combinat');
install.packages('vcd');
install.packages('methods');
install.packages('Bhat');
install.packages('ctest');
install.packages('mass');
install.packages('mgcv');
install.packages('MASS');
install.packages('modreg');
install.packages('Hmisc');
install.packages('design');
install.packages('survival');
install.packages('brlr');
install.packages('nnet');
install.packages('vgam');
install.packages('ordinal');
install.packages('gnlm');
install.packages('Design');
install.packages('exactLoglinTest');
install.packages('multiv');
install.packages('CoCoAn');
install.packages('repeated');
install.packages('yags');
install.packages('gee');
install.packages('geepack');
install.packages('glmmML');
install.packages('rmutil');
install.packages('e1071');
install.packages('flexmix');
install.packages('npmlreg');
install.packages('Rcapture');
install.packages('aod');

library(combinat)
n <- c(100,20,10)
p <- matrix(c(.3,.1,.5,.1,.1,.2,.6,.8,.3),3)
my.rmultinomial(n,p)

my.rmultinomial<-function (n, p, rows = max(c(length(n), nrow(p)))) 
{
  rmultinomial.1 <- function(n, p) {
    k <- length(p)
    tabulate(sample(k, n, replace = TRUE, prob = p), nbins = k)
  }
  n <- rep(n, length = rows)
  p <- p[rep(1:nrow(p), length = rows), , drop = FALSE]
  sapply(1:rows, function(i) rmultinomial.1(n[i],  p[i, ]))
}

##### Vegetarian Example ######

# binconf

binconf(0, 25, method="wilson")
PointEst         Lower     Upper 
0 1.202937e-017 0.1331923

# prop.test

res<-prop.test(x=0,n=25,correct=F)
res$conf.int
[1] 0.0000000 0.1331923
attr(, "conf.level"):
  [1] 0.95

## exact intervals
binom.test(x=0, n=25)

### Pearson's Chi-square Statistic (Mendel's theories)

chisq.test(x=c(6022,2001),p=c(.75,.25))

### Maximum likelihood estimation
obj.res<-deriv(~-(-4.292*pi1 + y1*log(la1) + y2*log(la2) + y3*log(pi1-la1) + y4*log(3.292*pi1-la2)),
               c("la1","la2","pi1"),
               function(la1, la2, pi1, y1, y2, y3, y4) NULL)
obj.function<-function(p, y){
  value<--(-4.292*p[3] + y[1]*log(p[1]) + y[2]*log(p[2]) + y[3]*log(p[3]-p[1]) + y[4]*log(3.292*p[3]-p[2]))
  la1<-p[1]; la2<-p[2]; pi1<-p[3]; y1<-y[1]; y2<-y[2]; y3<-y[3]; y4<-y[4]
  attr(value, "gradient")<-attr(obj.res(la1, la2, pi1, y1, y2, y3, y4), "gradient")
  value
}
nlm(f=obj.function, p=c(11, 62, 2*(11+4)), y=c(11, 62, 4, 7), typsize=c(1,1,.10))

# optim
obj.function<-function(p, y){
  -(-4.292*p[3] + y[1]*log(p[1]) + y[2]*log(p[2]) + y[3]*log(p[3]-p[1]) + y[4]*log(3.292*p[3]-p[2]))
}

obj.res<-deriv(~-(-4.292*pi1 + y1*log(la1) + y2*log(la2) + y3*log(pi1-la1) + y4*log(3.292*pi1-la2)),
               c("la1","la2","pi1"),
               
               function(la1, la2, pi1, y1, y2, y3, y4) NULL)
obj.gr<-function(p, y){
  la1<-p[1]; la2<-p[2]; pi1<-p[3]; y1<-y[1]; y2<-y[2]; y3<-y[3]; y4<-y[4]
  attr(obj.res(la1, la2, pi1, y1, y2, y3, y4), "gradient")
}
optim(par=c(11, 62, 2*(11+4)), fn=obj.function, gr=obj.gr, lower=c(0,0,0), method="L-BFGS-B",
      control=list(parscale=c(1,1,10)), y=c(11, 62, 4, 7))


## LR chi-squared test

# mendel's theories
obs<-c(6022,2001)
expected<-8023*c(.75,.25)
1-pchisq(2 * sum(obs * log(obs/expected)),df=1)



#### Chapter 2 ####

## Comparing two proportions ##

# Table 2.1 (attack v. no attack)

x<-c(104,189) # aspirin, placebo
n<-c(11037,11034)

# test H0:p1=p2 (equal probabilities of heart attack)
prop.test(x,n,p=c(.5,.5))
prop.test(x,n)$p.value

# Test H0:p1=p2 v. H1:p1<p2
prop.test(x,n,alt="less")$p.value

# sample difference of proportions:
temp<-prop.test(x,n)  
names(temp$estimate)<-NULL  # optional
temp$estimate[[1]]-temp$estimate[[2]]

# relative risk
temp$estimate[[2]]/temp$estimate[[1]]

# odds ratio
x[2]*(n[1]-x[1])/(x[1]*(n[2]-x[2]))

## partial tables ##

# Create a cross-classified table out of Table 2.6
vic.race<-c("white","black")
def.race<-vic.race
death.penalty<-c("yes", "no")
datalabel<-list(defendant=def.race,death=death.penalty,victim=vic.race)
table.2.6<- expand.grid(defendant=def.race,death=death.penalty,victim=vic.race) 
Data<-c(53, 11, 414, 37, 0, 4, 16, 139)
table.2.6<-cbind(table.2.6,count=Data)
temp<-xtabs(count~defendant+death+victim ,data=table.2.6)
update(temp, formula= ~.-victim)
methods("update")   # check that the method is set

# conditional odds ratios
apply(temp,3,function(x) x[1,1]*x[2,2]/(x[2,1]*x[1,2]))

summary(oddsratio(temp, log=F, stratum=3))

summary.table(temp)

# association in tables

income<-c("<15000","15000-25000","25000-40000",">40000")
jobsat<-c("VD","LD","MS","VS")
table.2.8<-expand.grid(income=income,jobsat=jobsat) 
Data<-c(1,2,1,0,3,3,6,1,10,10,14,9,6,7,12,11)
table.2.8<-cbind(table.2.8,count=Data)
(temp<-xtabs(count~income+jobsat,table.2.8))

library(vcd)
assoc.stats(temp)

## Paik diagram (by Tobias Verbeke)


# example from Paik(1985)
#
hiring <- array(c(550,1450,1250,2750,
                  2950,1050,800,200),
                dim=c(2,2,2),
                dimnames=list(Status=c("Hired", "Denied"),
                              Sex=c("Male", "Female"),
                              Department=c("L", "H")))
hiringL <- hiring[,,1]
hiringH <- hiring[,,2]
propL <- prop.table(hiringL, 2)[1,]
propH <- prop.table(hiringH, 2)[1,]
hiringall <- hiringL + hiringH
propall <- prop.table(hiringall, 2)[1,]
#


# function following Paik(1985)
#
paik <- function(x, xlim=c(-0.33,1.33), ylim=c(0,1), marg.assoc=TRUE, with.rug=FALSE,...){
  # extract 2 x 2 cont. tables
  x1 <- x[,,1]
  x2 <- x[,,2]
  propx1 <- prop.table(x1, 2)[1,]
  propx2 <- prop.table(x2, 2)[1,]
  np <- seq(0, 2*pi, length=120)
  plot.new()
  # *asp*ect ratio 1 for readability of correlations
  # plot.window(c(-0.33,1.33), c(0,1), asp = 1)
  plot.window(xlim, ylim, asp=1, ...)  # changed by LT
  axis(side=1,
       at=c(0,1),
       labels=dimnames(x)[[2]])
  mtext(text=names(dimnames(x))[2],
        side=1,
        line=2.5)
  # axis(side=2,at=seq(0, 1, by=0.2))
  axis(side=2,at=seq(0, ylim[2], by=diff(ylim)/5))    # changed by LT
  mtext(text=paste("prop. ",
                   names(dimnames(x))[1], " = ",
                   dimnames(x)[[1]][1], sep=""),
        side=2,
        line=2.5)
  # centers and slopes
  points(c(0, 1, 0, 1), c(propx1, propx2), pch=20)
  text(x=c(0, 1, 0, 1), y=c(propx1, propx2),c(rep(dimnames(x)[[3]],each=2)))    # added by LT
  segments(c(0,0),
           c(propx1[1], propx2[1]),
           c(1,1),
           c(propx1[2], propx2[2]),
           lty="solid")
  # relative sample sizes
  relx1 <- margin.table(x1, 2)/sum(x)
  relx2 <- margin.table(x2, 2)/sum(x)
  # length of rays
  rx11 <- 0.5 * relx1[1]
  rx12 <- 0.5 * relx1[2]
  rx21 <- 0.5 * relx2[1]
  rx22 <- 0.5 * relx2[2]
  # circles
  points(rx11*cos(np),
         propx1[1] + rx11*sin(np),
         type="l")
  points(1 + rx12*cos(np),
         propx1[2] + rx12*sin(np),
         type="l")
  points(rx21*cos(np),
         propx2[1] + rx21*sin(np),
         type="l")
  points(1 + rx22*cos(np),
         propx2[2] + rx22*sin(np),
         type="l")
  if (marg.assoc == TRUE){
    xall <- x1 + x2
    propxall <- prop.table(xall, 2)[1,]
    points(c(0, 1), propxall, pch=20)
    segments(0, propxall[1], 1, propxall[2],
             lty="dashed")
  }
  if (with.rug == TRUE){
    ifelse(marg.assoc,
           rugvec <- c(propx1, propx2, propxall),
           rugvec <- c(propx1, propx2))
    rug(rugvec, side=2)
  }
}

# construct Paik diagram for death penalty data
paik(xtabs(count~defendant+death+victim ,data=table.2.6))


#### Chapter 3 ####

## Confidence intervals ##

#source("C:/Program Files/Insightful/splus61/users/CDA/design.table.q")
#source("C:/Program Files/Insightful/splus61/users/CDA/factor.names.q")

library(methods)
Drug<-c("Placebo","Aspirin")
Infarction<-c("yes","no")
table.3.1<-expand.grid(drug=Drug,infarction=Infarction) 
Data<-c(28,18,656,658)
table.3.1<-cbind(table.3.1,count=Data)
tapply(table.3.1$count,table.3.1[,1:2], sum)

# wald confidence interval

Wald.ci<-function(Table, aff.response, alpha=.05){
  # Gives two-sided Wald CI's for odds ratio, difference in proportions and relative risk.
  # Table is a 2x2 table of counts with rows giving the treatment populations
  # aff.response is a string like "c(1,1)" giving the cell of the beneficial response and the treatment category 
  # alpha is significance level
  
  pow<-function(x, a=-1) x^a 
  z.alpha<-qnorm(1-alpha/2)
  
  if(is.character(aff.response)) where<-eval(parse(text=aff.response)) else where<-aff.response
  Next<-as.numeric(where==1) + 1
  
  # OR
  odds.ratio<-Table[where[1],where[2]]*Table[Next[1],Next[2]]/(Table[where[1],Next[2]]*Table[Next[1],where[2]])
  se.OR<-sqrt(sum(pow(Table)))
  ci.OR<-exp(log(odds.ratio) + c(-1,1)*z.alpha*se.OR)
  
  # difference of proportions 
  p1<-Table[where[1],where[2]]/(n1<-Table[where[1],Next[2]] + Table[where[1],where[2]])
  p2<-Table[Next[1],where[2]]/(n2<-Table[Next[1],where[2]]+Table[Next[1],Next[2]])
  
  se.diff<-sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  ci.diff<-(p1-p2) + c(-1,1)*z.alpha*se.diff
  
  # relative risk
  RR<-p1/p2
  se.RR<-sqrt((1-p1)/(p1*n1) + (1-p2)/(p2*n2))
  ci.RR<-exp(log(RR) + c(-1,1)*z.alpha*se.RR)
  
  list(OR=list(odds.ratio=odds.ratio, CI=ci.OR), proportion.difference=list(diff=p1-p2, CI=ci.diff), relative.risk=list(relative.risk=RR,CI=ci.RR))
}
Wald.ci(temp, "c(1, 1)")        # or use Wald.ci(temp, c(1, 1))

# score confidence interval 

gfun<-function(p,y0){
  sum((c(-(28/p[1])+(656/(1-p[1]))-p[3], -(18/p[2])+(658/(1-p[2]))+p[3], p[1]-p[2])-y0)^2)
}
optim(fn=gfun,par=c(28/(28+656),18/(18+658),7), method="BFGS",y0=c(0,0,-.1),control=list(trace=2,REPORT=10))

score.ci<-function(Delta) {
  temp<-optim(fn=gfun,par=c(28/(28+656),18/(18+658),7), method="BFGS",y0=c(0,0,Delta), 
              control=list(ndeps=c(rep(.000000001,3))))
  p<-temp$par[1:2]
  z<-(p[1]-p[2]-Delta)/sqrt(p[1]*(1-p[1])/684 + p[2]*(1-p[2])/676)
}
Delta<-seq(-.2,.2,.0005)
Delta<-seq(-.05,.05,.0001)
z<-sapply(Delta,score.ci)
Delta[abs(z)< qnorm(.975)]

# profile likelihood ci

library(Bhat)

# neg. log-likelihood of "new" multinomial model
nlogf <- function (p) { 
  p1p<-p[1]
  pp1<-p[2]
  theta<-p[3]
  n11 <- table.3.1$count[1]
  n21<-table.3.1$count[2]
  n12<-table.3.1$count[3]
  n22<-table.3.1$count[4]
  p22<-(-1 + p1p + pp1 + 2*theta - (p1p + pp1)*theta - sqrt(-4*p1p*(-1 + pp1)*(-1 + theta) + 
    (1 + p1p + pp1*(-1 + theta) - p1p*theta)^2))/(2*(-1 + theta))
  p11 <- (1 + p1p*(-1 + theta) + pp1*(-1 + theta) - sqrt(-4*p1p*(-1 + pp1)*(-1 + theta) + 
    (1 + p1p + pp1*(-1 + theta) - p1p*theta)^2))/(2*(-1 + theta))
  p21 <- (-1 + p1p + pp1*(-1 + theta) - p1p*theta + sqrt(-4*p1p*(-1 + pp1)*(-1 + theta) + 
    (1 + p1p + pp1*(-1 + theta) - p1p*theta)^2))/(2*(-1 + theta))
  p12 <- (-1 + pp1 + p1p*(-1 + theta) - pp1*theta + sqrt(-4*p1p*(-1 + pp1)*(-1 + theta) + 
    (1 + p1p + pp1*(-1 + theta) - p1p*theta)^2))/(2*(-1 + theta))
  
  -(n11*log(p11) + n12*log(p12) + n21*log(p21) + n22*log(p22))
}

# check answer with MLEs
nlogf(c((28+656)/(684+676),(28+18)/(684+676),28*658/(18*656)))

# neg LLH of multinomial
logLH<-function(p){
  n11 <- table.3.1$count[1]
  n21<-table.3.1$count[2]
  n12<-table.3.1$count[3]
  n22<-table.3.1$count[4]
  p11<-p[1]
  p12<-p[2]
  p21<-p[3]
  -(n11*log(p11) + n12*log(p12) + n21*log(p21) + n22*log(1-p11-p12-p21))
}

# check answer with MLEs
logLH(c(28/(684+676), 656/(684+676),18/(684+676)))

# check MLEs are what we expect
x <- list(label=c("p11","p12","p21"),est=c(28/(684+676),656/(684+676),18/(684+676)),
          low=c(0.001,0.001,0.001),upp=c(1,1,1))
dfp(x,f=logLH)

# Now get CI on theta
x <- list(label=c("p1p","pp1","theta"),est=c((28+656)/(684+676),(28+18)/(684+676),1.56),low=c(0,0,0),upp=c(1,1,100))
plkhci(x,nlogf,"theta")

## Tests of Independence ##

# Table 3.2 (p.80)

# set up data and expected counts
religion.counts<-c(178,138,108,570,648,442,138,252,252)
table.3.2<-cbind(expand.grid(list(Religious.Beliefs=c("Fund", "Mod", "Lib"), Highest.Degree=c("<HS","HS or JH", "Bachelor or Grad"))),count=religion.counts)
table.3.2.array<-tapply(table.3.2$count,table.3.2[,1:2], sum)


# Pearson Chi-squared test
(res<-chisq.test(table.3.2.array))
res$expected
Highest.Degree
Religious.Beliefs      <HS HS or JH Bachelor or Grad
Fund 137.8078 539.5304         208.6618
Mod  161.4497 632.0910         244.4593
Lib  124.7425 488.3786         188.8789
(res<-chisq.test(table.3.2, sim=T)) # simulate p-value


# LRT
2*sum(table.3.2.array*log(table.3.2.array/res$expected))
# or
table.3.2.df<-cbind(expand.grid(list(Religious.Beliefs=c("Fund", "Mod", "Lib"), Highest.Degree=c("<HS","HS or JH", "Bachelor or Grad"))),count=religion.counts)
res<-glm(count~Religious.Beliefs+Highest.Degree,data=table.3.2.df,family=poisson)
predict(res,type="response")

# using vcd
summary(assoc.stats(table.3.2.array))
expected(table.3.2.array)
mar.table(table.3.2.array)


## Follow -up tests ##
# pearson residuals (same as splus)
# partitioning chi-square (same as splus)

## Two-way Table with Ordered Classifications ##
# linear trend (same as splus)
# monotone trend
levels(table.2.8$income)<-c(7.5,20,32.5,60)
levels(table.2.8$jobsat)<-1:4

Gamma2.f<-function(x, pr=0.95)
{
  # x is a matrix of counts.  You can use output of crosstabs or xtabs in R.
  # A matrix of counts can be formed from a data frame by using design.table.
  
  # Confidence interval calculation and output from Greg Rodd
  
  # Check for using S-PLUS and output is from crosstabs (needs >= S-PLUS 6.0)
  if(is.null(version$language) && inherits(x, "crosstabs")) { oldClass(x)<-NULL; attr(x, "marginals")<-NULL}
  
  n <- nrow(x)
  m <- ncol(x)
  pi.c<-pi.d<-matrix(0,nr=n,nc=m)
  
  row.x<-row(x)
  col.x<-col(x)
  
  for(i in 1:(n)){
    for(j in 1:(m)){
      pi.c[i, j]<-sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
      pi.d[i, j]<-sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
    }
  }
  
  C <- sum(pi.c*x)/2
  D <- sum(pi.d*x)/2
  
  psi<-2*(D*pi.c-C*pi.d)/(C+D)^2
  sigma2<-sum(x*psi^2)-sum(x*psi)^2
  
  gamma <- (C - D)/(C + D)
  pr2 <- 1 - (1 - pr)/2
  CIa <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
  
  list(gamma = gamma, C = C, D = D, sigma = sqrt(sigma2), Level = paste(
    100 * pr2, "%", sep = ""), CI = paste(c("[", max(CIa[1], -1), 
                                            ", ", min(CIa[2], 1), "]"), collapse = ""))     
}

temp<-xtabs(formula = count ~ income + jobsat, data = table.2.8)
res<-Gamma2.f(temp)


## Small samples tests of independence

# Fisher's Exact test

# Table 3.8 (p.92)
# fisher.test() gives a two-sided test
(fisher.test(matrix(c(3,1,1,3),byrow=T,ncol=2), alternative="greater"))

Fisher's Exact Test for Count Data

data:  matrix(c(3, 1, 1, 3), byrow = T, ncol = 2) 
p-value = 0.2429
alternative hypothesis: true odds ratio is greater than 1 
95 percent confidence interval:
0.3135693       Inf 
sample estimates:
odds ratio 
6.408309 

# Fisher's Exact test for I x J tables

table.3.9<-matrix(c(25,25,12,0,1,3),byrow=T,ncol=3)

Gamma2.f(table.3.9)
$gamma:
  [1] 0.8716578

$C:
  [1] 175

$D:
  [1] 12

library(combinat)
num<-prod(fact(rep(1,2)%*%table.3.9))*prod(fact(rep(1,3)%*%t(table.3.9)))
den<-fact(sum(table.3.9))*prod(fact(table.3.9))
term1<-num/den

temp<-matrix(c(25,26,11,0,0,4),byrow=T,ncol=3)

num<-prod(fact(rep(1,2)%*%temp))*prod(fact(rep(1,3)%*%t(temp)))
den<-fact(sum(temp))*prod(fact(temp))
term2<-num/den

term1+term2
[1] 0.01830808

fisher.test(table.3.9)$p.value/2
[1] 0.01704545


# Exact Small-sample confidence intervals for 2x2 tables

library(ctest)
(fisher.test(matrix(c(3,1,1,3),byrow=T,ncol=2), alternative="two.sided", or=1))


#Fisher's Exact Test for Count Data

data:  matrix(c(3, 1, 1, 3), byrow = T, ncol = 2) 
p-value = 0.4857
alternative hypothesis: true odds ratio is not equal to 1 
95 percent confidence interval:
0.2117329 621.9337505 
sample estimates:
odds ratio 
6.408309 

# Using non-central hypergeometric function

f<-function(x, alpha,t0){
resl<-hypergeometric(4,4,8,x[1])
resu<-hypergeometric(4,4,8,x[2])
sum(c(1-resl$p(t0-1) - alpha/2, resu$p(t0)-alpha/2)^2)
}

optim(par=c(.22, 622), fn=f, method="BFGS", alpha=.05, t0=3, control=list(parscale=c(1,100)))

res<-hypergeometric(4,4,8,.212)     # lower endpoint
1-res$p(2)

res<-hypergeometric(4,4,8,626.24)        # Upper endpoint
res$p(3)    # p is the CDF returned by hypergeometric; we evaluate it at t=3

# mid-p-value

f<-function(x, alpha,t0){
resl<-hypergeometric(4,4,8,x[1])
resu<-hypergeometric(4,4,8,x[2])
sum(c(1-resl$p(t0-1) -.5*resl$d(t0) - alpha/2, resu$p(t0-1) + .5*resu$d(t0) -alpha/2)^2)
}

optim(par=c(.22, 622), fn=f, method="BFGS", alpha=.05, t0=3, control=list(parscale=c(1,100)))

res<-hypergeometric(4,4,8,308.5567363)
res$p(2) + .5*res$d(3)

res<-hypergeometric(4,4,8,0.3100547)
1-res$p(2) - .5*res$d(3)

## GLIMs for binary data

# Table 4.2

n<-c(1379, 638, 213, 254)
snoring<-rep(c(0,2,4,5),n)
y<-rep(rep(c(1,0),4),c(24,1355,35,603,21,192,30,224))

# logit model

logitreg <- function(x, y, wt = rep(1, length(y)),
intercept = T, start = rep(0, p), ...)
{
if(!exists("optim")) library(mass)
fmin <- function(beta, X, y, w) {
p <- plogis(X %*% beta)
-sum(2 * w * ifelse(y, log(p), log(1-p)))
}
gmin <- function(beta, X, y, w) {
eta <- X %*% beta; p <- plogis(eta)
t(-2 * (w *dlogis(eta) * ifelse(y, 1/p, -1/(1-p))))%*%X
}
if(is.null(dim(x))) dim(x) <- c(length(x), 1)
dn <- dimnames(x)[[2]]
if(!length(dn)) dn <- paste("Var", 1:ncol(x), sep="")
p <- ncol(x) + intercept
if(intercept) {x <- cbind(1, x); dn <- c("(Intercept)", dn)}
if(is.factor(y)) y <- (unclass(y) != 1)

fit <- optim(start, fmin, gmin, X = x, y = y, w = wt, ...)
names(fit$par) <- dn
cat("\nCoefficients:\n"); print(fit$par)
cat("\nResidual Deviance:", format(fit$value), "\n")
cat("\nConvergence message:", fit$convergence, "\n")
invisible(fit)
}


(logit.fit<-logitreg(x=snoring, y=y, hessian=T))



# linear probability fit

lpmreg <- function(x, y, wt = rep(1, length(y)),
intercept = T, start = rep(0, p), ...)
{
if(!exists("optim")) library(mass)

fmin <- function(beta, X, y, w) {
p <- X %*% beta
-sum(2 * w * ifelse(y, log(p), log(1-p)))
}
gmin <- function(beta, X, y, w) {
p <- X %*% beta;
t(-2 * (w * ifelse(y, 1/p, -1/(1-p))))%*% X
}
if(is.null(dim(x))) dim(x) <- c(length(x), 1)
dn <- dimnames(x)[[2]]
if(!length(dn)) dn <- paste("Var", 1:ncol(x), sep="")
p <- ncol(x) + intercept
if(intercept) {x <- cbind(1, x); dn <- c("(Intercept)", dn)}
if(is.factor(y)) y <- (unclass(y) != 1)
fit <- optim(start, fmin, gmin, X = x, y = y, w = wt, ...)
names(fit$par) <- dn
cat("\nCoefficients:\n"); print(fit$par)
cat("\nResidual Deviance:", format(fit$value), "\n")
cat("\nConvergence message:", fit$convergence, "\n")
invisible(fit)
}

(lpm.fit<-lpmreg(x=snoring, y=y, start=c(.05,.05), hessian=T))



# probit model

probitreg <- function(x, y, wt = rep(1, length(y)),
intercept = T, start = rep(0, p), ...)
{
if(!exists("optim")) library(mass)

fmin <- function(beta, X, y, w) {
p <- pnorm(X %*% beta)
-sum(2 * w * ifelse(y, log(p), log(1-p)))
}
gmin <- function(beta, X, y, w) {
eta <- X %*% beta; p <- pnorm(eta)
t(-2 * (w *dnorm(eta) * ifelse(y, 1/p, -1/(1-p))))%*% X
}
if(is.null(dim(x))) dim(x) <- c(length(x), 1)
dn <- dimnames(x)[[2]]
if(!length(dn)) dn <- paste("Var", 1:ncol(x), sep="")
p <- ncol(x) + intercept
if(intercept) {x <- cbind(1, x); dn <- c("(Intercept)", dn)}
if(is.factor(y)) y <- (unclass(y) != 1)
fit <- optim(start, fmin, gmin, X = x, y = y, w = wt, ...)
names(fit$par) <- dn
cat("\nCoefficients:\n"); print(fit$par)
cat("\nResidual Deviance:", format(fit$value), "\n")
cat("\nConvergence message:", fit$convergence, "\n")
invisible(fit)
}


(probit.fit<-probitreg(x=snoring, y=y, start=c(-3.87,.40), hessian=T))



# ASEs
sqrt(diag(solve(logit.fit$hessian)))
[1] 0.11753651 0.03536594

sqrt(diag(solve(lpm.fit$hessian)))
[1] 0.002425872 0.001977552

sqrt(diag(solve(probit.fit$hessian)))
[1] 0.04981663 0.01670823

# fitted probabilities

eta<-cbind(1,snoring)%*%logit.fit$par
logit.probs<-(exp(eta)/(1+exp(eta)))

eta<-cbind(1,snoring)%*%lpm.fit$par
lpm.probs<-(eta)

eta<-cbind(1,snoring)%*%logit.fit$par
probit.probs<-(pnorm(eta))

res<-cbind(logit=unique(logit.probs), lpm=unique(lpm.probs), probit=unique(probit.probs))
dimnames(res)<-list(unique(snoring),NULL)


# figure 4.1

snoring.plot<-unique(snoring)
plot(snoring,logit.probs,type="n",xlim=c(0,5),ylim=c(-.005,.20),xlab="Level of Snoring",
ylab="Predicted Probability", bty="L") 
lines(snoring.plot,unique(logit.probs),type="b",pch=16)
lines(snoring.plot,unique(probit.probs),type="b",pch=17)
lines(snoring.plot,unique(lpm.probs),type="l",lty=1)
legend(x=.05,y=.18,legend=c("Logistic","Probit","Linear"),
lty=c(1,1,1),pch=c(16,17,-1), bty="O", cex=.85, text.width=1, adj=-.5)

# IRLS

# see splus script


## GLMs for count data

table.4.3<-read.table("c:\\progra~1\\insightful\\splus61\\users\\CDA\\crab.ssc", col.names=c("C","S","W","Sa","Wt"))

# figure 4.3
plot.table.4.3<-aggregate(rep(1,nrow(table.4.3)), list(Sa=table.4.3$Sa, W=table.4.3$W), sum)
plot.table.4.3$Sa<-as.numeric(as.vector(plot.table.4.3$Sa))
plot.table.4.3$W<-as.numeric(as.vector(plot.table.4.3$W))
plot(y=plot.table.4.3$Sa,x=plot.table.4.3$W,xlab="Width (cm)", 
ylab="Number of Satellites", bty="L", axes=F, type="n")
axis(2, at=1:15)
axis(1)
text(y=plot.table.4.3$Sa,x=plot.table.4.3$W,labels=plot.table.4.3$x)


# figure 4.4
table.4.3$W.fac<-cut(table.4.3$W, breaks=c(0,seq(23.25, 29.25),Inf))
plot.y<-aggregate(table.4.3$Sa, by=list(W=table.4.3$W.fac), mean)$x
plot.x<-aggregate(table.4.3$W, by=list(W=table.4.3$W.fac), mean)$x
plot(x=plot.x, y=plot.y, ylab="Number of Satellites", xlab="Width (cm)",bty="L", 
axes=F, type="p", pch=16)
axis(2, at=0:5)
axis(1, at=seq(20,34,2))
library(mgcv)
res<-gam(plot.y~s(plot.x, k=5, fx=TRUE, bs="cr"), family=poisson(link=log))
lines(x=plot.x,y=res$fitted.values)

plot.gam(res, se=T)

# Poisson log linear model (IRLS)
log.fit<-glm(Sa~W, family=poisson(link=log),data=table.4.3)
summary(log.fit)

log.fit$null.deviance-log.fit$deviance
[1] 64.91309

attributes(summary(log.fit))

summary(log.fit)$coefficients

attributes(log.fit)

log.fit$fitted.values
fitted(log.fit)

# predict at given width value
predict.glm(log.fit, type="response", newdata=data.frame(W=26.3))

# Poisson glm with identity link

id.fit<-glm(Sa~W, family=poisson(link=identity),data=table.4.3, start=coef(log.fit))
summary(id.fit)


# Figure 4.5

plot(x=plot.x, y=plot.y, ylab=expression(paste("Mean number of satellites,", {mu})), xlab="Width (cm)",bty="L",axes=F, type="p", pch=16)
axis(2, at=0:5)
axis(1, at=seq(20,34,2))
ind<-order(table.4.3$W)
lines(x=table.4.3$W[ind],y=log.fit$fitted.values[ind])
lines(x=table.4.3$W[ind],y=id.fit$fitted.values[ind])
arrows(x0=23.5,y0=2.9,x1=23.5,y1=predict(log.fit,newdata=data.frame(W=23.5), type="response"),
length=.2)
text(x=23.5,y=3,"Log Link")
arrows(x0=29.75,y0=3.1,x1=29.75,y1=predict(id.fit,newdata=data.frame(W=29.75), type="response"),
length=.2)
text(x=29.75,y=2.9,"Identity Link")

# Compare AICs
library(MASS)
summary.glm(log.fit)$aic
summary.glm(id.fit)$aic
extractAIC(log.fit)[[2]]


## Overdispersion in Poisson GLIMs

# table 4.4
tapply(table.4.3$Sa, table.4.3$W.fac, function(x) c(mean=mean(x), variance=var(x)))
# by(table.4.3$Sa, table.4.3$W.fac, function(x) c(mean=mean(x), variance=var(x)))

log.fit.over<-glm(Sa~W, family=quasipoisson(link=log),data=table.4.3)
summary(log.fit.over)$dispersion
[1] 3.181786

summary(log.fit.over)
sqrt(summary(log.fit.over)$dispersion*diag(summary(log.fit)$cov.unscaled))

summary(log.fit)


null.fit<-glm(Sa~1, family=poisson(link=log),data=table.4.3)
anova.glm(null.fit, log.fit.over,test="F")

(deviance(null.fit)-deviance(log.fit))/summary.glm(log.fit.over)$dispersion
[1] 20.40146


## Negative binomial GLIMs

library(mass)
glm(Sa~W, family=negative.binomial(theta=1,link="identity"),data=table.4.3,start=coef(log.fit))

# using neg.bin
glm(Sa~W, family=neg.bin(theta=1),data=table.4.3)


# using glm.nb
nb.fit<-glm.nb(Sa ~ W, data = table.4.3, init.theta=1, link=identity, start=coef(id.fit))
summary(nb.fit)

Call:
glm.nb(formula = Sa ~ W, data = table.4.3, start = coef(log.fit), 
init.theta = 0.931699963254104, link = identity)

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-1.7897  -1.4092  -0.2559   0.4523   2.1069  

Coefficients:
Estimate Std. Error z value Pr(>|z|)    
(Intercept) -11.63298    1.08204  -10.75   <2e-16 ***
W             0.55396    0.05135   10.79   <2e-16 ***
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

(Dispersion parameter for Negative Binomial(0.9317) family taken to be 1)

Null deviance: 216.51  on 172  degrees of freedom
Residual deviance: 195.52  on 171  degrees of freedom
AIC: -747.65

Number of Fisher Scoring iterations: 1

Correlation of Coefficients:
(Intercept)
W     -0.9997


Theta:  0.932 
Std. Err.:  0.168 

2 x log-likelihood:  -747.929 

glm.convert(nb.fit)

## residuals

resid(log.fit, type="deviance")
pear.res<-resid(log.fit, type="pearson")
pear.std<-resid(log.fit, type="pearson")/sqrt(1-lm.influence(log.fit)$hat)

par(mfrow=c(2,2))
par(pty="s")
plot(pear.res, xlab="observation",ylab="Pearson Residuals")
abline(h=0)
plot(pear.std, xlab="observation",ylab="Standardized Pearson Residuals")
abline(h=0)

par(mfrow=c(1,2))
par(pty="s")
plot(log.fit)


## Quasi-LH and GLIMs

# horseshoe crab data (females)
Sa<-tapply(table.4.3$Sa, table.4.3$W, sum)
mu<-tapply(predict(log.fit, type="response"), table.4.3$W, sum)
chi.squared<-sum(((Sa-mu)^2)/mu)
[1] 174.2737

chi.squared/64

# teratology data

table.4.5<-read.table("teratology.ssc", col.names=c("","group","litter.size","num.dead"))[,-1]
table.4.5$group<-as.factor(table.4.5$group)

fit1<-glm(num.dead/litter.size~group-1, weights=litter.size, data=table.4.5, family=binomial)
pred<-unique(round(predict(fit1, type="response"),3))
[1] 0.758 0.102 0.034 0.048

(SE<-sqrt(pred*(1-pred)/tapply(table.4.5$litter.size,table.4.5$group,sum)))
1          2          3         4 
0.02368473 0.02786104 0.02379655 0.0209615

table.4.5$litter.size*predict(fit1, type="response")
(chi.squared<-sum(resid(fit1, type="pearson")^2))
[1] 154.7069

SE*sqrt(chi.squared/54)
1          2          3          4 
0.04008367 0.04715159 0.04027292 0.03547493

# quasi family
glm(num.dead/litter.size~group-1, weights=litter.size, data=table.4.5, family=quasi(link=identity, variance="mu(1-mu)"), 
start=pred) # see pred defined above


## GAMs

library(modreg)
library(mgcv)

table.4.3$Sa.bin<-ifelse(table.4.3$Sa>0,1,0)
plot.table.4.3<-aggregate(table.4.3$Sa, by=list(Sa.bin=table.4.3$Sa.bin,W=table.4.3$W), length)
plot.table.4.3$Sa.bin<-as.numeric(as.vector(plot.table.4.3$Sa.bin))
plot.table.4.3$W<-as.numeric(as.vector(plot.table.4.3$W))

plot(y=table.4.3$Sa.bin,x=table.4.3$W,xlab=expression(paste("Width, ", italic(x), "(cm)")), 
ylab="Probability of presence of satellites", axes=F, type="n")
axis(2, at=c(0,1))
axis(1, at=seq(20,34,2))
text(y=plot.table.4.3$Sa.bin,x=plot.table.4.3$W,labels=plot.table.4.3$x, cex=.5)

res<-gam(Sa.bin~s(W, k=3, fx=TRUE, bs="cr"), family=binomial(link=logit), data=plot.table.4.3)
lines(x=plot.table.4.3$W,y=res$fitted.values)

prop<-aggregate(table.4.3$Sa.bin, by=list(W=table.4.3$W.fac), mean)$x
lines(plot.x, prop, type="p",pch=16)    # see above for defn of plot.x

plot.gam(res, se=T)



#### Chapter 5 ####

## logistic regression for horseshoe crab data

# table.4.3$Sa.bin<-ifelse(table.4.3$Sa>0,1,0)
crab.fit.logit<-glm(Sa.bin~W, family=binomial, data=table.4.3)

# Recall:
# table.4.3$W.fac<-cut(table.4.3$W, breaks=c(0,seq(23.25, 29.25),Inf))
# prop<-aggregate(table.4.3$Sa.bin, by=list(W=table.4.3$W.fac), mean)$x
# plot.x<-aggregate(table.4.3$W, by=list(W=table.4.3$W.fac), mean)$x

plot(y=table.4.3$Sa.bin,x=table.4.3$W,xlab=expression(paste("Width, ", italic(x), "(cm)")), 
ylab=expression(paste("Proportion having satellites,", {pi}, "(x)")), axes=F, type="n")
axis(2, at=seq(0,1,.2))
axis(1, at=seq(20,34,2))

lines(y=prop, x=plot.x, pch=16, type="p")
ind<-order(table.4.3$W)
lines(x=table.4.3$W[ind],y=predict(crab.fit.logit, type="response")[ind], type="l", lty=3)

# Table 5.2

# Table with successes and failures per width (66 different widths)
cont.table<-xtabs(~W+Sa.bin, data=table.4.3)

# R Bertolusso:
# cont.table <- tapply(table.4.3$Sa.bin,table.4.3[,c("W","Sa.bin")], length)
# cont.table <- ifelse(is.na(cont.table), 0, cont.table)

# Extract widths from row.names
w.unique <-as.numeric(attr(cont.table,"dimnames")$W)

# R Bertolusso:
#w.unique <- as.numeric(row.names(cont.table))

# matrix of successes and failures
matrix.succ.fail<-structure(.Data=cont.table,dim=c(66,2))[,2:1]

# R Bertolusso:
# matrix.succ.fail <- cbind(cont.table[,"1"], cont.table[,"0"])

# First 2 columns of table 5.2
w.cut <- cut(w.unique, breaks=c(0,seq(23.25, 29.25),Inf), left.include=T)
observed<-apply(matrix.succ.fail,2,aggregate,by=list(W=w.cut),sum)
(observed <- matrix(c(observed[[1]][,ncol(observed.yes)],observed[[2]][,ncol(observed.no)]), ncol = 2))

# R Bertolusso:
#observed.yes <- aggregate(matrix.succ.fail[,1], by=list(W=w.cut), sum)
#observed.no <- aggregate(matrix.succ.fail[,2], by=list(W=w.cut), sum)
#(observed <- matrix(c(observed.yes[,ncol(observed.yes)],observed.no[,ncol(observed.no)]), ncol = 2))

# Last 2 columns
# Each fitted probability (and 1 - fitted probability)
# is multiplied by the total of observations for that width
# and totaled by group.
fit.1ogit <- glm(matrix.succ.fail~w.unique, family=binomial)
fitted.yes <- aggregate(predict(fit.1ogit, type="response") * apply(matrix.succ.fail,1,sum), by=list(W=w.cut), sum)
fitted.no <- aggregate((1-predict(fit.1ogit, type="response")) * apply(matrix.succ.fail,1,sum), by=list(W=w.cut), sum)
fitted.all <- matrix(c(fitted.yes$x,fitted.no$x), ncol = 2)

#Chi-square test
(x.squared = sum((observed-fitted.all)^2/fitted.all))

# df = #binomial samples - #coefficients
df <- length(observed[,1]) - length(fit.logit$coefficients)

1-pchisq(x.squared, df)

# G2

# R. Bertolusso (take medians of categories)
W.fac<-cut(table.4.3$W, breaks=c(0,seq(23.25, 29.25),Inf),left.include=T)
glm(observed~aggregate(table.4.3$W, by=list(W=W.fac), median)$x, family=binomial)$deviance

# Agresti (midpoint of category spread)
glm(observed~seq(22.75,29.75), family=binomial)$deviance


# inference
summary(crab.fit.logit, correlation=F)

# profile likelihood CIs

library(Bhat)

# neg. log-likelihood of logistic model with width included
nlogf <- function (p) { 
alpha<-p[1]; beta<-p[2]
y<-table.4.3$Sa.bin
lp<-alpha+beta*table.4.3$W

-sum(y*lp - y*log(1+exp(lp)) - (1-y)*log(1+exp(lp)))

}


x <- list(label=c("alpha", "beta"),est=c(-12.3508154, 0.4972305),low=c(-100,-100),upp=c(100,100))

# CI on beta
plkhci(x,nlogf,"beta")

...snip
CONVERGENCE:  4  iterations 

chisquare value is:  3.823855 
confidence bound of  beta  is  0.3087864 
log derivatives:     5.526653 
label estimate log deriv log curv
1 alpha -7.47862 5.52665   90404.4 
2 beta  0.308786 1217.3    61821100

[1] 0.3087864 0.7090134

# CI on alpha
plkhci(x,nlogf,"alpha")

...snip
CONVERGENCE:  4  iterations 

chisquare value is:  3.828054 
confidence bound of  alpha  is  -17.79965 
log derivatives:     -24.78501 
label estimate log deriv log curv
1 alpha -17.7997 30.2999   68230.4 
2 beta  0.708216 -24.785   48167900

[1] -17.799654  -7.467987


library(mass)
confint(crab.fit.logit)


# plot of estimate of p(x) and pointwise confidence interval
crab.predict<-predict(crab.fit.logit, type="response", se=T)
#crab.predict<-predict(crab.fit.logit, type="response", se=T, newdata=data.frame(W=seq(20,32,1)))
ind<-order(table.4.3$W)
plot(table.4.3$W[ind],crab.predict$fit[ind], axes=F, type="l", xlim=c(20,33),ylab="Probability of satellite", 
xlab=expression(paste("Width, ", italic(x), "(cm)")))
axis(2, at=seq(0,1,.2))
axis(1, at=seq(20,32,2))
lines(table.4.3$W[ind],crab.predict$fit[ind]-1.96*crab.predict$se[ind],lty=3)
lines(table.4.3$W[ind],crab.predict$fit[ind]+1.96*crab.predict$se[ind],lty=3)

## Hosmer-Lemeshow GOF test
table.4.3$prob.group<-cut(crab.predict$fit,breaks=quantile(x=crab.predict$fit,seq(0,1,.1)),include.lowest=T )
#table.4.3$prob.group<-cut(crab.predict$fit,breaks=10)
#table.4.3$prob.group<-cut(order(crab.predict$fit), breaks=seq(0,173,17.3), include.lowest=T)
table.4.3$predict<-crab.predict$fit

Hosmer.GOF<-sum(unlist(by(table.4.3, table.4.3$prob.group, function(x){
p<-sum(x$predict)
((sum(x$Sa.bin)-p)^2)/(p*(1-p/nrow(x)))
})))

pchisq(Hosmer.GOF,8,lower.tail=F)


## Logit models for categorical data

Alcohol<-factor(c("0","<1","1-2","3-5",">=6"), levels=c("0","<1","1-2","3-5",">=6"))
malformed<-c(48,38,5,1,1)
n<-c(17066,14464,788,126,37)+malformed

options(contrasts=c("contr.treatment", "contr.poly"))
Table.5.3.logit<-glm(malformed/n~Alcohol,family=binomial(link=logit), weights=n) # saturated model

revAlcohol<-factor(c("0","<1","1-2","3-5",">=6"), levels=rev(c("0","<1","1-2","3-5",">=6")))
Table.5.3.logit2<-glm(malformed/n~revAlcohol,family=binomial(link=logit), weights=n) # saturated model

cbind(logit=predict(Table.5.3.logit), fitted.prop=predict(Table.5.3.logit,type="response"))

cbind(logit=predict(Table.5.3.logit2), fitted.prop=predict(Table.5.3.logit2,type="response"))

(Table.5.3.logit3<-glm(malformed/n~1,family=binomial(link=logit), weights=n)) # independence model

# LR statistic
summary(Table.5.3.logit3)$deviance
[1] 6.201998

# Pearson chi-squared statistic
sum(residuals(Table.5.3.logit3, type="pearson")^2)
[1] 12.08205

# Linear logit model

scores<-c(0,.5,1.5,4,7)
Table.5.3.LL<-glm(malformed/n~scores,family=binomial,weights=n)
summary(Table.5.3.LL)

# chi-squared statistic
sum(residuals(Table.5.3.LL, type="pearson")^2)
[1] 2.050051

# lrt
Table.5.3.LL$null.deviance-Table.5.3.LL$deviance

library(MASS)
confint(Table.5.3.LL)

# estimated probabilities 
cbind(logit=predict(Table.5.3.LL), fitted.prop=predict(Table.5.3.LL,type="response"))

# Cochran-Armitage Trend test
x<-c(rep(scores,malformed),rep(scores,n-malformed))
y<-c(rep(1,sum(malformed)),rep(0,sum(n-malformed)))
(z2<-32574*cor(x,y)^2)

1-pchisq(z2, df=1)

# Using an ordered factor:
AlcoholO<-as.ordered(Alcohol)
res<-glm(malformed/n~AlcoholO,family=binomial,weights=n)

summary(res)$coefficients
Estimate Std. Error     z value     Pr(>|z|)
(Intercept) -5.0645460  0.3019524 -16.7726609 3.867930e-63
AlcoholO.L   0.2409534  0.7317476   0.3292848 7.419404e-01
AlcoholO.Q  -0.3041329  0.6232469  -0.4879814 6.255630e-01
AlcoholO.C   1.2661255  0.7705780   1.6430854 1.003653e-01
AlcoholO^4  -1.3549183  0.5530135  -2.4500638 1.428309e-02

# the quartic effect might be spurious


## Multiple logit model

# AIDS data
table.5.5<-expand.grid(AZT=factor(c("Yes","No"),levels=c("No","Yes")), Race=factor(c("White","Black"),levels=c("Black","White")))
table.5.5<-data.frame(table.5.5,Yes=c(14,32,11,12), No=c(93,81,52,43))

# Fit logit model
# set contrasts to treatment

options(contrasts=c("contr.treatment","contr.poly"))
fit<-glm(cbind(Yes,No) ~ AZT + Race , family=binomial, data=table.5.5) 

summary(fit)

confint(fit)

# lrt of conditional independence of race and AIDS given AZT
fit2<-update(object=fit, formula=~ . -Race) 

anova(fit2, fit, test="Chisq")

res<-predict(fit, type="response", se=T)

pointwise.normal<-function(results.predict, coverage = 0.99)
{
fit <- results.predict$fit
limits <- qnorm(1. - (1. - coverage)/2.) * results.predict$se.fit
list(upper = fit + limits, fit = fit, lower = fit - limits)
}

AIDS.bars<-pointwise.normal(res, coverage=.95)

error.bar<-function(x, y = NULL, lower, upper, incr = T, bar.ends = T, gap = T, add = F, horizontal = F, ..., xlab = deparse(substitute(
x)), xlim, ylim)
{
draw.null.warn <- function(draw, gap)
{
if(!any(draw)) {
warning("Not enough room for a gap.")
draw <- !draw
gap <- 0
}
invisible(list(draw = draw, gap = gap))
}
if(missing(x))
stop("no data for x or y")
if(missing(y)) {
if(missing(xlab))
xlab <- "Index"
y <- x
x <- time(x)
}
n <- length(x)
if(length(y) != n)
stop("length of y must equal the length of x")
center <- if(horizontal) x else y
if(missing(lower))
stop("you must provide lower")
if(length(lower) > 1 && length(lower) != n)
stop("length of lower must be 1 or equal to the length of x")
#if incr=T lower is assumed >=0
if(incr) lower <- center - abs(lower) else lower <- rep(lower, length = n)
if(any(lower >= center))
warning(paste("There are values of 'lower' which are greater or equal to ", if(horizontal) "x" else "y"))
if(missing(upper))
upper <- 2 * center - lower
else {
if(length(upper) > 1 && length(upper) != n)
stop("length of upper must be 1 or equal to the length of x")
if(incr)
upper <- center + upper
else upper <- rep(upper, length = n)
}
if(any(upper <= center))
warning(paste("There are values of 'upper' which are smaller or\nequal to ", if(horizontal) "x" else "y"))
if(!add)
if(horizontal) {
if(missing(ylim))
plot(x, y, xlim = if(missing(xlim)) range(c(lower, upper), na.rm = T) else xlim, xlab = xlab,
...)
else plot(x, y, xlim = if(missing(xlim)) range(c(lower, upper), na.rm = T) else xlim, ylim = ylim,
xlab = xlab, ...)
}
else {
if(missing(xlim))
plot(x, y, ylim = if(missing(ylim)) range(c(lower, upper), na.rm = T) else ylim, xlab = xlab,
...)
else plot(x, y, ylim = if(missing(ylim)) range(c(lower, upper), na.rm = T) else ylim, xlim = xlim,
xlab = xlab, ...)
}
if(horizontal) {
if(gap)
gap <- 0.75 * par("cxy")[1]
draw <- x - lower > gap
z <- draw.null.warn(draw, gap)
draw <- z$draw
gap <- z$gap
segments(lower[draw], y[draw], x[draw] - gap, y[draw])
draw <- upper - x > gap
z <- draw.null.warn(draw, gap)
draw <- z$draw
gap <- z$gap
segments(x[draw] + gap, y[draw], upper[draw], y[draw])
if(bar.ends) {
size.bar <- par("cxy")[2]
segments(lower, y - size.bar, lower, y + size.bar)
segments(upper, y - size.bar, upper, y + size.bar)
}
}
else {
if(gap)
gap <- 0.75 * par("cxy")[2]
draw <- upper - y > gap
z <- draw.null.warn(draw, gap)
draw <- z$draw
gap <- z$gap
segments(x[draw], y[draw] + gap, x[draw], upper[draw])
draw <- y - lower > gap
z <- draw.null.warn(draw, gap)
draw <- z$draw
gap <- z$gap
segments(x[draw], y[draw] - gap, x[draw], lower[draw])
if(bar.ends) {
size.bar <- par("cxy")[1]
segments(x - size.bar, upper, x + size.bar, upper)
segments(x - size.bar, lower, x + size.bar, lower)
}
}
}


error.bar(c(2,2,1,1), y=AIDS.bars$fit, AIDS.bars$lower, AIDS.bars$upper,incr=F, gap=F,
xlim=c(0,3),ylim=c(0,.35),axes=F,xlab="Race",ylab="Probability of AIDS (95% CI)",pch=".")
axis(1,at=c(2,1),labels=c("White","Black"))
axis(2,at=c(0,.1,.2,.3))
lines(c(1,2),AIDS.bars$fit[c(3,1)])
lines(c(1,2),AIDS.bars$fit[c(4,2)])
attach(table.5.5)
propAIDS<-Yes/(Yes+No)
points(c(2,2,1,1),propAIDS,pch=16)
detach(2)


# GOF
fit$deviance
sum(residuals(fit, type="pearson")^2)


# lrt of conditional independence of race and AIDS given AZT
fit2<-update(object=fit, formula=~ . -Race) 

(fit2$deviance-fit$deviance)
[1] 0.03708355

## Multiple logistic regression

options(contrasts=c("contr.treatment", "contr.poly"))
table.4.3$C.fac<-factor(table.4.3$C, levels=c("5","4","3","2"), labels=c("dark","med-dark","med","med-light"))

crab.fit.logist<-glm(Sa.bin~C.fac+W, family=binomial,data=table.4.3)
summary(crab.fit.logist,cor=F)

# profile LH CI
library(mass)
confint(crab.fit.logist)

# figure 5.5
res1<-predict(crab.fit.logist, type="response", newdata=data.frame(W=seq(18,34,1),C.fac="med-light"))
res2<-predict(crab.fit.logist, type="response", newdata=data.frame(W=seq(18,34,1),C.fac="med"))
res3<-predict(crab.fit.logist, type="response", newdata=data.frame(W=seq(18,34,1),C.fac="med-dark"))
res4<-predict(crab.fit.logist, type="response", newdata=data.frame(W=seq(18,34,1),C.fac="dark"))

plot(seq(18,34,1),res1,type="l",bty="L",ylab="Predicted Probability", axes=F,
xlab=expression(paste("Width, ", italic(x), "(cm)")))
axis(2, at=seq(0,1,.2))
axis(1, at=seq(18,34,2))
lines(seq(18,34,1),res2)
lines(seq(18,34,1),res3)
lines(seq(18,34,1),res4)
arrows(x0=29, res1[25-17],x1=25, y1=res1[25-17], length=.09)
text(x=29.1, y=res1[25-17], "Color 1", adj=c(0,0))
arrows(x0=23, res2[26-17],x1=26, y1=res2[26-17], length=.09)
text(x=21.1, y=res2[26-17], "Color 2", adj=c(0,0))
arrows(x0=28.9, res3[24-17],x1=24, y1=res3[24-17], length=.09)
text(x=29, y=res3[24-17], "Color 3", adj=c(0,0))
arrows(x0=25.9, res4[23-17],x1=23, y1=res4[23-17], length=.09)
text(x=26, y=res4[23-17], "Color 4", adj=c(0,0))


# lrt of conditional independence of AZT and AIDS given race
crab.fit.logist.ia<-update(object=crab.fit.logist, formula=~ . + W:d1 + W:d2 + W:d3) 
anova(crab.fit.logist, crab.fit.logist.ia, test="Chisq")

# quantitative ordinal
crab.fit.logist.ord<-glm(Sa.bin~C+W, family=binomial, data=table.4.3)
summary(crab.fit.logist.ord, cor=F)

## Example problem 5.17
duration<-c(45,15,40,83,90,25,35,65,95,35,75,45,50,75,30,25,20,60,70,30,60,61,65,15,20,45,15,25,15,30,40,15,135,20,40)
type<-c(0,0,0,1,1,1,rep(0,5),1,1,1,0,0,1,1,1,rep(0,4),1,1,0,1,0,1,0,0,rep(1,4))
sore<-c(0,0,rep(1,10),0,1,0,1,0,rep(1,4),0,1,0,0,1,0,1,0,1,1,0,1,0,0)

sore.fr<-cbind(duration, type, sore)
options(contrasts=c("contr.treatment","contr.poly"))

sorethroat.lg<-glm(sore ~ type*duration, family=binomial)
summary(sorethroat.lg, cor=T)

sorethroat.lg<-glm(sore ~ type*scale(duration), family=binomial)
summary(sorethroat.lg, cor=T)


# LRT of interaction
sorethroat.lg2<-glm(sore ~ type + scale(duration), family=binomial)
anova(sorethroat.lg2, sorethroat.lg, test="Chisq")

# plot
plot(c(15,135),c(0,1), type="n", xlab="duration",ylab="prob")
text(duration,sore,as.character(ifelse(type,"T","L")))
lines(15:135,predict.glm(sorethroat.lg,data.frame(duration=15:135,type=1),type="response"))
lines(15:135,predict.glm(sorethroat.lg,data.frame(duration=15:135,type=0),type="response"), lty=2)
legend(x=100, y=.6, legend=list("Tracheal","Laryngeal"), lty=1:2)

# test for type difference at 1 hour
options(contrasts=c("contr.treatment","contr.poly"))
sorethroat.lgA<-glm(sore ~ type*I(scale(duration)-.502), family=binomial)
summary(sorethroat.lgA)

# test for parallel lines across type
type.fac<-factor(ifelse(type,"trach","laryn"),levels=c("trach","laryn"))
sorethroat.lgB<-update(sorethroat.lg, . ~ type.fac + scale(duration) -1)
summary(sorethroat.lgB)$coefficients

library(mass)
dose.p(sorethroat.lgB,c(1,3),(1:3)/4)
dose.p(sorethroat.lgB,c(2,3),(1:3)/4)

par(mfrow=c(2,2))
plot(sorethroat.lgB,ask=T)


##### Chapter 6 ####

options(contrasts=c("contr.treatment", "contr.poly"))
table.4.3$C.fac<-factor(table.4.3$C, levels=c("5","4","3","2"))
table.4.3$S.fac<-factor(table.4.3$S, levels=c("3","2","1"))
crab.fit.logist.full<-glm(Sa.bin~C.fac+S.fac+W+I(Wt/1000), family=binomial,data=table.4.3)

crab.fit.logist.stuffed<-glm(Sa.bin~C.fac*S.fac*W, family=binomial,data=table.4.3)
res<-step(crab.fit.logist.stuffed, list(lower = ~1, upper =  formula(crab.fit.logist.stuffed) ), scale=1, trace=T, direction="backward") 
res$anova

library(mass)
res<-stepAIC(crab.fit.logist.stuffed, list(lower = ~1, upper =  formula(crab.fit.logist.stuffed) ), scale=1, trace=T, direction="backward") 
res$anova


## Logistic regression diagnostics

# Residuals

# Table 6.5
BP<-factor(c("<117","117-126","127-136","137-146","147-156","157-166","167-186",">186"))
CHD<-c(3,17,12,16,12,8,16,8)
n<-c(156,252,284,271,139,85,99,43)

resCHD<-glm(CHD/n~1,family=binomial, weights=n) 
resCHD$deviance
[1] 30.02257

pred.indep<-n*predict(resCHD, type="response")

dev.indep<-resid(resCHD, type="deviance")
pear.indep<-resid(resCHD, type="pearson")
pear.std.indep<-resid(resCHD, type="pearson")/sqrt(1-lm.influence(resCHD)$hat)

structure(cbind(pred.indep, dev.indep, pear.indep, pear.std.indep), 
dimnames=list(as.character(BP),c("fitted","deviance resid","pearson resid", "pearson std resid")))

par(mfrow=c(2,2))
plot(pear.std.indep, xlab="",ylab="Standardized Pearson Residuals", pch=16, axes=F)
axis(1,at=1:8, labels=as.character(BP), srt=90)
axis(2)
abline(h=0)

out<-pear.std.indep>3
par(mfrow=c(2,2))
plot(pear.std.indep, xlab="",ylab="Standardized Pearson Residuals", axes=F, type="n")
text((1:8)[out], pear.std.indep[out], ">3")
points((1:8)[!out], pear.std.indep[!out], pch=16)
axis(1,at=1:8, labels=as.character(BP), srt=90)
axis(2)
abline(h=0)

# Linear logit model
scores<-c(seq(from=111.5,to=161.5,by=10),176.5,191.5)
resLL<-glm(CHD/n~scores,family=binomial,weights=n)
resLL$deviance
[1] 5.909158

pred.indep<-n*predict(resLL, type="response")

dev.indep<-resid(resLL, type="deviance")
pear.indep<-resid(resLL, type="pearson")
pear.std.indep<-resid(resLL, type="pearson")/sqrt(1-lm.influence(resLL)$hat)

structure(cbind(pred.indep, dev.indep, pear.indep, pear.std.indep), dimnames=list(as.character(scores),c("fitted","deviance resid","pearson resid", "pearson std resid")))

# figure 6.2
win.graph(width=10, height=8)
plot(scores,CHD/n,pch="X",yaxt="n",xaxt="n",ylim=c(0,.2),xlab="Blood Pressure Level",ylab="Proportion",bty="L")
axis(side=1,at=seq(from=110,to=200,by=10))
axis(side=2,at=seq(from=0,to=.2,by=.05))
lines(scores,predict(resLL, type="response"),type="l")

# Table 6.7
yes<-c(32,21,6,3,12,34,3,4,52,5,8,6,35,30,9,11,6,15,17,4,9,21,26,25,21,7,25,31,3,9,10,25,25,39,2,4,3,0,29,6,16,7,23,36,4,10)
no<-c(81,41,0,8,43,110,1,0,149,10,7,12,100,112,1,11,3,6,0,1,9,19,7,16,10,8,18,37,0,6,11,53,34,49,123,41,3,2,13,3,33,17,9,14,62,54)
table.6.7<-cbind(expand.grid(gender=c("female","male"),dept=c("anth","astr","chem","clas","comm","comp","engl","geog","geol","germ","hist","lati","ling","math","phil","phys","poli","psyc","reli","roma","soci","stat","zool")),prop=yes/(yes+no))

res.gradadmit<-glm(prop~dept, family=binomial, data=table.6.7, weights=yes+no)
res.gradadmit$deviance
sum(resid(res.gradadmit, type="pearson")^2)

resid(res.gradadmit, type="pearson")/sqrt(1-lm.influence(res.gradadmit)$hat)


## Influence diagnostics

dfbetas(resLL)[,2,drop=F]
which.influence(resLL)

## Predictive power
library(Hmisc)
library(design)
#BP<-factor(c("<117","117-126","127-136","137-146","147-156","157-166","167-186",">186"))
#scores<-c(seq(from=111.5,to=161.5,by=10),176.5,191.5)
#CHD<-c(3,17,12,16,12,8,16,8)
#n<-c(156,252,284,271,139,85,99,43)

res<-numeric(2*length(CHD))
res[rep(c(T,F),length(CHD))]<-CHD
res[rep(c(F,T),length(CHD))]<-n-CHD
table.6.5<-data.frame(CHD=rep(rep(c(1,0),length(CHD)),res), BP=rep(BP,n), scores=rep(scores,n))

res.lrm<-lrm(CHD~scores,data=table.6.5, x=T, y=T)

(1-exp((resLL$deviance-resLL$null.deviance)/8))/(1-exp(-resLL$null.deviance/8))

junk<-glm(formula = CHD ~ scores, family = binomial, data=table.6.5)
(1-exp((junk$deviance-junk$null.deviance)/1329))/(1-exp(-junk$null.deviance/1329))

validate.lrm(res.lrm, method="boot", B=100)

detach("Design")
detach("Hmisc")

## Conditional associations

table.6.9<-data.frame(scan(file="c:/program files/insightful/splus61/users/CDA/clinical trials table 69.ssc", what=list(Center="",Treatment="",Response="",Freq=0)))
levels(table.6.9$Treatment)<-c("Drug","Control")
levels(table.6.9$Response)<-c("Success","Failure")
table.6.9.array<-xtabs(Freq~Treatment+Response+Center, data=table.6.9)

oddsratio.L<-
function (x, stratum = NULL, Log = TRUE, conf.level = 0.95, correct=T) 
{
l <- length(dim(x))
if (l > 2 && is.null(stratum)) 
stratum <- 3:l
if (l - length(stratum) > 2) 
stop("All but 2 dimensions must be specified as strata.")
if (l == 2 && dim(x) != c(2, 2)) 
stop("Not a 2 x 2 - table.")
if (!is.null(stratum) && dim(x)[-stratum] != c(2, 2)) 
stop("Need strata of 2 x 2 - tables.")
lor <- function(y, correct, Log) {
if(correct) y<-y + 0.5
or <- y[1, 1] * y[2, 2]/y[1, 2]/y[2, 1]
if (Log) 
log(or)
else or
}
ase <- function(y,correct) sqrt(sum(1/(ifelse(correct,y + 0.5,y))))
if (is.null(stratum)) {
LOR <- lor(x, correct)
ASE <- ase(x)
}
else {
LOR <- apply(x, stratum, lor, correct=correct, Log=Log)
ASE <- apply(x, stratum, ase, correct=correct)
}
I <- ASE * qnorm((1 + conf.level)/2)
Z <- LOR/ASE
structure(LOR, ASE = if (Log) 
ASE, lwr = if (Log) 
LOR - I
else exp(log(LOR) - I), upr = if (Log) 
LOR + I
else exp(log(LOR) + I), Z = if (Log) 
Z, P = if (Log) 
1 - pnorm(abs(Z)), log = Log, class = "oddsratio")
}

oddsratio.L(table.6.9.array, correct=F, Log=F)

# CMH
mantelhaen.test(table.6.9.array, correct=F)


# logit model test
n<-aggregate(table.6.9$Freq, list(table.6.9$Treatment,table.6.9$Center), FUN=sum)$x
table.6.9$n<-rep(n,rep(2,16))
res<-glm(Freq/n~Center+Treatment, family=binomial, data=table.6.9,  weights=n, subset=Response=="Success")
summary(res)
anova(res)

mantelhaen.test(table.6.9.array, exact=T)


# heterogeneity of conditional ORs

res2<-update(res, .~.-Treatment)
res3<-update(res, .~.+Center:Treatment)

anova(res2,res3,test="Chisq")



# test homogeneity of odds ratios
library(vcd)
woolf.test(table.6.9.array)

woolf <- function(x) {
x <- x + 1 / 2
k <- dim(x)[3]
or <- apply(x, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
w <-  apply(x, 3, function(x) 1 / sum(1 / x))
1 - pchisq(sum(w * (log(or) - weighted.mean(log(or), w)) ^ 2), k - 1)
}

## power and sample size

library(Hmisc)
bpower(p1=.5, p2=.6, n=50)

chipower.f<-function(x=NULL, pm=NULL, n=NULL, alpha, df, pearson=T, fit=NULL)
{
if(pearson) nc<-chisq.test(x=x,p=pm)$statistic
else nc<-n*fit$deviance
1-pchisq(qchisq(1-alpha,df),df=df,ncp=nc)
}

table.6.13<-data.frame(expand.grid(New = c("very worry", "somewhat", "reassuring"),Standard=c("worry", "reassure")), 
prop=10000*c(.04*.4, .08*.32,.04*.27,.02*.3,.18*.22,.64*.15))

weight<-10000*c(0.04,0.08,0.04,0.02,0.18,0.64)

fit1<-glm(prop/weight~Standard, data=table.6.13, family="binomial",weights=weight)
fit2<-glm(prop/weight~Standard+New, data=table.6.13, family="binomial",weights=weight)

res<-list(deviance=anova(fit1,fit2)$Deviance[2]/10000)

# LR
chipower.f(fit=res, alpha=.05, df=2, pearson=F, n=c(400,600,1000))


# example 2

table.2.5<-data.frame(expand.grid(Smoker=c("yes","no"), Lung.Cancer=c("Cases","Controls")),count=c(688,21,650,59))

table.2.5.array<-xtabs(count~Smoker+Lung.Cancer,data=table.2.5)
fit<-expected(table.2.5.array)

chipower.f(x=table.2.5.array, pm=fit, alpha=.05,df=1)


## Probit and complementary log-log

# Table 6.14 
table.6.14<-data.frame(log.dose=c(1.691,1.724,1.755,1.784,1.811,1.837,1.861,1.884),
n=c(59,60,62,56,63,59,62,60),
y=c(6,13,18,28,52,53,61,60))

# probit
options(contrasts=c("contr.treatment", "contr.poly"))
res.probit<-glm(y/n~log.dose,weights=n,family=binomial(link=probit), data=table.6.14)

# cloglog
(res.cloglog<-glm(y/n~log.dose,weights=n,family=binomial(link=cloglog), data=table.6.14))

data.frame(table.6.14, fitted.probit=round(n*fitted(res.probit),1), fitted.cloglog=round(n*fitted(res.cloglog),1))


plot(table.6.14$log.dose,table.6.14$y/table.6.14$n, pch=16, xlab="Log dosage", ylab="Proportion Killed", bty="L", axes=F)
axis(1, at=seq(1.7, 1.9, .05))
axis(2, at=seq(0,1,.2))
lines(table.6.14$log.dose, fitted(res.probit), lty=2)
lines(table.6.14$log.dose, fitted(res.cloglog), lty=1)
legend(x=1.8,y=.4,legend=c("Complementary log-log", "Probit"),lty=c(1,2), cex=.85, text.width=1)


## conditional logistic regression

Alcohol<-factor(c("0","<1","1-2","3-5",">=6"))
malformed<-c(48,38,5,1,1)
n<-c(17066,14464,788,126,37)+malformed

temp<-length(10)
temp[rep(c(T,F),5)]<-c(48,38,5,1,1)
temp[rep(c(F,T),5)]<-c(17066,14464,788,126,37)
table.5.3<-data.frame(Alcohol=rep(c(0,.5,1.5,4,7), n), case=rep(rep(c(1,0),5),temp))

library(survival)
#fit<-clogit(case~Alcohol, data=table.5.3, method="exact")
fit<-clogit(case~Alcohol, data=table.5.3, method="approximate")

library(cond)
(fit<-cond(glm(formula=case~Alcohol, family=binomial, data=table.5.3), offset=Alcohol))

summary(fit, test=T)


## conditional inference for 2x2xK tables

table.6.15<-data.frame(expand.grid(race=c("black","white"),promote=c("yes","no"),month=c("july","august","september")),
count=c(0,4,7,16,0,4,7,13,0,2,8,13))

mantelhaen.test(xtabs(count~month+race, data=table.6.15), exact=T)


## separation case (diarrhea)
table.6.16<-expand.grid(Ceph=c(0,1), Age=c(0,1), Stay=c(0,1))
table.6.16<-data.frame(table.6.16[c(rep(1,385),rep(5,233), rep(3,789), rep(7,1081), rep(8, 5)),], row.names=1:2493)
table.6.16$case<-c(rep(0,385), rep(1,5), rep(0,233-5), rep(1,3),rep(0,789-3), rep(1,47), rep(0,1081-47), rep(1,5))

library(survival)
#fit<-clogit(case~Ceph+Age+Stay, data=table.6.16, method="exact")
fit<-clogit(case~Ceph, data=table.6.16, method="approximate", subset=Age==1 & Stay==1)

table.6.16<-expand.grid(Ceph=factor(c(0,1)), Age=factor(c(0,1)), Stay=factor(c(0,1)), case=factor(c(0,1)))
table.6.16$count<-c(385,0,789-3,0,233-5,0,1081-47,0,0,0,3,0,5,0,47,5)
fisher.test(xtabs(count~Ceph+case,data=table.6.16,subset=(table.6.16$Age==1 & table.6.16$Stay==1)))


## bias-reduced logistic regression
library(brlr)
fit<-glm(case~Ceph+Age+Stay, family=binomial,data=table.6.16)
fit2<-brlr(case~Ceph+Age+Stay, data=table.6.16)
summary(fit)
summary(fit2)
(fit<-glm(case~Ceph, family=binomial,data=table.6.16, subset=(table.6.16$Age==1 & table.6.16$Stay==1)))
(fit2<-brlr(case~Ceph, data=table.6.16, subset=(table.6.16$Age==1 & table.6.16$Stay==1)))



#### Chapter 7 ####

# Table 7.1
food.labs<-factor(c("fish","invert","rep","bird","other"),levels=c("fish","invert","rep","bird","other"))
size.labs<-factor(c("<2.3",">2.3"),levels=c(">2.3","<2.3"))
gender.labs<-factor(c("m","f"),levels=c("m","f"))
lake.labs<-factor(c("hancock","oklawaha","trafford","george"),levels=c("george","hancock","oklawaha","trafford"))

table.7.1<-expand.grid(food=food.labs,size=size.labs,gender=gender.labs,lake=lake.labs)
temp<-c(7,1,0,0,5,4,0,0,1,2,16,3,2,2,3,3,0,1,2,3,2,2,0,0,1,13,7,6,0,0,3,9,1,0,2,0,1,0,1,0,3,7,1,0,1,8,6,6,3,5,2,4,1,1,4,0,1,0,0,0,13,10,0,2,2,9,0,0,1,2,3,9,1,0,1,8,1,0,0,1)
table.7.1<-structure(.Data=table.7.1[rep(1:nrow(table.7.1),temp),], row.names=1:219)

# fit models in Table 7.2
library(nnet)
options(contrasts=c("contr.treatment","contr.poly"))
fitS<-multinom(food~lake*size*gender,data=table.7.1)    # saturated model
fit0<-multinom(food~1,data=table.7.1)                       # null
fit1<-multinom(food~gender,data=table.7.1)                  # G
fit2<-multinom(food~size,data=table.7.1)                    # S
fit3<-multinom(food~lake,data=table.7.1)                    # L
fit4<-multinom(food~size+lake,data=table.7.1)           # L + S
fit5<-multinom(food~size+lake+gender,data=table.7.1)    # L + S + G

deviance(fit1)-deviance(fitS)   
deviance(fit2)-deviance(fitS)   
deviance(fit3)-deviance(fitS)
deviance(fit4)-deviance(fitS)   
deviance(fit5)-deviance(fitS)
deviance(fit0)-deviance(fitS)

# collapse over gender
options(contrasts=c("contr.treatment","contr.poly"))
fitS<-multinom(food~lake*size,data=table.7.1)   # saturated model
fit0<-multinom(food~1,data=table.7.1)               # null
fit1<-multinom(food~size,data=table.7.1)            # S
fit2<-multinom(food~lake,data=table.7.1)            # L
fit3<-multinom(food~size+lake,data=table.7.1)   # L + S

deviance(fit1)-deviance(fitS)   
deviance(fit2)-deviance(fitS)   
deviance(fit3)-deviance(fitS)
deviance(fit0)-deviance(fitS)

# fitted counts
marg.counts <- tapply(table.7.1$food, list(factor(table.7.1$size, levels = c("<2.3", ">2.3")),factor(table.7.1$lake, 
levels =c("hancock", "oklawaha", "trafford", "george"))), length)
row.names.71 <- rev(expand.grid(dimnames(marg.counts)))

fitted.counts<-round(as.vector(marg.counts)*fitted(fit3)[!duplicated(as.data.frame(fitted(fit3))),],1)
structure(.Data=as.data.frame(fitted.counts), row.names=apply(row.names.71,1,paste,collapse=" "))

library(mass)
summary(fit3, cor=F)

# estimated response probabilities
predictions<-predict(fit3, type="probs", newdata=data.frame(size=factor(">2.3",levels=c(">2.3","<2.3")), 
lake=factor("hancock",levels=c("george", "hancock", "oklawaha","trafford"))))


## using vglm

library(vgam)
fit.vglm<-vglm(food~size+lake, multinomial, data=table.7.1)
-coef(fit.vglm, matrix=T)


## using lcr (didn't work)

library(ordinal)

# transform data format
table.7.1.2<-as.data.frame(table(table.7.1))
temp<-wr(food~size+lake, data=table.7.1.2)
table.7.1.2<-data.frame(temp$design, resp=codes(temp$response)-1, freq=table.7.1.2$Freq)
attach(table.7.1.2)

y <- restovec(resp,times=F,weights=freq,type="ordinal")

tcc <- tcctomat(size.2.3,name="size")
tcc <- tcctomat(lakehancock,name="lakehancock",oldccov=tcc)
tcc <- tcctomat(lakeoklawaha,name="lakeoklawaha",oldccov=tcc)
tcc <- tcctomat(laketrafford,name="laketrafford",oldccov=tcc)

w <- rmna(y,ccov=tcc)

rm(resp,freq,race,county,trial,y,tcc)

junk<-c(apply(summary(fit3, cor=F)$coefficients,1,c))
lcr(w,dist="multinomial", transformation="logit",mu=~ size +lakehancock+ lakeoklawaha+ laketrafford, pcoef=junk)



## Proportional Odds Models

# mental impairment data
table.7.5<-read.table("c:/program files/insightful/splus61/users/cda/mental.ssc",col.names=c("mentalC", "ses", "life"))
table.7.5$mental<-ordered(table.7.5$mentalC, levels=1:4, labels=c("well","mild","moderate","impaired"))
table.7.5$ses<- -table.7.5$ses
table.7.5$life<- -table.7.5$life

# polr in MASS
library(mass)

fit.polr<-polr(mental~ses+life, data=table.7.5)
summary(fit.polr)

# lrm in design
library(hmisc)
library(design)

table.7.5$ses<- -table.7.5$ses
table.7.5$life<- -table.7.5$life
table.7.5$mental.rev<-ordered(table.7.5$mentalC, levels=4:1, labels=c("impaired","moderate","mild","well"))

(fit.lrm<-lrm(mental.rev~ses+life, data=table.7.5))

# lcr in ordinal

library(ordinal)
recode<-(table.7.5$mentalC-1)
y <- restovec(recode,times=T,weights=rep(1,40),type="ordinal") # response vector
tcc <- tcctomat(table.7.5$ses,name="ses")   # create covariate object
tcc <- tcctomat(table.7.5$life,name="life",oldccov=tcc)

w <- rmna(y,ccov=tcc)   # create a repeated object, with no repeats here

fit.lcr<-lcr(w,distribution="prop",mu=~ses+life)

# nordr in gnlm

library(gnlm)
attach(table.7.5)
nordr(w, dist="prop",mu=~ses+life,pmu=c(-.5,1,-.3), pintercept=c(1,2))
detach("table.7.5")


# figure 7.6
plot(0:9,rowSums(predict(fit.polr, newdata=data.frame(ses=rep(-1,10), life=-c(0:9)), type="probs")[,3:4]), axes=F,lty=2, type="l",ylim=c(0,1),bty="L",ylab="P(Y > 2)", xlab="Life Events Index")
axis(2)
axis(1, at=0:9)
lines(0:9,rowSums(predict(fit.polr, newdata=data.frame(ses=rep(0,10), life=-c(0:9)), type="probs")[,3:4]), lty=1)
text(2.5, .5, "SES=0")
arrows(2.5,.48,2.75,.42,length=.1)
text(3.5, .33, "SES=1")
arrows(3.5,.31,3.75,.25,length=.1)


# score test
fit<-lcr(w,mu=~ses+life)
2*(fit$likelihood-fit.lcr$likelihood)

1-pchisq(2*(fit$likelihood-fit.lcr$likelihood),4)

## cumulative link models


table.7.7<-data.frame(expand.grid(Race=c(0, 1), Sex=c(0,1), Life=ordered(c("0-20","20-40","40-50","50-60","over 65"))),
                      LifeC=rep(0:4, each=4), 
                      percent=c(2.4,3.6,1.6,2.7,3.4,7.5,1.4,2.9,3.8,8.3,2.2,4.4,17.5,25,9.9,16.3,72.9,55.6,84.9,73.7))

### unsucessful try for lcr#######################################
library(ordinal)
tcc <- tcctomat(table.7.7$Race,name="Race")   # create covariate object
tcc <- tcctomat(table.7.7$Sex,name="Sex",oldccov=tcc)
lcr(response=table.7.7$LifeC, frequencies=table.7.7$percent,dist="prop",link="cloglog",ccov=tcc)

lcr(response=table.7.7$LifeC, frequencies=table.7.7$percent,dist="prop",link="cloglog",mu=~Sex+Race, envir=table.7.7)

lcr(response=table.7.7$LifeC, frequencies=table.7.7$percent,dist="simpl",ccov=tcc,envir=table.7.7)

lcr(response=table.7.7$LifeC, frequencies=table.7.7$percent, link="cloglog",
    distribution="prop",envir=table.7.7)

lcr(response=w,  mu=~Sex+Race, dist="simpl",transform="cloglog",envir=table.7.7, iterlim=1000)

y <- restovec(table.7.7$LifeC,weights=table.7.7$percent,type="ordinal") # response vector
tcc <- tcctomat(table.7.7$Race,name="Race")   # create covariate object
tcc <- tcctomat(table.7.7$Sex,name="Sex",oldccov=tcc)
w <- rmna(y,ccov=tcc)   # create a repeated object, with no repeats here
fit.lcr<-lcr(w,distribution="prop",mu=~Sex+Race,
             link="cloglog", pcoef=c( -3.7304374,-2.7998698,-2.2182609 ,-1.1544934,.658,-.626), envir=table.7.7)
################################################################

# using vglm

library(vgam)
fit.vglm<-vglm(Life~Sex+Race,cumulative(link=cloglog, parallel=T),weights=percent,data=table.7.7)
summary(fit.vglm)
round(predict(fit.vglm, type="response")[1:4,]*100,1)


## Ajacent-categories logit models

table.7.8<-read.table("jobsat.r", col.names=c("gender","income","jobsat","freq"))
table.7.8$jobsatf<-ordered(table.7.8$jobsat, labels=c("very diss","little sat","mod sat","very sat"))

table.7.8a<-data.frame(expand.grid(income=1:4,gender=c(1,0)),unstack(table.7.8,freq~jobsatf))

library(vgam)
fit.vglm<-vglm(cbind(very.diss,little.sat,mod.sat,very.sat)~gender+income, family=acat(link="loge",parallel=T), data=table.7.8a)


## Continuation-Ratio logit models

# lrm from Design

# Add the libraries in this order:
library(Hmisc,T)
library(Design,T)

y<-ordered(c("non-live","malformed","normal"),levels=c("non-live","malformed","normal"))
x<-c(0, 62.5, 125, 250, 500)
table.7.9<-data.frame(expand.grid(y=y, x=x), freq=c(15,1,281,17,0,225,22,7,283,38,59,202,144,132,9))
table.7.9a<-structure(.Data=unstack(table.7.9, freq~y),row.names=x)

y<-rep(rep(y,5),table.7.9$freq)
x<-rep(x,tapply(table.7.9$freq, table.7.9$x, sum))
u<-cr.setup(y)
y.u<-u$y
x.u<-x[u$subs]

#j=1
fit1<-lrm(y.u[u$cohort=="all"]~x.u[u$cohort=="all"])

# For j=2
fit2<-lrm(y.u[u$cohort=="y>=malformed"]~x.u[u$cohort=="y>=malformed"])

# To fit both models together, fit an interaction term, as in the following.

fit<-lrm(y~cohort*x)

# To get selected odds ratios for the j=2 model, first issue the datadist command 
# and reissue the lrm call, as follows

x.u<-x.u[u$cohort=="y>=malformed"]
dd<-datadist(x.u)
options(datadist='dd')
fit<-lrm(y.u[u$cohort=="y>=malformed"]~x.u)

summary(fit)

summary(fit,x=c(125,250))


## using glm

# Set x and the weights for the first linear logit model
x<-c(0,62.5,125,250,500)
n1<-rowSums(table.7.9a) # use the whole table

# j=1
fit<-glm(table.7.9a[,1]/n1~x, family=binomial,link=logit,weights=n1)

# For j=2, take the second and third columns of table.9.7

n2<-rowSums(table.7.9a[,c(2,3)])
glm(table.7.9a[,2]/n2~x, family=binomial,weights=n2)

# To fit both models together, fit an interaction term, as in the following.

fit<-lrm(y.u~u$cohort*x.u)

# To get selected odds ratios for the j=2 model, first issue the datadist command 
# and reissue the lrm call, as follows

x.u<-x.u[u$cohort=="y>=malformed"]
dd<-datadist(x.u)
options(datadist='dd')
fit<-lrm(y.u[u$cohort=="y>=malformed"]~x.u)

summary(fit)
summary(fit,x=c(125,250))
summary(fit,x=c(250,500))


## using nordr (couldn't get to work)

library(gnlm)

table.7.9$yC<-codes(table.7.9$y)-1
y <- restovec(table.7.9$yC,times=F,weights=table.7.9$freq,type="ordinal") # response vector
tcc <- tcctomat(table.7.9$x,name="Concen")   # create covariate object
w <- rmna(y,ccov=tcc)   # create a repeated object, with no repeats here

nordr(w, dist="cont", mu=~Concen)
nordr(table.7.9$yC, dist="cont", mu=~Concen, envir=w,pmu=.006, pint=-3)
nordr(yC, dist="cont", mu=~x, pmu=c(-3.24,.006), weights=table.7.9$freq, envir=table.7.9, pint=1)


## using lcr
library(ordinal)
lcr(w,distribution="cont",direc="up",mu=~Concen)


## using vglm

library(vgam)
x<-c(0,62.5,125,250,500)
fit.vglm<-vglm(cbind( non.live, malformed,normal )~x,family=sratio(link ="logit",parallel = F),data=table.7.9a)
summary(fit.vglm)
fit.vglm<-vglm(cbind( non.live, malformed,normal )~x,family=cratio(link ="logit",parallel = F),data=table.7.9a)
summary(fit.vglm)


### Mean REsponse models for ordinal responses

fit.glm<-glm(jobsat~gender+income, weights=freq, family=poisson(identity), data=table.7.8)
fit.aov<-aov(jobsat ~ gender + income, data = table.7.8, weights = freq)


## MPH model

table.7.8a<-data.frame(expand.grid(income=1:4,gender=c(1,0)), unstack(table.7.8,freq~jobsatf))
table.7.8a<-structure(.Data=table.7.8a[,-(1:2)],row.names=apply(expand.grid(income=1:4,gender=c(1,0)),1,paste,collapse=" " ),names=levels(table.7.8$jobsatf) )

# population matrix
Z<-kronecker(diag(8), matrix(1,4,1)) # version 1.0
# or, Z<-pop(8, 4)

# ZF constraint matrix
ZF<-Z # version 1.0

# vector of table counts
Y<-c(t(as.matrix(table.7.8a)))

# design matrix
X<-cbind(1,unique(as.matrix(table.7.8[,1:2])))
X<-as.data.frame(X)

A<-kronecker(diag(8),matrix(1:4,1,4))

L.fct <- function(m) {
  p <- diag(c(1/(Z%*%t(Z)%*%m)))%*%m
  A<-kronecker(diag(8),matrix(1:4,1,4))
  
  A%*%p
}


# fit<-mph.fit(y=Y,Z=Z,ZF=ZF,X=X,L.fct=L.fct) # (version 1.0)
fit<-mph.fit(y=Y, X=model.matrix(~gender+income,data=X),L.fct=L.fct,L.mean=T,strata=rep(1:8,each=4),maxiter=500)
mph.summary(fit)


## Using WLS directly 

table.7.8a<-data.frame(expand.grid(income=1:4,gender=c(1,0)), unstack(table.7.8,freq~jobsatf))
table.7.8a<-structure(.Data=table.7.8a[,-(1:2)],row.names=apply(expand.grid(income=1:4,gender=c(1,0)),1,paste,collapse=" " ),names=levels(table.7.8$jobsatf) )

n<-rowSums(table.7.8a)

p<-sweep(table.7.8a,1,n,FUN="/")
p1<-p[1,]
p2<-p[2,]
p3<-p[3,]
p4<-p[4,]
p5<-p[5,]
p6<-p[6,]
p7<-p[7,]
p8<-p[8,]

rm(p)

J<-4
I<-8

#  Compute V1, V2, V3, V4

Vnames<-sapply(1:I,function(x) paste("V", x, collapse = " ", sep = ""))
pnames<-sapply(1:I,function(x) paste("p", x, collapse = " ", sep = ""))
V<-matrix(0,nc=J,nr=J)

for(i in 1:I)
{
  p<-as.numeric(eval(parse(text = pnames[i])))
  diag(V)<-p*(1-p)
  p<-as.matrix(p)
  junk<-matrix(-kronecker(p,p),nc=J,nr=J,byrow=T)
  V[lower.tri(diag(J))]<-junk[lower.tri(junk)]
  V<-t(V)
  V[lower.tri(V)]<-junk[lower.tri(junk,diag=F)]
  assign(Vnames[i], matrix(V/n[i], ncol = J, byrow = T))
}

junk<-matrix(0,J,J)
# if mph.fit.f has been sourced: V<-block.fct(V1,V2,V3,V4,V5,V6,V7,V8)
V<-rbind(
  cbind(V1,junk,junk,junk,junk,junk,junk,junk),
  cbind(junk,V2,junk,junk,junk,junk,junk,junk),
  cbind(junk,junk,V3,junk,junk,junk,junk,junk),
  cbind(junk,junk,junk,V4,junk,junk,junk,junk),
  cbind(junk,junk,junk,junk,V5,junk,junk,junk),
  cbind(junk,junk,junk,junk,junk,V6,junk,junk),
  cbind(junk,junk,junk,junk,junk,junk,V7,junk),
  cbind(junk,junk,junk,junk,junk,junk,junk,V8)
  )

js<-1:4

Q<-rbind(
  c(js,rep(0,28)),
  c(rep(0,4),js,rep(0,24)),
  c(rep(0,8),js,rep(0,20)),
  c(rep(0,12),js,rep(0,16)),
  c(rep(0,16),js,rep(0,12)),
  c(rep(0,20),js, rep(0,8)),
  c(rep(0,24),js, rep(0,4)),
  c(rep(0,28),js))

VF<-Q%*%V%*%t(Q)

# Design matrix
X<-as.matrix(cbind(rep(1,I),unique(table.7.8[,1:2])))

# Functions

Fp<-c(js%*%as.numeric(p1), js%*%as.numeric(p2), js%*%as.numeric(p3), js%*%as.numeric(p4), js%*%as.numeric(p5), 
      js%*%as.numeric(p6), js%*%as.numeric(p7), js%*%as.numeric(p8))

# Estimate beta

b<-as.numeric(solve(t(X)%*%solve(VF)%*%X)%*%t(X)%*%solve(VF)%*%Fp)
[1]  2.61328732 -0.04155966  0.18163655

#ASCOV
sqrt(diag(solve(t(X)%*%solve(VF)%*%X)))
[1] 0.2311374 0.1369473 0.0681758

# W for entire model
t(Fp-X%*%b)%*%solve(VF)%*%(Fp-X%*%b)

[,1] 
[1,] 5.242789


### Generalized CMH Tests

# general association
mantelhaen.test(xtabs(freq~jobsatf+income+gender, data=table.7.8))

# nonzero correlation
table.7.8$jobsatS<-ifelse(table.7.8$jobsat==4,5,table.7.8$jobsat)
table.7.8$jobsatS<-ifelse(table.7.8$jobsat==3,4,table.7.8$jobsatS)
table.7.8$jobsatS<-ifelse(table.7.8$jobsat==2,3,table.7.8$jobsatS)

table.7.8$incomeS<-ifelse(table.7.8$income==4,35,table.7.8$income)
table.7.8$incomeS<-ifelse(table.7.8$income==3,20,table.7.8$incomeS)
table.7.8$incomeS<-ifelse(table.7.8$income==2,10,table.7.8$incomeS)
table.7.8$incomeS<-ifelse(table.7.8$income==1,3,table.7.8$incomeS)

table.7.8.array<-xtabs(freq~jobsatf+income+gender, data=table.7.8)
Tk<-apply(table.7.8.array,3,function(x,u,v) sum(outer(u,v)*x), u=c(1,3,4,5), v=c(3,10,20,35))
ETk<-apply(table.7.8.array,3,function(x,u,v) sum(rowSums(x)*u)*sum(colSums(x)*v)/sum(x), u=c(1,3,4,5), v=c(3,10,20,35))

varTk<-apply(table.7.8.array,3,function(x,u,v) {
  n<-sum(x)
  rowsums<-rowSums(x)
  colsums<-colSums(x)
  (sum(rowsums*u^2) - (sum(rowsums*u)^2)/n)*(sum(colsums*v^2) - (sum(colsums*v)^2)/n)/(n-1)
}, u=c(1,3,4,5), v=c(3,10,20,35))

(sum(Tk-ETk)^2)/sum(varTk)


#### Chapter 8 ####

### Loglinear models

## three-way tables

# table 8.3

table.8.3<-data.frame(expand.grid(
  marijuana=factor(c("Yes","No"),levels=c("No","Yes")), cigarette=factor(c("Yes","No"),levels=c("No","Yes")), 
  alcohol=factor(c("Yes","No"),levels=c("No","Yes"))), count=c(911,538,44,456,3,43,2,279))

# IPF
library(mass)

fitACM<-loglm(count~alcohol*cigarette*marijuana,data=table.8.3,param=T,fit=T)   # ACM
fitAC.AM.CM<-update(fitACM, .~. - alcohol:cigarette:marijuana)                          # AC, AM, CM
fitAM.CM<-update(fitAC.AM.CM, .~. - alcohol:cigarette)                                  # AM, CM
fitAC.M<-update(fitAC.AM.CM, .~. - alcohol:marijuana - cigarette:marijuana)         # AC, M
fitA.C.M<-update(fitAC.M, .~. - alcohol:cigarette)                                      # A, C, M

data.frame(table.8.3[,-4], ACM=c(aperm(fitted(fitACM))), AC.AM.CM=c(aperm(fitted(fitAC.AM.CM))), 
           AM.CM=c(aperm(fitted(fitAM.CM))),AC.M=c(aperm(fitted(fitAC.M))), A.C.M=c(aperm(fitted(fitA.C.M))))


# conditional odds ratios for model AC,AM,CM
fit.array<-fitted(fitAC.AM.CM)

apply(fit.array,1,function(x) x[1,1]*x[2,2]/(x[2,1]*x[1,2]))    # CM (given level of A)
apply(fit.array,2,function(x) x[1,1]*x[2,2]/(x[2,1]*x[1,2]))    # AM
apply(fit.array,3,function(x) x[1,1]*x[2,2]/(x[2,1]*x[1,2]))    # AC


# marginal odds ratios

sum.array<-function(array, perm=c(3,2,1)){  
  res<-aperm(array,perm)
  colSums(res)
}

odds.ratio<-function(x) x[1,1]*x[2,2]/(x[2,1]*x[1,2])

odds.ratio(sum.array(fit.array))    # AC (sum over M, the faces)
odds.ratio(sum.array(fit.array, perm=1:3))  # CM (sum over A, the rows)
odds.ratio(sum.array(fit.array, perm=c(2,1,3))) # AM (sum over C, the columns)

# loglin 
fit.loglin<-loglin(fitted(fitACM), margin=list(1:2, 2:3, c(1,3)),fit=T,param=T)

# glm
options(contrasts=c("contr.treatment","contr.poly"))
fit.glm<-glm(count~.^2, data=table.8.3, family=poisson)


## GOF tests
summary(fitAC.AM.CM)
fit.loglin
summary(fit)
anova(fitAC.M,fitAC.AM.CM,fitAM.CM,fitA.C.M)

## parameter estimates

fitAC.AM.CM$param
X<-model.matrix(count~(alcohol+cigarette+marijuana)^2,data=table.8.3,contrasts=list(alcohol=as.matrix(c(1,0)), marijuana=as.matrix(c(1,0)), cigarette=as.matrix(c(1,0))))
sqrt(diag(solve(t(X)%*%diag(c(fitAC.AM.CM$fitted))%*%X)))

# glm
fit.glm2<-update(fit.glm, contrasts=list(alcohol=as.matrix(c(1,0)), marijuana=as.matrix(c(1,0)), cigarette=as.matrix(c(1,0))))
summary(fit.glm2, cor=F)


## higher dimensional tables
table.8.8<-data.frame(expand.grid(belt=c("No","Yes"), location=c("Urban","Rural"), gender=c("Female","Male"), injury=c("No","Yes")), 
                      count=c(7287,11587,3246,6134,10381,10969,6123,6693,996,759,973,757,812,380,1084,513))

library(mass)
fitG.I.L.S<-loglm(count~., data=table.8.8, fit=T, param=T)  # mutual independence
fitGI.GL.GS.IL.IS.LS<-update(fitG.I.L.S, .~.^2, data=table.8.8, fit=T, param=T) # all pairwise associations
fitGIL.GIS.GLS.ILS<-update(fitG.I.L.S, .~.^3, data=table.8.8, fit=T, param=T)   # all three-way associations

# comparison
anova(fitG.I.L.S, fitGI.GL.GS.IL.IS.LS,fitGIL.GIS.GLS.ILS)

# try another
(fitGI.IL.IS.GLS<-update(fitGI.GL.GS.IL.IS.LS, .~.+ gender:location:belt))

fitted(fitGI.IL.IS.GLS)


# conditional odds ratios for model GI.IL.IS.GLS

fit.array<-fitted(fitGI.IL.IS.GLS)

odds.ratio<-function(x) x[1,1]*x[2,2]/(x[2,1]*x[1,2])

apply(fit.array,c(1,4),odds.ratio)    # GL S (same for I = yes or no - column)
apply(fit.array,c(2,4),odds.ratio)    # GS L (same for I = yes or no - column)
apply(fit.array,c(3,4),odds.ratio)    # LS G (same for I = yes or no - column)

apply(fit.array,c(1,2),odds.ratio)    # GI (same for each combination of LS)
apply(fit.array,c(1,3),odds.ratio)    # IL (same for each combination of GS)
apply(fit.array,c(2,3),odds.ratio)    # IS (same for each combination of GL)



# dissimilarity matrix
Fitted.values<-c(fit.array)
sum(abs(table.8.8$count-Fitted.values))/(2*sum(table.8.8$count))


### Loglinear models as Logit Models
options(contrasts=c("contr.treatment","contr.poly"))

# loglinear model
fit.loglinear<-glm(count~.^2 + gender:location:belt, data=table.8.8, family=poisson)

# logit model
fit.logit<-glm(injury~gender+belt+location, data=table.8.8, family=binomial, weight=count)

fit.loglinear$coefficients
fit.logit$coefficients


### contingency table standardization
table.8.15<-data.frame(expand.grid(Attitude=factor(c("Disapprove","Middle","Approve"), levels=c("Disapprove","Middle","Approve")),
                                   Education=factor(c("<HS","HS",">HS"), levels=c("<HS","HS",">HS"))),
                       count=c(209,101,237,151,126,426,16,21,138))

fit<-glm(I(rep(100/3, 9))~Attitude+Education+offset(log(count)), family=poisson, data=table.8.15)

cbind(table.8.15,std=fitted(fit))


#### Chapter 9 ####

## model selection (student survey data)

table.9.1<-data.frame(expand.grid(cigarette=c("Yes","No"), alcohol=c("Yes","No"),marijuana=c("Yes","No"), sex=c("female","male"), 
                                  race=c("white","other")), count=c(405,13,1,1,268,218,17,117,453,28,1,1,228,201,17,133,23,2,0,0,23,19,1,12,30,1,1,0,19,18,8,17))

library(mass)
fit.GR<-glm(count~ . + sex*race,data=table.9.1,family=poisson)  # mutual independence + GR
fit.homog.assoc<-glm(count~ .^2,data=table.9.1,family=poisson)  # homogeneous association

summary(res<-stepAIC(fit.homog.assoc, scope= list(lower = ~ + cigarette + alcohol + marijuana + sex*race), direction="backward"))

fit.AC.AM.CM.AG.AR.GM.GR.MR<-res

fit.AC.AM.CM.AG.AR.GM.GR<-update(fit.AC.AM.CM.AG.AR.GM.GR.MR, ~. - marijuana:race)


## residuals (standardized pearson)

res.model6<-resid(fit.AC.AM.CM.AG.AR.GM.GR, type="pearson")/sqrt(1-lm.influence(fit.AC.AM.CM.AG.AR.GM.GR.MR)$hat)
fit.model6<-fitted(fit.AC.AM.CM.AG.AR.GM.GR)

data.frame(table.9.1, fitted=fit.model6, residuals=res.model6)

resid(fit.AC.AM.CM.AG.AR.GM.GR.MR, type="pearson")/sqrt(1-lm.influence(fit.AC.AM.CM.AG.AR.GM.GR.MR)$hat)
fitted(fit.AC.AM.CM.AG.AR.GM.GR.MR)


### Modeling ordinal associations

# table 9.3
table.9.3<-read.table("c:/program files/r/rw1070/cda/sex.txt", col.names=c("premar","birth","u","v","count"))
table.9.3$birth<-5-table.9.3$birth      # rearrange scores so that table starts at 1,1 in the upper left corner

# create uniform scores
table.9.3$u1<-table.9.3$premar 
table.9.3$v1<-table.9.3$birth 

# need factors for premar and birth.  Code levels so that 4th level is set to zero
table.9.3$premar<-factor(table.9.3$premar, levels=4:1)
table.9.3$birth<-factor(table.9.3$birth, levels=4:1)


# Fit the Uniform Association model using scores for the variables only in the interaction. Note the use of ':'
options(contrasts=c("contr.treatment","contr.poly"))
fit.ua<- glm(count ~ premar + birth + u1:v1, data=table.9.3, family=poisson)

# local association parameter
exp(coef(fit.ua)["u1:v1"])

# Test of the association parameter:
summary(fit.ua)$coef["u1:v1",]

exp(summary(fit.ua)$coef["u1:v1",1]+1.96*c(-1,1)*summary(fit.ua)$coef["u1:v1",2])

# nonlocal association parameter
exp(coef(fit.ua)["u1:v1"]*(4-1)*(4-1))


# fitted values
matrix(fitted(fit.ua),byrow=T,ncol=4,
       dimnames=list(PreMar=c("Always wrong", "Almost always wrong", "Wrong sometimes", "Not wrong"),
                     TeenBirth=c("SA","A","D","SD")))


# Independence model
fit.ind<-glm(count ~ premar + birth, data=table.9.3, family=poisson)

# Compare the two models:
anova(fit.ind,fit.ua,test="Chi")


## Row effects model (same effect for the entire row)

# Table 9.5
table.9.5<-data.frame(expand.grid(Affil=factor(c("Democrat","Independent","Republican"),levels=c("Republican","Independent","Democrat")),
                                  Ideology=factor(c("Liberal","Moderate","Conservative"), levels=c("Conservative","Moderate","Liberal"))), 
                      c.Ideo=rep(1:3,each=3), count=c(143,119,15,156,210,72,100,141,127))


# Row Effects model:
options(contrasts=c("contr.treatment","contr.poly"))
(fit.RE<-glm(count~Ideology+Affil*c.Ideo,family=poisson,data=table.9.5))


# predicted logits

res<-matrix(fit.RE$linear.predictors,byrow=F,ncol=3)
mat<-matrix(c(res[,2]-res[,1],res[,3]-res[,2]),byrow=T,ncol=3)


# fitted values
matrix(fitted(fit.RE),byrow=F,ncol=3,dimnames=list(Affiliation=c("Democrat","Independent","Republican"),Ideology=c("Liberal","Moderate","Conservative")))


# Comparing independence and Row effects model:
fit.ind<-glm(count~Affil+Ideology,family=poisson,data=table.9.5)
anova(fit.ind,fit.RE)


## Multi-way-tables

# Table 9.7

table.9.7<-data.frame(expand.grid(Status=factor(c("Never","Former","Current"),  levels=c("Current","Former","Never",)),
                                  Breath=factor(c("Normal","Borderline","Abnormal"),  levels=c("Abnormal","Borderline","Normal")), 
                                  Age=factor(c("< 40","40-50"), levels=c("40-50","< 40"))),
                      c.Status=rep(1:3, 6), c.Breath=rep(rep(1:3,each=3),2), 
                      count=c(577,192,682,27,20,46,7,3,11,164,145,245,4,15,47,0,7,27)
                      )

options(contrasts=c("contr.treatment","contr.poly"))
fit.hetero<-glm(count~Age*Breath + Status*Age + Age:c.Breath:c.Status, data=table.9.7, family=poisson)
summary(fit.hetero, cor=F)

(fit.homo<-glm(count~Age*Breath + Status*Age + c.Breath:c.Status, data=table.9.7, family=poisson))

# ordered strata (Z)

table.9.8<-data.frame(expand.grid(Age=factor(c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64"),
                                             levels=c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")),
                                  Wheeze=factor(c("Yes","No"), levels=c("Yes", "No")),
                                  Breath=factor(c("Yes","No"), levels=c("Yes", "No")) ),
                      c.Breath=rep(1:2, each=18), c.Wheeze=rep(rep(1:2,ea=9),2),
                      extra.term=c(1:9,rep(0,27)),
                      count=c(9,23,54,121,169,269,404,406,372,7,9,19,48,54,88,117,152,106,95,105,177,257,273,324,245,225,132,1841,1654,1863,2357,1778,1712,1324,967,526)
                      )

fit.ord.strata<-glm(count~Age*Breath + Wheeze*Age + c.Breath:c.Wheeze + extra.term, data=table.9.8, family=poisson)
summary(fit.ord.strata, cor=F)


# Ordinal tests of conditional independence

# exact test (using exactLoglinTest)

library(exactLoglinTest)
set.seed(1)

fit.mc<-mcexact(count~Age*(Breath + Wheeze) + c.Breath:c.Wheeze + extra.term, data=table.9.8, method="bab", nosim=10^4, savechain=T)

pchisq(fit.mc$dobs, df=7, lower=F)

# plot diagnostics
layout(matrix(c(1,1,2,3),2,2, byrow=T))
plot(fit.mc$chain[,1], type="l", xlab="iteration", ylab="deviance")
acf(fit.mc$chain[,1])
library(mass)
truehist(fit.mc$chain[,3], xlab="log importance weight")


## Association Models, Correlation models, and correspondence analysis

# RC model

table.9.9<-data.frame(expand.grid(MH=factor(c("well","mild","moderate","impaired"), levels=rev(c("well","mild","moderate","impaired"))),
                                  SES=factor(LETTERS[1:6], levels=LETTERS[6:1])), 
                      count=c(64,94,58,46,57,94,54,40,57,105,65,60,71,141,77,94,36,97,54,78,21,71,54,71))

table.9.9.matrix<-t(xtabs(count~MH + SES ,data=table.9.9))

library(vgam)
options(contrasts=c("contr.treatment", "contr.poly"))
(fit.rc<-grc(table.9.9.matrix, Rank=1, Structural.zero=1))

res<-biplot(fit.rc, plot.it=F)
res$Cmatrix
res$Amatrix
Delta<-structure(rbind(0,res$Cmatrix)%*%t(res$Amatrix), dimnames=list(c(LETTERS[6:1]),c(rev(c("well","mild","moderate","impaired")) )))

mu<-c(-1.68, -.14, .14,1.41)
nu<-c(-1.11,-1.12,-.37,.03,1.01,1.82)
mu<-mu-mu[4]
nu<-nu-nu[6]

structure(t(.17*mu%*%t(nu)), dimnames=list(c(LETTERS[1:6]),c("well","mild","moderate","impaired") ) )


# L x L model

table.9.9a<-data.frame(expand.grid(MH=factor(c("well","mild","moderate","impaired"), levels=rev(c("well","mild","moderate","impaired"))),
                                   SES=factor(LETTERS[1:6], levels=LETTERS[6:1])), c.MH=rep(4:1,6), c.SES=rep(6:1,each=4),
                       count=c(64,94,58,46,57,94,54,40,57,105,65,60,71,141,77,94,36,97,54,78,21,71,54,71))

(fit.LL<-glm(count~MH+SES + c.MH:c.SES, family=poisson,data=table.9.9))

fit.LL$deviance-fit.rc@criterion$deviance

detach("package:vgam")


# Correlation model

library(mass)

# do not reverse levels of factors
table.9.9b<-data.frame(expand.grid(MH=factor(c("well","mild","moderate","impaired"), levels=(c("well","mild","moderate","impaired"))),
                                   SES=factor(LETTERS[1:6], levels=LETTERS[1:6])), 
                       count=c(64,94,58,46,57,94,54,40,57,105,65,60,71,141,77,94,36,97,54,78,21,71,54,71))

table.9.9b.array<-xtabs(count~MH+SES,data=table.9.9b)
fit.corresp<-corresp(x=table.9.9b.array)


# Correspondence Analysis

library(multiv)
table.9.9b.array<-t(table.9.9b.array)  # transposed the default
fit.ca<-ca(table.9.9b.array, nf=3)

# plot of first and second factors
plot(fit.ca$rproj[,1], fit.ca$rproj[,2],type="n",ylim=c(-.1,.1),xlim=c(-.3,.3),
     xlab="",ylab="",axes=F)
text(fit.ca$rproj[,1], fit.ca$rproj[,2], labels=dimnames(table.9.9b.array)$SES)
text(fit.ca$cproj[,1], fit.ca$cproj[,2], labels=dimnames(table.9.9b.array)$MH)
# Place additional axes through x=0 and y=0:
my.plaxes(fit.ca$rproj[,1], fit.ca$rproj[,2],Length=.15)

my.plaxes<-
  function(a, b, Length = 0.1)
  {
    arrows(min(a), 0, max(a), 0, length = Length)
    arrows(0, min(b), 0, max(b), length = Length)
  }


library(mass)
fit.corresp<-corresp(x=design.table(table.9.9b),nf=3)

fit.corresp$cor
fit.corresp$rscore%*%diag(fit.corresp$cor)
fit.corresp$cscore%*%diag(fit.corresp$cor)

plot(fit.corresp)


library(CoCoAn)
fit.CAIV<-CAIV(table.9.9b.array)
fit.CAIV<-CAIV(t(table.9.9b.array))



### Analyzing Rates ###

table.9.11<-data.frame(expand.grid(factor(c("Aortic","Mitral"),levels=c("Aortic","Mitral")),
                                   factor(c("<55","55+"),levels=c("<55","55+"))), Deaths=c(4,1,7,9), Exposure=c(1259,2082,1417,1647))
names(table.9.11)[1:2]<-c("Valve","Age")
attach(table.9.11)
table.9.11<-data.frame(table.9.11,Risk=Deaths/Exposure)

options(contrasts=c("contr.treatment", "contr.poly"))
fit.rate<-glm(Deaths~Valve+Age+offset(log(Exposure)),family=poisson,data=table.9.11)
summary(fit.rate, cor=F)

library(mass)
exp(confint(fit.rate))

# chi-squared statistic, use resid()
sum(resid(fit.rate,type="pearson")^2)


# Table 9.12
attach(table.9.11)
mhat<-fitted(fit.rate)
exphat<-fitted(fit.rate)/Exposure
temp<-rbind(mhat,exphat)
array(temp,dim=c(2,2,2),dimnames=list(c("Deaths","Risk"),Valve=c("Aortic","Mitral"),Age=c("<55","55+")))

# identity link
fit.id<-glm(Deaths~I(codes(Valve)*Exposure)+I(rev(codes(Age))*Exposure)+Exposure-1,family=poisson(link=identity),data=table.9.11)
summary(fit.id, cor=F)


### Modelling survival times ###

# Table 9.13

# form the data set
table.9.13<-expand.grid(Stage=factor(c(1,2,3), levels=3:1),Histology=factor(c("I","II","III"), levels=rev(c("I","II","III"))),
                        Time=factor(c(0,2,4,6,8,10,12), levels=rev(c(0,2,4,6,8,10,12))))
table.9.13<-data.frame(table.9.13,Deaths=c(9,12,42,5,4,28,1,1,19,2,7,26,2,3,19,1,1,11,9,5,12,3,5,10,1,3,7,
                                           10,10,10,2,4,5,1,1,6,1,4,5,2,2,0,0,0,3,3,3,4,2,1,3,1,0,3,1,4,1,2,4,2,0,2,3),
                       Exposure=c(157,134,212,77,71,130,21,22,101,139,110,136,68,63,72,17,18,63,126,96,90,63,58,42,14,14,43,102,86,64,
                                  55,42,21,12,10,32,88,66,47,50,35,14,10,8,21,82,59,39,45,32,13,8,8,14,76,51,29,42,28,7,6,6,10) )

# loglinear analysis using glm()
options(contrasts=c("contr.treatment", "contr.poly"))
fit.surv<-glm(Deaths~Histology+Time+Stage+offset(log(Exposure)),data=table.9.13,family=poisson)
summary(fit.surv, cor=F)

# choose a model using stepAIC from the Mass library
fit2<-glm(Deaths~Time+offset(log(Exposure)),data=table.9.13,family=poisson)
library(MASS)
stepAIC(fit.surv,scope=list(lower=formula(fit2),upper=~.^2),direction="both",trace=T)$anova


## Sparse tables ##

table.9.16<-data.frame(expand.grid(treatment=c(1,0), 
                                   Center=factor(1:5,levels=5:1)),prop=c(0,0,1/13,0,0,0,6/9,2/8,5/14,2/14))

options(contrasts=c("contr.treatment", "contr.poly"))
fit.glm.sparse<-glm(prop~treatment+Center-1,data=table.9.16,weights=c(5,9,13,10,7,5,9,8,14,14),family=binomial)
summary(fit.glm.sparse)

fit.glm.sparse2<-glm(prop~Center-1,data=table.9.16,weights=c(5,9,13,10,7,5,9,8,14,14),family=binomial)
anova(fit.glm.sparse2,fit.glm.sparse)

# exact conditional test
temp<-length(20)
temp[rep(c(T,F),10)]<-c(0,0,1,0,0,0,6,2,5,2)
temp[rep(c(F,T),10)]<-c(5,9,12,10,7,5,3,6,9,12)
table.9.16a<-data.frame(table.9.16[rep(1:10,c(5,9,13,10,7,5,9,8,14,14)),1:2],case=rep(rep(c(1,0),10),temp))

library(survival)
fit.clogit.sparse<-clogit(case~treatment+strata(Center), data=table.9.16a, method="exact")

coxph(Surv(rep(1,sum(temp)),case)~treatment+strata(Center), data=table.9.16a)


#### Chapter 10 ####

## Comparing dependent proportions

table.10.1<-matrix(c(794,150,86,570),byrow=T,ncol=2)
mcnemar.test(table.10.1,correct=F)

table.10.1.prop<-prop.table(table.10.1)
prop.diff<-margin.table(table.10.1.prop,2)[1]-margin.table(table.10.1.prop,1)[1]
off.diag<-diag(table.10.1.prop[1:2,2:1])

prop.diff + c(-1,1)*qnorm(.975)*sqrt((sum(off.diag) - diff(off.diag)^2)/sum(table.10.1))


## CLR matched case-control 

table.10.3<-data.frame(pair=rep(1:144,rep(2,144)), MI=rep(c(0,1),144), 
                       diabetes=c(rep(1,2*9),rep(c(1,0),16),rep(c(0,1),37),rep(0,2*82))
                       )

library(survival)
fit.CLR<-clogit(MI~diabetes+strata(pair),method="exact", data=table.10.3)

summary(fit.CLR)


## random effects model

library(MASS)
fit.glmmPQL<-glmmPQL(MI ~ diabetes, random = ~ 1 | pair, family = binomial, data = table.10.3)
summary(fit.glmmPQL)

library(glmmML)
(fit.glmmML<-glmmML(MI ~ diabetes, cluster=table.10.3$pair, family = binomial, data = table.10.3))

library(repeated)
glmm(MI ~ diabetes, family=binomial, data=table.10.3, nest=pair)



### Marginal Models for Square Tables

## oridnal models


# marginal homogeneity model

y <- scan() 
144 33 84 126 2 4 14 29 0 2 6 25 0 0 1 5

Z <- matrix(1,16,1) 
ZF <- Z 

M1 <- Marg.fct(1,rep(4,2))  # used to get y1+, etc
M2 <- Marg.fct(2,rep(4,2))  # used to get y+1, etc


C.matrix <- scan() 
1 0 0 0 -1 0 0 0 
0 1 0 0 0 -1 0 0
0 0 1 0 0 0 -1 0 

C.matrix <- matrix(C.matrix,3,8,byrow=T) 

h.fct <- function(m) { # constraint function
  marg <- rbind(M1%*%m,  M2%*%m)   # y1+, y2+, y3+, y4+, y+1, y+2, y+3, y+4
  C.matrix%*%marg                         # y1+ = y+1, y2+ = y+2, etc
} 


a <- mph.fit(y=y,Z=Z,ZF=ZF,h.fct=h.fct) 

mph.summary(a)


# cumulative logit model

y <- scan() 
144 33 84 126 2 4 14 29 0 2 6 25 0 0 1 5

Z <- matrix(1,16,1) 
ZF <- Z 

# create A

M1 <- Marg.fct(1,rep(4,2))  
M2 <- Marg.fct(2,rep(4,2))  
M<-rbind(M1,M2)

CUM0 <- scan()
1 0 0 0
0 1 1 1
1 1 0 0
0 0 1 1
1 1 1 0
0 0 0 1

CUM0 <- matrix(CUM0,6,4,byrow=T)
CUM <- kronecker(diag(2),CUM0)

AMarg <- CUM%*%M


# create C

C0 <- scan()
1 -1 0 0 0 0
0 0 1 -1 0 0
0 0 0 0 1 -1

C0 <- matrix(C0,3,6,byrow=T)
CMarg <- kronecker(diag(2),C0)


# CLog(Am) function

L.fct <- function(m) {
  CMarg%*%log(AMarg%*%m)
}


# CREATE DESIGN MATRIX 

XMarg <- scan()
1 0 0 1
0 1 0 1
0 0 1 1
1 0 0 0
0 1 0 0
0 0 1 0

XMarg <- matrix(XMarg, 6, 4,byrow=T)


# FIT THE MODEL AND SUMMARIZE 

a <- mph.fit(y,Z,ZF,L.fct=L.fct,X=XMarg)

mph.summary(a)


### nominal models (baseline-category logit)

# ML fit

dummies<-matrix(c(  
  1,    0,    0,    0,    0,    0,    0,    0,    0,     0,     0,     0,     0,
  0,    1,    0,    0,    0,    0,    0,    0,    0,     0,     0,     0,     0,
  0,    0,    1,    0,    0,    0,    0,    0,    0,     0,     0,     0,     0,
  -1,   -1,   -1,    0,    0,    0,    0,    0,    0,     0,     1,     0,     0,
  0,    0,    0,    1,    0,    0,    0,    0,    0,     0,     0,     0,     0,
  0,    0,    0,    0,    1,    0,    0,    0,    0,     0,     0,     0,     0,
  0,    0,    0,    0,    0,    1,    0,    0,    0,     0,     0,     0,     0,
  0,    0,    0,   -1,   -1,   -1,    0,    0,    0,     0,     0,     1,     0,
  0,    0,    0,    0,    0,    0,    1,    0,    0,     0,     0,     0,     0,
  0,    0,    0,    0,    0,    0,    0,    1,    0,     0,     0,     0,     0,
  0,    0,    0,    0,    0,    0,    0,    0,    1,     0,     0,     0,     0,
  0,    0,    0,    0,    0,    0,   -1,   -1,   -1,     0,     0,     0,     1,
  -1,    0,    0,   -1,    0,    0,   -1,    0,    0,     0,     1,     0,     0,
  0,   -1,    0,    0,   -1,    0,    0,   -1,    0,     0,     0,     1,     0,
  0,    0,   -1,    0,    0,   -1,    0,    0,   -1,     0,     0,     0,     1,
  0,    0,    0,    0,    0,    0,    0,    0,    0,     1,     0,     0,     0),
                nrow=16, ncol=((4-1)^2) + 1 +3)

dummies<-data.frame(counts=c(11607,100,366,124,87,13677,515,302,172,225,17819,270,63,176,286,10192),dummies)
names(dummies)<-c("counts","m11","m12","m13","m21","m22","m23","m31","m32","m33","m44","m1","m2","m3")

fit<-glm(counts~.-1,family=poisson(identity),data=dummies)

residence80<-(c("NE","MW","S","W"))
residence85<-(c("NE","MW","S","W"))
matrix(fitted(fit),byrow=T,nc=4,dimnames=list(residence80,residence85))

rowSums(matrix(fitted(fit),byrow=T,nc=4))
colSums(matrix(fitted(fit),byrow=T,nc=4))


# Bhapkar's method (Wald test)

A<-matrix(c(0,1,1,1,-1,0,0,0,-1,0,0,0,-1,0,0,0,
            0,1,0,0,-1,0,-1,-1,0,1,0,0,0,1,0,0,
            0,0,1,0,0,0,1,0,-1,-1,0,-1,0,0,1,0),nc=16,nr=3,byrow=T)

counts<-c(11607,100,366,124,87,13677,515,302,172,225,17819,270,63,176,286,10192)
p<-counts/(n<-sum(counts))
y<-A%*%p

Sp<--outer(p,p)/n
diag(Sp)<-p*(1-p)/n

Sy<-A%*%Sp%*%t(A)

1-pchisq(as.numeric(t(y)%*%solve(Sy)%*%y),df=3)


## Symmetry models

residence80<-factor(residence80, levels=residence80)
residence85<-residence80

table.10.6<-expand.grid(res80=residence80,res85=residence85)
table.10.6$counts<-c(11607,100,366,124,87,13677,515,302,172,225,17819,270,63,176,286,10192)
table.10.6$symm<-paste(
  pmin(as.numeric(table.10.6$res80),as.numeric(table.10.6$res85)),
  pmax(as.numeric(table.10.6$res80),as.numeric(table.10.6$res85)),sep=",")
table.10.6$symm<-factor(table.10.6$symm, levels=rev(table.10.6$symm))

options(contrasts=c("contr.treatment", "contr.poly"))
(fit.symm<-glm(counts~symm,family=poisson(log),data=table.10.6))


## Quasi-Symmetry Model

options(contrasts = c("contr.treatment", "contr.poly"))
table.10.6$res80a<-factor(table.10.6$res80, levels=rev(residence80))
(fit.qsymm<-glm(counts~symm+res80a,family=poisson(log),data=table.10.6))

1-pchisq(fit.qsymm$deviance, df=fit.qsymm$df)


# fitted values for quasi-symmetry model
matrix(fitted(fit.qsymm),ncol=4,byrow=T,dimnames=list(rev(levels(residence80)),rev(levels(residence85))))


# using exactLoglinTest
library(exactLoglinTest)
data(residence.dat)

fit.qsymm.exact<-mcexact(y ~ res.1980 + factor(sym.pair), data=residence.dat, maxiter=10^6, nosim=10^4)


## Quasi-independence

table.10.6$D1<-as.numeric(table.10.6$symm=="1,1")
table.10.6$D2<-as.numeric(table.10.6$symm=="2,2")
table.10.6$D3<-as.numeric(table.10.6$symm=="3,3")
table.10.6$D4<-as.numeric(table.10.6$symm=="4,4")

options(contrasts = c("contr.treatment", "contr.poly"))
(fit.qi<-glm(counts~res80a+res85a+D1+D2+D3+D4,family=poisson(log),data=table.10.6))


## Square Tables with Ordered Categories

table.10.5<-data.frame(expand.grid(PreSex=factor(1:4),ExSex=factor(1:4)),counts=c(144, 33, 84, 126, 2, 4 ,14 ,29 ,0 ,2 ,6 ,25, 0, 0, 1, 5))

# quasi-symmetry (ordinal)

table.10.5$symm<-paste(
  pmin(as.numeric(table.10.5$PreSex),as.numeric(table.10.5$ExSex)),
  pmax(as.numeric(table.10.5$PreSex),as.numeric(table.10.5$ExSex)),sep=",")
table.10.5$symm<-factor(table.10.5$symm, levels=rev(table.10.5$symm))

table.10.5$scores<-rep(1:4,each=4)
options(contrasts = c("contr.treatment", "contr.poly"))
fit.oqsymm<-glm(counts~symm + scores,data=table.10.5,family=poisson(log))   


# Conditional symmetry model

# Get tau
temp<-matrix(0,nr=4,nc=4)
tau<-as.numeric(row(temp)<col(temp))

options(contrasts = c("contr.treatment", "contr.poly")) 
fit.cs<-glm(counts~symm+tau,family=poisson(log),data=table.10.5)
summary(fit.cs, cor=F)

# quasi-association
table.10.5$scores.a<-rep(1:4,4)
Delta<-as.numeric(row(temp)==col(temp))

options(contrasts = c("contr.treatment", "contr.poly")) 
table.10.5$PreSex<-factor(table.10.5$PreSex, levels=rev(table.10.5$PreSex))
table.10.5$ExSex<-factor(table.10.5$ExSex, levels=rev(table.10.5$ExSex))
fit.qa<-glm(counts~PreSex+ExSex+scores:scores.a + Delta,family=poisson(log),data=table.10.5)
summary(fit.qa, cor=F)


## Measuring Agreement

library(exactLoglinTest)
data(pathologist.dat)

dump("pathologist.dat",file="c:/program files/insightful/splus61/users/cda/pathologist.R")

names(pathologist.dat)<-c("y","B","A")
pathologist.dat$y[pathologist.dat$A==4 &(pathologist.dat$B==3 | pathologist.dat$B==4)]<-c(17,10)

pathologist.dat$A<-factor(pathologist.dat$A, levels=5:1)
pathologist.dat$B<-factor(pathologist.dat$B, levels=5:1)

(fit.ind<-glm(y~A+B, data=pathologist.dat, family=poisson(log), subset=(A!=5)&(B!=5)))

pear.std<-resid(fit.ind, type="pearson")/sqrt(1-lm.influence(fit.ind)$hat)

matrix(pear.std,nr=4,byrow=T)

# quasi-independence

pathologist.dat$D1<-as.numeric((pathologist.dat$A==1) & (pathologist.dat$B==1))
pathologist.dat$D2<-as.numeric((pathologist.dat$A==2) & (pathologist.dat$B==2))
pathologist.dat$D3<-as.numeric((pathologist.dat$A==3) & (pathologist.dat$B==3))
pathologist.dat$D4<-as.numeric((pathologist.dat$A==4) & (pathologist.dat$B==4))

(fit.qi<-glm(y~A+B+D1+D2+D3+D4,family=poisson(log),data=pathologist.dat, subset=(A!=5)&(B!=5)))

matrix(fitted(fit.qi),nr=4,byrow=T)

# quasi-symmetry

pathologist.dat2<-pathologist.dat[(pathologist.dat$A!=5)&(pathologist.dat$B!=5),]
pathologist.dat2$A<-pathologist.dat2$A[drop=T]
pathologist.dat2$B<-pathologist.dat2$B[drop=T]

pathologist.dat2$symm<-paste(
  pmin(as.numeric(pathologist.dat2$A),as.numeric(pathologist.dat2$B)),
  pmax(as.numeric(pathologist.dat2$A),as.numeric(pathologist.dat2$B)),sep=",")

pathologist.dat2$symm<-factor(pathologist.dat2$symm, levels=(pathologist.dat2$symm))

starter<-fitted(fit.qi)
starter[c(4,13)]<-0
fit.qsymm<-glm(y~symm + A, data=pathologist.dat2, family=poisson(log), control=glm.control(maxit=100), etastart=starter)

coefs<-coef(fit.qsymm)
exp(coefs["symm2,2"] + coefs["symm3,3"] - 2*coefs["symm2,3"])

matrix(fitted(fit.qsymm),nr=4,byrow=T)


## Kappa

library(exactLoglinTest)
data(pathologist.dat)

library(vcd)
path.tab<-xtabs(y~A+B, data=pathologist.dat2)
Kappa(path.tab, weights="Fleiss-Cohen")

agreementplot(path.tab)

## Bradley-Terry Model

# Using logit models (see Appendix A)

Milwaukee<-c(-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
Detroit<-c(1,0,0,0,0,0,-1,-1,-1,-1,-1,rep(0,10))
Toronto<-c(0,1,0,0,0,0,1,0,0,0,0,-1,-1,-1,-1,rep(0,6))
NY<-c(0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,-1,-1,-1,rep(0,3))
Boston<-c(0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,0,-1,-1,0)
Cleveland<-c(0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,-1)
Baltimore<-c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0)

response<-cbind(c(6,4,6,6,4,2,6,8,2,4,4,6,6,5,1,7,6,3,6,1,7),13-c(6,4,6,6,4,2,6,8,2,4,4,6,6,5,1,7,6,3,6,1,7))

options(contrasts=c("contr.treatment", "contr.poly"))
fit.BT<-glm(response~-1+Milwaukee+Detroit+Toronto+NY+Boston+Cleveland,family=binomial)   # exclude intercept
summary(fit.BT, cor=F)

## fitted counts

losing.team<-c("Milwaukee","Detroit","Toronto","NY","Boston","Cleveland","Baltimore")
win.team<-losing.team

fitted.counts<-matrix(0,nc=7,nr=7,dimnames=list(win.team,win.team))
fitted.counts[lower.tri(fitted.counts)]<-round(13*fitted(fit.BT),1)
fitted.counts[!lower.tri(fitted.counts,diag=T)]<-13-round(13*fitted(fit.BT),1)

fitted.probs<-matrix(0,nc=7,nr=7,dimnames=list(win.team,win.team))
fitted.probs[lower.tri(fitted.probs)]<-round(fitted(fit.BT),2)
fitted.probs[!lower.tri(fitted.probs,diag=T)]<-1-round(fitted(fit.BT),2)


# Fit Table 10.10 using a quasi-symmetry model.

losing.team<-c("Milwaukee","Detroit","Toronto","NY","Boston","Cleveland","Baltimore")
win.team<-losing.team

table.10.10<-expand.grid(losing=factor(losing.team, levels=rev(losing.team)),winning=factor(win.team, levels=rev(win.team)))
table.10.10$counts<-c(0,7,9,7,7,9,11,6,0,7,5,11,9,9,4,6,0,7,7,8,12,6,8,6,0,6,7,10,6,2,6,7,0,7,12,4,4,5,6,6,0,6,2,4,1,3,1,7,0)
table.10.10$symm<-paste(
  pmin(as.character(table.10.10$winning),as.character(table.10.10$losing)),
  pmax(as.character(table.10.10$winning),as.character(table.10.10$losing)),sep=",")

options(contrasts=c("contr.treatment", "contr.poly"))
fit.BTQS<-glm(counts~symm+winning,data=table.10.10,family=poisson(log))
summary(fit.BTQS, cor=F)

# fitted counts
matrix(round(fitted(fit.BTQS),1),nr=7,nc=7,byrow=T,dimnames=list(win.team,losing.team)) # byrow=T matches the data entry of the counts above


## BT model with order effect

response<-cbind(c(4,4,4,6,4,6,4,4,6,6,4,2,4,4,6,4,4,6,5,6,2,
                  3,2,3,5,2,2,4,5,2,3,1,2,3,3,1,4,4,2,4,1,3),
                c(3,2,3,1,2,0,2,3,0,1,3,4,3,2,0,3,2,1,2,0,4,
                  3,5,3,1,5,5,3,1,5,3,5,5,3,4,6,2,3,4,2,6,4))         # 42 pair sets

Milwaukee<-c( 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Detroit<-  c(-1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Toronto<-  c( 0,-1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0)
NY<-       c( 0, 0,-1, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,-1,-1,-1, 0, 0, 0)
Boston<-   c( 0, 0, 0,-1, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0,-1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0,-1,-1, 0)
Cleveland<-c( 0, 0, 0, 0,-1, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0,-1, 0,-1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0,-1)
Baltimore<-c( 0, 0, 0, 0, 0,-1, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0,-1, 0,-1,-1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1)


options(contrasts=c("contr.treatment", "contr.poly"))
fit.BTO<-glm(response~Milwaukee+Detroit+Toronto+NY+Boston+Cleveland,family=binomial)
summary(fit.BTO, cor=F)

# fitted probs
fitted.probs<-matrix(0,nc=7,nr=7,dimnames=list(win.team,win.team))
fitted.probs[lower.tri(fitted.probs,diag=F) | !lower.tri(fitted.probs,diag=T)]<-round(fitted(fit.BTO),2)

## Matched sets

table.10.13<-data.frame(expand.grid(gender=factor(c("M","F"), levels=c("M","F")), 
                                    question1=factor(c("Y","N"), levels=c("N","Y")), 
                                    question2=factor(c("Y","N"), levels=c("N","Y")), 
                                    question3=factor(c("Y","N"), levels=c("N","Y"))),
                        count=c(342,440,6,14,11,14,19,22,26,25,21,18,32,47,356,457))

temp<-apply(table.10.13[,c("question1","question2","question3")],1,function(x) paste(sort(x),collapse=","))

table.10.13$symm<-factor(temp, levels=rev(unique(temp)))

# complete symmetry
options(contrasts=c("contr.treatment", "contr.poly"))
glm(count~-1+gender+symm, family=poisson, data=table.10.13)


# quasi-symmetry
fit.qs<-glm(count~-1+gender+question1+question2+symm, family=poisson, data=table.10.13)

fitted.qs<-predict(fit.qs,type="response")/sum(table.10.13$count)
fitted.table.10.13<-cbind(table.10.13,fitted.qs)

aggregate(fitted.table.10.13$fitted.qs, list(gender=fitted.table.10.13$gender,question1=fitted.table.10.13$question1), sum)
aggregate(fitted.table.10.13$fitted.qs, list(gender=fitted.table.10.13$gender,question2=fitted.table.10.13$question2), sum)
aggregate(fitted.table.10.13$fitted.qs, list(gender=fitted.table.10.13$gender,question3=fitted.table.10.13$question3), sum)


#### Chapter 11 ####

### Comparing marginal distributions

## MPH model

table.11.1<-data.frame(expand.grid( C=factor(c("Y","N"), levels=c("N","Y")),
                                    B=factor(c("Y","N"), levels=c("N","Y")),
                                    A=factor(c("Y","N"), levels=c("N","Y"))),
                       count=c(6,16,2,4,2,4,6,6))

source("C:/Program Files/R/rw1080/CDA/mph.r")
source("C:/Program Files/R/rw1080/CDA/mph.supp.fcts.r")


# marginal homogeneity model

y <- table.11.1$count

Z <- matrix(1,2*2*2,1) 
ZF <- Z 

M1 <- Marg.fct(1,rep(2,3))  # used to get y1++, etc
M2 <- Marg.fct(2,rep(2,3))  # used to get y+1+, etc
M3 <- Marg.fct(3,rep(2,3))  # used to get y++1, etc

C.matrix <- matrix(c(
  1, 0, -1, 0, 0, 0, 
  0, 0, 1, 0, -1, 0),
                   2,6,byrow=T)      

h.fct <- function(m) { # constraint function
  marg <- rbind(M1%*%m,  M2%*%m, M3%*%m)   # y1++, y2++, y+1+, y+2+, y++1, y++2
  C.matrix%*%marg                         # y1++ = y+1+, y2++ = y+2+, etc
} 


a <- mph.fit(y=y,Z=Z,ZF=ZF,h.fct=h.fct) 

mph.summary(a)


## repeated library

library(repeated)
catmiss(sapply(table.11.1[,-4],unclass), table.11.1$count)  # esto no sirve

temp<-table.11.1[1:4,-c(3)]
marg.hom(temp$count, temp$C, temp$B)



### Marginal Modeling: MLE 

table.11.2<-read.table("c:/program files/r/rw1080/cda/depress.txt", header=T)

# marginal proportions
tapply(table.11.2$outcome,list(diagnose=table.11.2$diagnose,treat=table.11.2$treat,time=table.11.2$time), mean)

# unstack data set (order of variables, and factor levels helps give us a certain order of counts)
table.11.2a<-data.frame(unique(table.11.2[,c("case","treat","diagnose")]),unstack(table.11.2,outcome~time))
names(table.11.2a)[4:6]<-c("t1","t2","t3")

table.11.2a$diagnose<-factor(table.11.2a$diagnose, levels=c(1,0))    # factorize helps us control the ordering of the variables
table.11.2a$treat<-factor(table.11.2a$treat, levels=c(1,0))    
table.11.2a$t1<-factor(table.11.2a$t1, levels=c(1,0))    
table.11.2a$t2<-factor(table.11.2a$t2, levels=c(1,0))    
table.11.2a$t3<-factor(table.11.2a$t3, levels=c(1,0))    

# get 32 x 1 vector of counts
Y<-c(tapply(table.11.2a$case,table.11.2a[,2:6],length))
Y<-ifelse(is.na(Y),0,Y)

# design matrix
X<-cbind(1,expand.grid(time=0:2, treat=c(1,0), diagnose=c(1,0) ))

# population and sampling constraint matrices
Z<-kronecker(matrix(rep(1,8),nc=1),diag(4))
ZF<-Z


A.matrix<-matrix(c(1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, # g1 t0=1
                   1,0,0,0, 1,0,0,0, 0,0,0,0, 0,0,0,0, 1,0,0,0, 1,0,0,0, 0,0,0,0, 0,0,0,0, # g1 t1=1
                   1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g1 t2=1
                   
                   # complement
                   0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, # g1 t0=0
                   0,0,0,0, 0,0,0,0, 1,0,0,0, 1,0,0,0, 0,0,0,0, 0,0,0,0, 1,0,0,0, 1,0,0,0, # g1 t1=0
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, # g1 t2=0
                   
                   0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, # g2 t0=1
                   0,1,0,0, 0,1,0,0, 0,0,0,0, 0,0,0,0, 0,1,0,0, 0,1,0,0, 0,0,0,0, 0,0,0,0, # g2 t1=1
                   0,1,0,0, 0,1,0,0, 0,1,0,0, 0,1,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g2 t2=1
                   
                   # complement
                   0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, # g2 t0=0
                   0,0,0,0, 0,0,0,0, 0,1,0,0, 0,1,0,0, 0,0,0,0, 0,0,0,0, 0,1,0,0, 0,1,0,0, # g2 t1=0
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,1,0,0, 0,1,0,0, 0,1,0,0, 0,1,0,0, # g2 t2=0
                   
                   0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, # g3 t0=1
                   0,0,1,0, 0,0,1,0, 0,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0, 0,0,0,0, # g3 t1=1
                   0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g3 t2=1
                   
                   # complement
                   0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, # g3 t0=0
                   0,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,1,0, # g3 t1=0
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0, # g3 t2=0
                   
                   0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, # g4 t0=1
                   0,0,0,1, 0,0,0,1, 0,0,0,0, 0,0,0,0, 0,0,0,1, 0,0,0,1, 0,0,0,0, 0,0,0,0, # g4 t1=1
                   0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g4 t2=1
                   
                   # complement
                   0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, # g4 t0=0
                   0,0,0,0, 0,0,0,0, 0,0,0,1, 0,0,0,1, 0,0,0,0, 0,0,0,0, 0,0,0,1, 0,0,0,1, # g4 t1=0
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1), # g4 t2=0
                 
                 nr=24,nc=32, byrow=TRUE)


C.matrix<-kronecker(diag(4),matrix(c( 
  1, 0, 0, -1, 0, 0,   
  0, 1, 0, 0, -1, 0, 
  0, 0, 1, 0, 0, -1),nr=3,nc=6,byrow=T))

L.fct <- function(m) {  
  C.matrix%*%log(A.matrix%*%m)
} 

fit<-mph.fit(y=Y,Z=Z,ZF=ZF,X=as.matrix(X),L.fct=L.fct)

mph.summary(fit)


# Add time by treatment interaction

X$trtxtime<-X$treat*X$time

fit.ia<-mph.fit(y=Y,Z=Z,ZF=ZF,X=as.matrix(X),L.fct=L.fct)
mph.summary(fit.ia)


## Modeling a repeated ordinal response

table.11.4<-read.table("c:/program files/r/rw1080/cda/insomnia.txt", header=T)

# marginal proportions
n<-array(tapply(table.11.4$case,list(treat=table.11.4$treat, time=table.11.4$time),length),dim=c(2,4,2))
n<-aperm(n, c(3,2,1))

xtabs(~time+outcome+treat, data=table.11.4)/n


# unstack data set (order of variables, and factor levels helps give us a certain order of counts)
table.11.4a<-data.frame(unique(table.11.4[,c("case","treat")]),unstack(table.11.4,outcome~time))
names(table.11.4a)[3:4]<-c("initial","followup")

table.11.4a$treat<-factor(table.11.4a$treat, levels=c(1,0))    # factorize helps us control the ordering of the variables
table.11.4a$initial<-factor(table.11.4a$initial, levels=1:4)    
table.11.4a$followup<-factor(table.11.4a$followup, levels=1:4)    


# get 32 x 1 vector of counts
Y<-c(tapply(table.11.4a$case,table.11.4a[,2:4],length))
Y<-ifelse(is.na(Y),0,Y)

# design matrix
X<-data.frame(kronecker(diag(3),rep(1,4)),treat=1:0,time=c(0,0,1,1))
X$trtxtime<-X$time*X$treat

# population and sampling constraint matrices
Z<-kronecker(matrix(rep(1,16),nc=1),diag(2))
ZF<-Z


A.matrix<-matrix(c(1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0, # g1 t1=<1
                   1,0,1,0, 1,0,1,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g1 t2=<1
                   # complement
                   0,0,1,0, 1,0,1,0, 0,0,1,0, 1,0,1,0, 0,0,1,0, 1,0,1,0, 0,0,1,0, 1,0,1,0, # g1 t1>1
                   0,0,0,0, 0,0,0,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, # g1 t2>1
                   
                   1,0,1,0, 0,0,0,0, 1,0,1,0, 0,0,0,0, 1,0,1,0, 0,0,0,0, 1,0,1,0, 0,0,0,0, # g1 t1=<2
                   1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g1 t2=<2
                   # complement
                   0,0,0,0, 1,0,1,0, 0,0,0,0, 1,0,1,0, 0,0,0,0, 1,0,1,0, 0,0,0,0, 1,0,1,0, # g1 t1>2
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, # g1 t2>2
                   
                   1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0, # g1 t1=<3
                   1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0, 0,0,0,0, 0,0,0,0, # g1 t2=<3
                   # complement
                   0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,1,0, # g1 t1>3
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 1,0,1,0, 1,0,1,0, # g1 t2>3
                   
                   
                   0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,1,0,0, 0,0,0,0, # g2 t1=<1
                   0,1,0,1, 0,1,0,1, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g2 t2=<1
                   # complement
                   0,0,0,1, 0,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,1, 0,1,0,1, # g2 t1>1
                   0,0,0,0, 0,0,0,0, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, # g2 t2>1
                   
                   0,1,0,1, 0,0,0,0, 0,1,0,1, 0,0,0,0, 0,1,0,1, 0,0,0,0, 0,1,0,1, 0,0,0,0, # g2 t1=<2
                   0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, # g2 t2=<2
                   # complement
                   0,0,0,0, 0,1,0,1, 0,0,0,0, 0,1,0,1, 0,0,0,0, 0,1,0,1, 0,0,0,0, 0,1,0,1, # g2 t1>2
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, # g2 t2>2
                   
                   0,1,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0, 0,1,0,1, 0,1,0,0, # g2 t1=<3
                   0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,1,0,1, 0,0,0,0, 0,0,0,0, # g2 t2=<3
                   # complement
                   0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1, # g2 t1>3
                   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,1,0,1, 0,1,0,1),# g2 t2>3
                 
                 nr=24,nc=32, byrow=TRUE)

C.matrix<-matrix(
  c(1,0, -1,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, # P(time1 =< 1 for group1)
    0,0,  0,0, 0,0, 0,0, 0,0, 0,0, 1,0, -1,0, 0,0, 0,0, 0,0, 0,0, # P(time1 =< 1 for group2)
    
    0,1,  0,-1, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, # P(time2 =< 1 for group1)
    0,0,  0,0, 0,0, 0,0, 0,0, 0,0, 0,1, 0,-1, 0,0, 0,0, 0,0, 0,0, # P(time2 =< 1 for group2)
    
    0,0,  0,0, 1,0, -1,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, # P(time1 =< 2 for group1)
    0,0,  0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,0, -1,0, 0,0, 0,0, # P(time1 =< 2 for group2)
    
    0,0,  0,0, 0,1,  0,-1, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, # P(time2 =< 2 for group1)
    0,0,  0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,1, 0,-1, 0,0, 0,0, # P(time2 =< 2 for group2)
    
    0,0,  0,0, 0,0,  0,0, 1,0, -1,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, # P(time1 =< 3 for group1)
    0,0,  0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 1,0, -1,0, # P(time1 =< 3 for group2)
    
    0,0,  0,0, 0,0,  0,0, 0,1,  0,-1, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, # P(time2 =< 3 for group1) 
    0,0,  0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,1, 0,-1), # P(time2 =< 3 for group2)
  
  nr=12,nc=24,byrow=T)


L.fct <- function(m) {  
  C.matrix%*%log(A.matrix%*%m)
} 

fit<-mph.fit(y=Y,Z=Z,ZF=ZF,X=as.matrix(X),L.fct=L.fct)

mph.summary(fit)


## GEE

table.11.2<-read.table("c:/program files/r/rw1080/cda/depress.txt", header=T)
table.11.2b<-table.11.2
table.11.2b$diagnose<-ifelse(table.11.2b$diagnose==0,"mild","severe")
table.11.2b$diagnose<-factor(table.11.2b$diagnose, levels=c("mild","severe"))
table.11.2b$treat<-ifelse(table.11.2b$treat==0,"standard","new")
table.11.2b$treat<-factor(table.11.2b$treat,levels=c("standard","new"))
table.11.2b<-table.11.2b[order(table.11.2b$case),]

# yags
library(yags)
fit.yags<-yags(outcome~diagnose + time*treat, id=case, cor.met=I(2^time), family=binomial, corstruct="exchangeable", data=table.11.2b, alphainit=.05)

# gee
library(gee)
fit.gee<-gee(outcome~diagnose + time*treat, id=case, family=binomial, corstr="exchangeable", data=table.11.2b) 
summary(fit.gee)

library(geepack)
fit.geese<-geese(outcome~diagnose + time*treat, id=case, data=table.11.2b, family=binomial, corstr="exch")
summary(fit.geese)


## repeated cumulative logit 

table.11.4<-read.table("c:/program files/r/rw1080/cda/insomnia.txt", header=T)
table.11.4ord<-table.11.4[order(table.11.4$case, table.11.4$time),]
table.11.4ord$outcome<-ordered(table.11.4ord$outcome, levels=1:4)
#table.11.4ord$treat<-factor(table.11.4$treat, levels=c(1,0))    
#table.11.4ord$time<-factor(table.11.4$time, levels=c(0,1))    

library(geepack)
fit.ordgee<-ordgee(outcome~treat*time, id=case , data = table.11.4ord, corstr = "independence", rev=TRUE,
                   control = geese.control(maxit = 100))
summary(fit.ordgee)


## Transitional Models

# Table 11.7
table.11.7<-data.frame(expand.grid(Y12=c(1,0), Y11=c(1,0), Y10=c(1,0), Y9=c(1,0)), 
                       count=c(94,30,15,28,14,9,12,63,19,15,10,44,17,42,35,572))


library(MASS)

# 1st-order model
(fit.loglm1<-loglm(count~Y9*Y10+Y10*Y11+Y11*Y12, data=table.11.7, param=TRUE, fit=TRUE))

# 2nd-order model
(fit.loglm2<-loglm(count~Y9*Y10*Y11+Y10*Y11*Y12, data=table.11.7, param=TRUE, fit=TRUE))

# all pairwise associations
(fit.loglm3<-loglm(count~ Y9*Y10 + Y11*Y12 + Y9*Y11 + Y10*Y12 + Y9*Y12 + Y10*Y11, data=table.11.7, param=TRUE, fit=TRUE))

# simpler model
oneyear<-cbind(table.11.7$Y9*table.11.7$Y10, table.11.7$Y10*table.11.7$Y11, table.11.7$Y11*table.11.7$Y12)
gtoneyear<-cbind(table.11.7$Y9*table.11.7$Y11, table.11.7$Y9*table.11.7$Y12, table.11.7$Y10*table.11.7$Y12)
table.11.7$oneyear<-rowSums(oneyear)
table.11.7$gtoneyear<-rowSums(gtoneyear)

(fit.glm4<-glm(count~Y9 + Y10 + Y11 + Y12 + oneyear + gtoneyear, data=table.11.7, family=poisson))



# transitional models with explanatory variables

# Table 11.9
table.11.9.ref<-data.frame(expand.grid(Y10=c(0,1), Y9=c(0,1), Y8=c(0,1), Y7=c(0,1), mat.smoke=c(0,1)), 
                           count=c(237,10,15,4,16,2,7,3,24,3,3,2,6,2,5,11,118,6,8,2,11,1,6,4,7,3,3,1,4,2,4,7))

table.11.9<-data.frame(expand.grid(previous=c(0,1), t=8:10, mat.smoke=c(0,1)))

count.yes<-c(
  sum(table.11.9.ref[table.11.9.ref$Y8==1 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y7==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y8==1 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y7==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==1 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y8==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==1 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y8==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==1 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y9==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==1 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y9==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y8==1 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y7==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y8==1 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y7==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==1 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y8==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==1 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y8==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==1 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y9==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==1 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y9==1,"count"])
  )

count.no<-c(
  sum(table.11.9.ref[table.11.9.ref$Y8==0 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y7==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y8==0 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y7==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==0 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y8==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==0 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y8==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==0 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y9==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==0 & table.11.9.ref$mat.smoke==0 & table.11.9.ref$Y9==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y8==0 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y7==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y8==0 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y7==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==0 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y8==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y9==0 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y8==1,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==0 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y9==0,"count"]),
  sum(table.11.9.ref[table.11.9.ref$Y10==0 & table.11.9.ref$mat.smoke==1 & table.11.9.ref$Y9==1,"count"])
  )

table.11.9$prop<-count.yes/(total<-count.yes+count.no)

fit.glm5<-glm(prop ~ previous + t + mat.smoke, data=table.11.9, family=binomial, weight=total)
summary(fit.glm5)


##### Chapter 12 #####

table.12.1<-read.table("primeminister.txt",col.names=c("case", "occasion", "response", "count"))

temp1<-cbind(case=rep(1:794,each=2),table.12.1[rep(1:2,794),2:3])
temp2<-cbind(case=rep(795:(794+150),each=2),table.12.1[rep(3:4,150),2:3])
temp3<-cbind(case=rep(945:(944+86),each=2),table.12.1[rep(5:6,86),2:3])
temp4<-cbind(case=rep(1031:(1030+570),each=2),table.12.1[rep(7:8,570),2:3])

table.12.1a<-rbind(temp1,temp2,temp3,temp4)
table.12.1a$case<-factor(table.12.1a$case)

library(MASS)
fit.glmmPQL<-glmmPQL(response~occasion, random=~1 | case , family=binomial, data=table.12.1a)

library(repeated)
fit.glmm<-glmm(response~occasion , nest=case, family=binomial, data=table.12.1a, points=10)

library(glmmML)
fit.glmmML<-glmmML(response~occasion , cluster=table.12.1a$case, family=binomial, data=table.12.1a)

# Small-area estimation

table.12.2<-read.table("vote.txt",col.names=c("y","n"))
table.12.2$state<-1:nrow(table.12.2)

library(MASS)
fit.glmmPQL<-glmmPQL(y/n~1, random=~1 | state , weights=n,family=binomial, data=table.12.2)
summary(fit.glmmPQL)
exp(fitted(fit.glmmPQL))/(1+exp(fitted(fit.glmmPQL)))

library(repeated)
fit.glmm<-glmm(cbind(y,n-y)~1 , nest=state, family=binomial, data=table.12.2, points=10)
summary(fit.glmm)


# Repeated binary response

table.10.13a<-reshape(table.10.13, varying=list(names(table.10.13)[2:4]),drop="symm",direction="long", 
                      v.names="response", timevar="question")
table.10.13a$question<-factor(table.10.13a$question,levels=3:1)
table.10.13b<-table.10.13a[rep(1:nrow(table.10.13a),table.10.13a$count),c("response","gender","question")]
table.10.13b$id<-factor(rep(1:1850,3))
table.10.13b$response<-c(unclass(table.10.13b$response)-1)
#table.10.13b$gender<-c(unclass(table.10.13b$gender)-1)

require(glmmML)
fit.glmmML<-glmmML(response~gender+question, cluster=table.10.13b$id, family=binomial, data=table.10.13b)

library(MASS)
fit.glmmPQL<-
  glmmPQL(response~gender+question, random=~1|id, family=binomial, data=table.10.13b)
summary(fit.glmmPQL)

fit.glmmPQL<-glmmPQL(response~gender+time, random=~1|id, family=binomial, data=table.10.13a, weights=count)
summary(fit.glmmPQL)



# predictions

invlogit<-function (x) {exp(x)/(1+exp(x))}
beta.coef<-c(fit.glmmML$coef[c("question1","question2")],0)
female<-1

f<-function(u){
  prod(invlogit(u+fit.glmmML$coef["(Intercept)"]+ beta.coef +
    fit.glmmML$coef["genderF"]*female))*dnorm(u,0,fit.glmmML$sigma)
}

library(rmutil)
1037*int(f)

integrate(f,lower=-Inf,upper=Inf)


# longitudinal mental depression data

library(glmmML)
fit.glmmML<-glmmML(outcome~diagnose+treat*time, cluster=table.11.2$case, family=binomial, data=table.11.2)

library(MASS)
fit.glmmPQL<-
  glmmPQL(outcome~diagnose+treat*time, random=~1|case, family=binomial, data=table.11.2)
summary(fit.glmmPQL)

library(repeated)
fit.glmm<-glmm(outcome~diagnose+treat*time, nest=case, family=binomial, data=table.11.2, points=10)
summary(fit.glmm)


# predictions

invlogit<-function (x) {exp(x)/(1+exp(x))}
diagnosis<-1
drug<-1
beta.coef<-(fit.glmmML$coef["time"] + drug*fit.glmmML$coef["treat:time"])*0:2

f<-function(u){
  prod(invlogit(u+fit.glmmML$coef["(Intercept)"]+fit.glmmML$coef["diagnose"]*diagnosis+beta.coef))*dnorm(u,0,fit.glmmML$sigma)
}

library(rmutil)
340*int(f)

integrate(f,lower=-Inf,upper=Inf)


## Multicenter clinical trials

#Recall
# table.6.9<-data.frame(scan(file="c:/program files/insightful/splus61/users/CDA/clinical trials table 69.ssc", what=list(Center="",Treatment="",Response="",Freq=0)))
# levels(table.6.9$Treatment)<-c("Drug","Control")
# levels(table.6.9$Response)<-c("Success","Failure")

table.6.9$TreatC<-ifelse(table.6.9$Treatment=="Drug",.5,-.5)
table.6.9$RespC<-ifelse(table.6.9$Response=="Success",1,0)

table.6.9a<-table.6.9[rep(1:nrow(table.6.9),table.6.9$Freq),c("RespC","TreatC","Center")]


library(MASS)
fit.glmmPQL.int<-
  glmmPQL(RespC~TreatC, random=~1|Center, family=binomial, data=table.6.9a)
summary(fit.glmmPQL.int)

fit.glmmPQL.ia<-
  glmmPQL(RespC~TreatC, random=~TreatC|Center, family=binomial, data=table.6.9a)
summary(fit.glmmPQL.ia)


# predictions

invlogit<-function (x) {exp(x)/(1+exp(x))}

odds.treat<- invlogit(as.matrix(coef(fit.glmmPQL.ia))%*%c(1,.5))
odds.control<-invlogit(as.matrix(coef(fit.glmmPQL.ia))%*%c(1,-.5))

odds.treat/odds.control 

## Capture-recapture

temp<-matrix(c(
  c(0,0,1,0,0,0),
  c(0,1,0,0,0,0),
  c(0,1,1,0,0,0),
  c(1,0,0,0,0,0),
  c(1,0,1,0,0,0),
  c(1,1,0,0,0,0),
  c(1,1,1,0,0,0),
  
  c(0,0,0,0,0,1),
  c(0,0,1,0,0,1),
  c(0,1,0,0,0,1),
  c(0,1,1,0,0,1),
  c(1,0,0,0,0,1),
  c(1,0,1,0,0,1),
  c(1,1,0,0,0,1),
  c(1,1,1,0,0,1),
  
  c(0,0,0,0,1,0),
  c(0,0,1,0,1,0),
  c(0,1,0,0,1,0),
  c(0,1,1,0,1,0),
  c(1,0,0,0,1,0),
  c(1,0,1,0,1,0),
  c(1,1,0,0,1,0),
  c(1,1,1,0,1,0),
  
  c(0,0,0,0,1,1),
  c(0,0,1,0,1,1),
  c(0,1,0,0,1,1),
  c(0,1,1,0,1,1),
  c(1,0,0,0,1,1),
  c(1,0,1,0,1,1),
  c(1,1,0,0,1,1),
  c(1,1,1,0,1,1),
  
  c(0,0,0,1,0,0),
  c(0,0,1,1,0,0),
  c(0,1,0,1,0,0),
  c(0,1,1,1,0,0),
  c(1,0,0,1,0,0),
  c(1,0,1,1,0,0),
  c(1,1,0,1,0,0),
  c(1,1,1,1,0,0),
  
  c(0,0,0,1,0,1),
  c(0,0,1,1,0,1),
  c(0,1,0,1,0,1),
  c(0,1,1,1,0,1),
  c(1,0,0,1,0,1),
  c(1,0,1,1,0,1),
  c(1,1,0,1,0,1),
  c(1,1,1,1,0,1),
  
  c(0,0,0,1,1,0),
  c(0,0,1,1,1,0),
  c(0,1,0,1,1,0),
  c(0,1,1,1,1,0),
  c(1,0,0,1,1,0),
  c(1,0,1,1,1,0),
  c(1,1,0,1,1,0),
  c(1,1,1,1,1,0),
  
  c(0,0,0,1,1,1),
  c(0,0,1,1,1,1),
  c(0,1,0,1,1,1),
  c(0,1,1,1,1,1),
  c(1,0,0,1,1,1),
  c(1,0,1,1,1,1),
  c(1,1,0,1,1,1),
  c(1,1,1,1,1,1)), byrow=T,nc=6)

counts<-c(3,6,0,5,1,0,0,3,2,3,0,0,1,0,0,4,2,3,1,0,1,0,0,1,0,0,0,0,0,0,0,4,1,1,1,2,0,2,0,4,0,3,0,1,0,2,0,2,0,1,0,1,0,1,0,1,1,1,0,0,0,1,2)

table.12.6<-data.frame(temp,counts)
names(table.12.6)<-c(paste("Survey",c(3:1,6:4),sep=""),"counts")

table.12.6a<-table.12.6[rep(1:nrow(table.12.6),table.12.6$counts),1:6]
row.names(table.12.6a)<-1:nrow(table.12.6a)

table.12.6b<-reshape(table.12.6a,direction="long",varying=list(names(table.12.6a)),timevar="Survey",v.names="Capture")
table.12.6b$Survey<-factor(table.12.6b$Survey)


## MLE and profile-LH CI

require(MASS)
fit.glmmPQL<-glmmPQL(Capture~Survey-1, random=~1|id, family=binomial, data=table.12.6b)
beta.coef<-fit.glmmPQL$coef$fixed

temp2<-rbind(rep(0,6),temp)
invlogit.i<-function (x,i) {exp(i*x)/(1+exp(x))}

func2<-function(y) {
  
  counts<-c(exp(y[8]),3,6,0,5,1,0,0,3,2,3,0,0,1,0,0,4,2,3,1,0,1,0,0,1,0,0,0,0,0,0,0,4,1,1,1,2,0,2,0,4,0,3,0,1,0,2,0,2,0,1,0,1,0,1,0,1,1,1,0,0,0,1,2)
  
  require(rmutil)
  
  probs<-apply(temp2,1,function(x){
    i<-x
    f<-function(u){
      prod(invlogit.i(y[1:6]+u,i))*dnorm(u,0,exp((y[7])))
    }
    int(f)
    #integrate(f,lower=-10000,upper=10000)
  })
  -lgamma(sum(counts)+1) +sum(lgamma(counts+1)) - sum(counts*log(probs))
}

result<-optim(par=c(beta.coef,log(fit.glmmPQL$sigma),log(18)),func2,control=list(fnscale=1,trace=6,REPORT=1),method="BFGS")
result.values<-result$par

# profile LH CI

library(Bhat)

x <- list(label=c("beta1","beta2","beta3","beta4","beta5","beta6","lsig","lN"),
          est=result.values,low=c(rep(-4,6),-1,-10),upp=c(rep(4,6),2,10))

ans<-plkhci(x,func2,"lN",prob=0.95)
exp(ans)+68
[1]  73.39961 141.07961


## Conditional N-estimation

func<-function(Beta,counts,n,matrix.i) {
  
  require(rmutil)
  
  probs<-apply(matrix.i,1,function(x){
    i<-x
    f<-function(u){
      prod(invlogit.i(Beta[1:6]+u,i))*dnorm(u,0,exp(Beta[7]))
    }
    int(f)
  })
  -sum(counts*log(probs/sum(probs)))
}

junk<-optim(par=c(beta.coef,log(fit.glmmPQL$sigma)),fn=func,control=list(trace=2), method="BFGS",
            counts=counts,n=68,matrix.i=temp)


beta.coef.000<-junk$par[1:6]
sigma.000<-exp(junk$par[7])

invlogit<-function (x) {exp(x)/(1+exp(x))}

prob.000000<-function(u){
  prod(1-invlogit(beta.coef.000+u))*dnorm(u,0,sigma.000)
}

library(rmutil)
68/(1-int(prob.000000))

## Random effects models for multinomial data

# cumulative logit model

table.11.4<-read.table("c:/program files/r/rw1080/cda/insomnia.txt", header=T)
table.11.4$outcome<-factor(table.11.4$outcome)
table.11.4$treat<--table.11.4$treat
table.11.4$time<--table.11.4$time

library(MASS)
fit<-polr(outcome~treat*time, data=table.11.4)

table.11.4a<-reshape(table.11.4, direction="wide", v.names = "outcome", timevar = "time",
                     idvar = "case",drop="count")
table.11.4a<-apply(table.11.4a,2,as.numeric)[,-1]    # make sure all numeric columns, and exclude case


invlogit<-function (x) {exp(x)/(1+exp(x))}

func<-function(Beta,matrix.i) { # three alphas, three betas, one log sigma
  
  require(rmutil)
  
  probs<-apply(matrix.i,1,function(x){
    f<-function(u){
      treat<-x[1]
      
      prob1.0<-prob1.1<-prob2.0<-prob2.1<-prob3.0<-prob3.1<-prob4.0<-prob4.1<-1
      
      if(x[2]==1) prob1.0<-invlogit(Beta[1] + Beta[5]*treat + u) 
      else if(x[2]==2) prob2.0<-invlogit(Beta[2] + Beta[5]*treat + u) - invlogit(Beta[1] + Beta[5]*treat + u)
      else if(x[2]==3) prob3.0<-invlogit(Beta[3] + Beta[5]*treat + u) - invlogit(Beta[2] + Beta[5]*treat + u)
      else if(x[2]==4) prob4.0<-1-invlogit(Beta[3] + Beta[5]*treat + u)
      if(x[3]==1) prob1.1<-invlogit(Beta[1] + Beta[4] + Beta[5]*treat + Beta[6]*treat + u)
      else if(x[3]==2) prob2.1<-invlogit(Beta[2] + Beta[4] + Beta[5]*treat + Beta[6]*treat + u) - invlogit(Beta[1] + Beta[4] + Beta[5]*treat + Beta[6]*treat + u)
      else if(x[3]==3) prob3.1<-invlogit(Beta[3] + Beta[4] + Beta[5]*treat + Beta[6]*treat + u) - invlogit(Beta[2] + Beta[4] + Beta[5]*treat + Beta[6]*treat + u)
      else if(x[3]==4) prob4.1<-1-invlogit(Beta[3] + Beta[4] + Beta[5]*treat + Beta[6]*treat + u)
      
      prob1.0*prob1.1*prob2.0*prob2.1*prob3.0*prob3.1*prob4.0*prob4.1*dnorm(u,0,exp(Beta[7]))
      
      
    }
    int(f)
  })
  -sum(log(probs))
}

#junk<-optim(par=c(fit$zeta,fit$coefficients[c("time","treat","treat:time")],.5),fn=func,control=list(trace=2,REPORT=1), method="BFGS",
#matrix.i=as.matrix(table.11.4a))

junk<-optim(par=c(-3.4858986, -1.4830128, 0.5606742,1.602,.058,1.081,log(1.9)),fn=func,control=list(trace=2,REPORT=1), method="BFGS",
            matrix.i=as.matrix(table.11.4c))


## Multivariate random effects for binary data

table.12.8<-read.table("c:/program files/r/rw1090/cda/crowd.txt", header=T)

invlogit<-function (x) {exp(x)/(1+exp(x))}

func<-function(Beta,matrix.i) { 
  
  require(adapt)
  require(mvtnorm)
  
  sigma11<-exp(Beta[5])
  sigma22<-exp(Beta[6])
  
  probs<-apply(matrix.i,1,function(x){
    f<-function(u){
      mem1<-x[1]
      att1<-x[2]
      mem2<-x[3]
      att2<-x[4]
      
      prob.mem1<-invlogit(Beta[1]+ u[1])
      prob.mem2<-invlogit(Beta[2]+ u[1])
      
      prob.att1<-invlogit(Beta[3]+ u[2])
      prob.att2<-invlogit(Beta[4]+ u[2])
      
      log.prob<-mem1*log(prob.mem1) + (1-mem1)*log(1-prob.mem1) + mem2*log(prob.mem2) + (1-mem2)*log(1-prob.mem2) +
        att1*log(prob.att1) + (1-att1)*log(1-prob.att1) + att2*log(prob.att2) + (1-att2)*log(1-prob.att2) 
      
      exp(log.prob)*exp(dnorm(u[1], mean=0, sd=sigma11,log=T))*
        exp(dnorm(u[2], mean=u[1]*Beta[7]*sigma22/sigma11, sd=sigma22*sqrt(1-Beta[7]^2),log=T))
    }
    #        adapt(ndim=2,lower=rep(-10,2), upper=rep(10,2), eps=.001,functn=f)$value
    GL.integrate.2D(fct=f, low=rep(-10,2), upp=rep(10,2))
    
  })
  
  -sum(matrix.i[,5]*log(probs))
}

# Diego Kuonen's GL.integrate.2D

GL.integrate.2D<-function(fct, low, upp, order=10,...)
{
  YW.list<-as.list(1:2)
  for(i in 1:2) {
    name.YW<-paste("GL.YW", abs(low[i]), abs(upp[i]), order,
                   sep=".")
    if(!exists(name.YW)){
      assign(name.YW, GL.YW(order,
                            xrange=c(low[i],upp[i])),
             pos=1, immediate=T)
    }
    YW.list[[i]]<-get(name.YW)
  }
  
  point<-expand.grid(YW.list[[1]][,1], YW.list[[2]][,1])  
  
  fcteval<-apply(point,1,fct,...)
  fcteval<-matrix(fcteval,nr=order,nc=order,byrow=F)
  sum(YW.list[[1]][,2]*apply(YW.list[[2]][,2]*fcteval,2,sum))
}


# Diego Kuonen's GL.YW

GL.YW<-function(M, xrange=NULL, epsilon=NULL)
{
  if (is.null(epsilon)) epsilon<-.Machine$double.eps
  if(M%%2==1) stop("M needs to be an even number")
  MM<-(M+1)/2
  Y<-W<-numeric(M)
  for(i in 1:floor(MM)) {
    ok<-F
    z<-cos(pi*(i-0.25)/(M+0.5))
    while(ok==F){
      p1<-1.0
      p2<-0
      for(j in 1:M) {
        p3<-p2
        p2<-p1
        p1<-((2*j-1)*z*p2-(j-1)*p3)/j
      }
      pp<-M*(z*p1-p2)/(z^2 -1)
      z1<-z
      z<-z-p1/pp
      if(abs(z-z1)<epsilon) ok<-T
    }
    Y[i]<--z
    Y[M+1-i]<-z
    W[i]<-2/((1-z^2)*pp^2)
    W[M+1-i]<-W[i]
  }
  if(!is.null(xrange)) {
    xL<-(xrange[2]-xrange[1])/2
    W<-xL*W
    Y<-xL*Y + (xrange[1]+xrange[2])/2
  }
  cbind(Y,W)
}


res<-optim(par=c(-1.105,-.74,.21,.39,log(3.1),log(1.5),.3),
           fn=func,control=list(trace=6,REPORT=1, parscale=c(1,1,1,1,1,1,1)),method="L-BFGS-B",hessian=T,
           lower=c(rep(-Inf,6),-.99), upper=c(rep(Inf,6),.99),matrix.i=as.matrix(table.12.8))

# ASE
se<-sqrt(diag(solve(res$hessian)))
names(se)<-c("beta1m","beta2m","beta1a","beta2a","log(Sm)","log(Sa)","rho")
se

# method 2

data1<-table.12.8[rep(1:nrow(table.12.8),table.12.8$count),]
data1<-cbind(id=1:nrow(data1),data1[,1:4])
data1<-reshape(data1, direction="long", varying=list(c("mem1","att1","mem2","att2")),v.names="response",
               timevar="occasion")

data2<-cbind(data1$response,data1$id,rep(c(1,0,1,0),each=nrow(data1)/4),rep(c(0,1,0,1),each=nrow(data1)/4),
             rep(c(1,1,0,0),each=nrow(data1)/4))
data2<-as.data.frame(data2)
names(data2)<-c("response","id","var1","var2","time")

temp<-glmmPQL(response~-1 + var1+var2+var1:time + var2:time,data=data2,family=binomial,random=~-1+var1+var2|id,
              niter=5)    # this is sort of okay, but only runs for 4 iterations 
summary(temp)


## Continuation ratio logits for clustered ordinal outcomes

table.12.9<-read.table("c:/program files/r/rw2000/cda/toxic.txt", header=T)

doses<-c(0,.75,1.5,3)

invlogit<-function (x) {exp(x)/(1+exp(x))}

func.common.sig<-function(Beta,matrix.i) { 
  
  require(adapt)
  require(mvtnorm)   
  
  probs<-apply(matrix.i,1,function(x){
    f<-function(u){
      
      sigma.dead<-exp(Beta[5])
      sigma.mal<-exp(Beta[6])
      
      dose<-doses[x[2]]
      dead<-x[3]
      mal<-x[4]
      normal<-x[5]
      n<-sum(x[3:5])
      
      prob.dead<-invlogit(Beta[1]*dose + Beta[2] + u[1])
      prob.mal.given.alive<-invlogit(Beta[3]*dose + Beta[4] + u[2])
      
      
      log.prob<-dead*log(prob.dead)  + mal*log(prob.mal.given.alive) + 
        (normal)*log(1-prob.mal.given.alive)  + (n-dead)*log(1-prob.dead)
      
      exp(log.prob)*exp(dnorm(u[1], mean=0, sd=sigma.dead,log=T))*
        exp(dnorm(u[2], mean=u[1]*Beta[7]*sigma.mal/sigma.dead, sd=sigma.mal*sqrt(1-Beta[7]^2),log=T))
    }
    #        adapt(ndim=2,lower=rep(-5,2), upper=rep(5,2), eps=.001,functn=f)$value
    GL.integrate.2D(fct=f, low=rep(-5,2), upp=rep(5,2))
    
  })
  
  -sum(log(probs))
}


library(VGAM)
table.12.9a<-aggregate(table.12.9,by=list(dose=table.12.9$dose),FUN=sum)
#type rm(logit) if below gives error about logit function
fit.vglm<-vglm(cbind( dead, malformed,normal )~doses,family=sratio(link ="logit", parallel = F), data=table.12.9a)

# fit model
res<-optim(par=c(coef(fit.vglm)[c(3,1,4,2)],log(1),log(1),0),
           fn=func.common.sig,control=list(trace=6,REPORT=1),method="L-BFGS-B",hessian=T,
           lower=c(rep(-Inf,6),-.99), upper=c(rep(Inf,6),.99),matrix.i=as.matrix(table.12.9))

# MLEs
c(res$par[1:4],sigma1=exp(res$par[5]),sigma2=exp(res$par[6]),rho=res$par[7])

# ASEs
sqrt(diag(solve(res$hessian)))


# obj function for distinct covariance matrices per random effect #

func<-function(Beta,matrix.i) { 
  
  require(adapt)
  require(mvtnorm)   
  
  probs<-apply(matrix.i,1,function(x){
    f<-function(u){
      
      sigma.dead<-exp(Beta[3*x[2]+2])
      sigma.mal<-exp(Beta[3*x[2]+3])
      
      dose<-doses[x[2]]
      dead<-x[3]
      mal<-x[4]
      normal<-x[5]
      n<-sum(x[3:5])
      
      prob.dead<-invlogit(Beta[1]*dose + Beta[2] + u[1])
      prob.mal.given.alive<-invlogit(Beta[3]*dose + Beta[4] + u[2])
      
      log.prob<-dead*log(prob.dead)  + mal*log(prob.mal.given.alive) + 
        (normal)*log(1-prob.mal.given.alive)  + (n-dead)*log(1-prob.dead)
      
      exp(log.prob)*exp(dnorm(u[1], mean=0, sd=sigma.dead,log=T))*
        exp(dnorm(u[2], mean=u[1]*Beta[3*x[2]+4]*sigma.mal/sigma.dead, sd=sigma.mal*sqrt(1-Beta[3*x[2]+4]^2),log=T))
    }
    adapt(ndim=2,lower=rep(-5,2), upper=rep(5,2), eps=.001,functn=f)$value
    #        GL.integrate.2D(fct=f, low=rep(-5,2), upp=rep(5,2))
    
  })
  
  -sum(log(probs))
}

# fit model
res<-optim(par=c(.08,.1,1.79,.1,log(1),log(1),-.1,log(1),log(1),-.1,log(1),log(1),-.1,log(1),log(1),-.1),
           fn=func,control=list(trace=6,REPORT=1),method="L-BFGS-B",hessian=F,
           lower=c(rep(-Inf,6),-.99,rep(-Inf,2),-.99,rep(-Inf,2),-.99,rep(-Inf,2),-.99), 
           upper=c(rep(Inf,6),.99,rep(Inf,2),.99,rep(Inf,2),.99,rep(Inf,2),.99),matrix.i=as.matrix(table.12.9))


### Chapter 13 ###

## Latent Class Models

# Table 13.1

# LCM for reader agreement

library(combinat)
table.13.1<-matrix(0,nc=7,nr=2^7)

i<-1
sapply(1:6, function(y) {
  nrows<-ncol(cols<-combn(1:7,y))
  sapply(1:nrows, function(x) {
    table.13.1[i,cols[,x]]<<-1
    i<<-i+1
  })
})

table.13.1[2^7,]<-rep(1,7)

table.13.1<-cbind(table.13.1,counts<-c(2,6,0,0,2,0,0,
                                       2,0,0,0,0,0,0,0,4,0,1,0,0,0,0, 0,0,0, 0,0, 0,
                                       0,0,2,0,1, 0,0,0,0, 0,0,0,0,0,0, 0,0,0,0, 0,0,0, 0,5,0, 0,0,0, 0,0,0, 0,0,0, 0, 
                                       0,0,0,0, 0,0,1, 0,7,0, 0,0,0,0,1, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 
                                       0,0,0,0,13, 0,0,2,0,1, 0,0,0,0,0, 0,0,0,0,0, 0,0,10,0,5,3, 0,0,34,16))

# remove zero counts
table.13.1<-table.13.1[table.13.1[,8]!=0,]

# expand table
table.13.1<-table.13.1[rep(1:nrow(table.13.1), table.13.1[,8]),]


library(e1071)
summary(res.lca<-lca(countpattern(table.13.1[,1:7]),k=2,niter=15))
summary(lca(countpattern(table.13.1[,1:7]),k=3,niter=15)) # unstable fit
summary(lca(countpattern(table.13.1[,1:7]),k=4,niter=15)) # unstable fit

summary(lca(table.13.1[,1:7],k=3,niter=15)) # unstable fit

# expected counts example
sum(counts)*(prod(res.lca$p[1,])*res.lca$classprob[1] + prod(res.lca$p[2,])*res.lca$classprob[2])


## package mmlcr

table.13.1.df<-data.frame(table.13.1[,1:7])
names(table.13.1.df)<-paste("p",1:7,sep="")
table.13.1.df<-sapply(table.13.1.df, as.factor)

table.13.1.df<-data.frame(table.13.1.df[,1:7],id=1:nrow(table.13.1.df))

library(mmlcr)

# 2-class model
res.mmlcr<-mmlcr(outer = ~ 1 | id, components=list(
  list(formula=p1~1, class= "multinomonce"),
  list(formula=p2~1, class="multinomonce"),
  list(formula=p3~1, class="multinomonce"),
  list(formula=p4~1, class="multinomonce"),
  list(formula=p5~1, class="multinomonce"),
  list(formula=p6~1, class="multinomonce"),
  list(formula=p7~1, class="multinomonce")), data=table.13.1.df, n.groups=2)


# 3-class model
res3.mmlcr<-mmlcr(outer = ~ 1 | id, components=list(
  list(formula=p1~1, class= "multinomonce"),
  list(formula=p2~1, class="multinomonce"),
  list(formula=p3~1, class="multinomonce"),
  list(formula=p4~1, class="multinomonce"),
  list(formula=p5~1, class="multinomonce"),
  list(formula=p6~1, class="multinomonce"),
  list(formula=p7~1, class="multinomonce")), data=table.13.1.df, n.groups=3)


# get fitted values
unique(sapply(res3.mmlcr$components,function(x) round(x$fitted-1,3)))
unique(sapply(res.mmlcr$components,function(x) round(x$fitted-1,3)))


## package flexmix

library(flexmix)

table.13.1.df<-data.frame(table.13.1[,1:7])
names(table.13.1.df)<-paste("p",1:7,sep="")

table.13.1.long<-reshape(table.13.1.df, varying=list(names(table.13.1.df)),direction="long", 
                         v.names="response", timevar="pathologist")

table.13.1.long$pathologist<-factor(table.13.1.long$pathologist)

fit1<-stepFlexmix(cbind(response,1-response)~pathologist|id, data=table.13.1.long, k=2,model=FLXglm(family="binomial"), nrep=5)
summary(fit1)
cluster(fit1)

flexmix(cbind(response,1-response)~pathologist|id, data=table.13.1.long, k=2,model=FLXglm(family="binomial"), nrep=5)

plot(fit1)

fit2<-stepFlexmix(cbind(response,1-response)~pathologist|id, data=table.13.1.long, k=3,model=FLXglm(family="binomial"), nrep=5)
summary(fit2)
cluster(fit2)


## capture-recapture using latent class analysis 

# using gllm  

i<-rep(1,128)
x<-as.integer(gl(2,64,128))-1
a<-as.integer(gl(2,32,128))-1
b<-as.integer(gl(2,16 ,128))-1
c<-as.integer(gl(2,8 ,128))-1
d<-as.integer(gl(2,4 ,128))-1
e<-as.integer(gl(2,2 ,128))-1
f<-as.integer(gl(2,1 ,128))-1     

X<-cbind(i,x,a,b,c,d,e,f,x*cbind(a,b,c,d,e,f))

colnames(X)<-c("Int","X","A","B","C","D","E","F","AX","BX","CX","DX","EX","FX")

counts<-c(3,6,0,5,1,0,0,3,2,3,0,0,1,0,0,4,2,3,1,0,1,0,0,1,0,0,0,0,0,0,0,4,1,1,1,2,0,2,0,4,0,3,0,1,0,2,0,2,0,1,0,1,0,1,0,1,1,1,0,0,0,1,2)


library(gllm)

func<-function(y,s,X,counts){
  emgllm(c(y,counts),s,X,maxit=1)$deviance
}

s<-c(1:64,1:64)

optim(par=c(2),func,control=list(fnscale=1,REPORT=1,trace=3, parscale=c(1)),lower=c(0),upper=c(100),method="L-BFGS-B",s=s,X=X,counts=counts)


# using likelihood function


X<-X[1:64,c("A","B","C","D","E","F")]

func<-function(y, X, counts) {
  
  n.s<-c(exp(y[14]),counts)
  
  z <- y[13]    
  x<-1-y[1:12]    
  
  res<-apply(X,1,function(w){
    exp(sum(log(w*y[1:6] + (1-w)*x[1:6])))*z + exp(sum(log(w*y[7:12] + (1-w)*x[7:12])))*(1-z)
  })
  
  xg0 <- n.s[n.s > 0]
  ll0 <- sum(xg0 * log(xg0/sum(n.s)))  # saturated likelihood
  
  ll<-n.s%*%log(res)
  
  -2 * (ll0 - ll)
  
}


temp2<-temp[rep(1:63,counts),]

library(e1071)
res<-lca(countpattern(temp2),k=2) # starting values

result<-optim(par=c(as.numeric(res$p), res$classprob[1] ,log(20)),func,
              control=list(fnscale=-1,trace=3,REPORT=1),lower=c(rep(0.001,13),0),upper=c(rep(.999,13),100),X=X,counts=counts, method="L-BFGS-B")


### Nonparametric Random effects models

## Logit models

library(npmlreg)
res<-allvc(response ~ gender + question, random = ~1|id,  
           family=binomial(link=logit), data=table.10.13b, k=2) 


## Mixing of Logistic Regressions

table.13.4<-data.frame(dose=seq(4.7,5.4,.1),n=c(55,49,60,55,53,53,51,50),dead=c(0,8,18,18,22,37,47,50))
table.13.4a<-rbind(data.frame(dose=rep(table.13.4$dose,table.13.4$dead),response=1),
                   data.frame(dose=rep(table.13.4$dose,table.13.4$n-table.13.4$dead),response=0))
#table.13.4a$id<-1:nrow(table.13.4a)

library(npmlreg)
res<-alldist(response~log(dose), random=~1,family=binomial(link=logit),k=2,data=table.13.4a)

# ordinary logistic fit using alldist
res2<-alldist(response~log(dose), random=~1,family=binomial(link=logit),k=1,data=table.13.4a)

# predictions
res3<-alldist(cbind(dead,n-dead)~log(dose), random=~1,family=binomial(link=logit),k=2,data=table.13.4)
ypred<-sort(predict.glmmNPML(res3,  type = "response"))
plot(ldose<-log(table.13.4$dose),table.13.4$dead/table.13.4$n,xlab="Log(dose)",ylab="P(death)",bty="L",axes=F)
axis(2)
axis(1, at=c(1.548,1.6,1.65,1.69,1.75), labels=as.character(c(1.55,1.6,1.65,1.7,1.75)))
lines(smooth.spline(ldose,ypred,spar=.05))

## Rasch Mixture Model

# Modeling rater agreement
options(contrasts=c("contr.sum", "contr.poly"))

library(npmlreg)
summary(fit.rm<-allvc(response~pathologist, random=~1|id,family=binomial(link=logit),k=3,data=table.13.1.long))


# log-linear model for capture-recapture

options(contrasts=c("contr.treatment", "contr.poly"))

# using repeated library
counts<-c(0,3,6,0,5,1,0,0,3,2,3,0,0,1,0,0,4,2,3,1,0,1,0,0,1,0,0,0,0,0,0,0,4,1,1,1,2,0,2,0,4,0,3,0,1,0,2,0,2,0,1,0,1,0,1,0,1,1,1,0,0,0,1,2)

reindex<-c(1, 5, 3, 7, 2, 6, 4, 8)
index<-outer(seq(0,56,by=8),reindex,"+")
index<-index[reindex,]
new.counts<-counts[as.numeric(index)]

library(repeated)

n<-6
eval(setup)

# mutual independence
(z0 <- glm(new.counts~(p1+p2+p3+p4+p5+p6), family=poisson, weights=rev(pw))) # give 000000 a case weight of 0
capture(z0,n)
z0$fit[1]+68

# two-factor 
(z1 <- glm(new.counts~(p1+p2+p3+p4+p5+p6)^2, family=poisson, weights=rev(pw)))
capture(z1,n)
z1$fit[1]+68

# exchangeable association
assoc<-choose(n=rowSums(cbind(p1,p2,p3,p4,p5,p6)),k=2)
(z1 <- glm(new.counts~p1+p2+p3+p4+p5+p6+assoc, family=poisson, weights=rev(pw)))
z1$fit[1] + 68


# using Rcapture

library(Rcapture)

fit.cap<-closedp(cbind(p1,p2,p3,p4,p5,p6,new.counts),dfreq=TRUE)

# profile LH CI
profileCI(cbind(p1,p2,p3,p4,p5,p6,new.counts), dfreq=TRUE, m="Mth", h="Darroch", a=2, mX=NULL, 
          mname="Customized model", neg=TRUE, alpha=0.05)


## marginal quasi-symmetry loglinear model 

table.10.13c<-table.10.13[,2:4]
table.10.13c<-as.data.frame(lapply(table.10.13c,function(x) as.numeric(x)-1))
symm<-as.factor(rowSums(table.10.13c))
table.10.13c<-data.frame(table.10.13c,count=table.10.13$count,gender=table.10.13$gender,symm=symm)

summary(glm(count~-1+question1+question2+question3+gender+symm, family=poisson, data=table.10.13c))

### Beta-binomial models

table.4.5<-read.table("teratology.txt", col.names=c("","group","litter.size","num.dead"))[,-1]
table.4.5$group<-as.factor(table.4.5$group)

group<-table.4.5$group

## Estimation using the bb likelihood

library(VGAM)
fit.bb<-vglm(cbind(num.dead,litter.size-num.dead) ~ group, betabinomial(zero=2,irho=.2),
             data=table.4.5, trace=TRUE)
coef(fit.bb, matrix=TRUE)
summary(fit.bb)
logit(-1.146, inverse=T)


library(gnlm)
require(VGAM)   # for logit function
mu <- function(p, linear) logit(linear,inverse=T)
#shape<-function(pshape) pshape)
#gnlr(cbind(table.4.5$num.dead,table.4.5$litter.size-table.4.5$num.dead), distribution="beta binomial",linear=~group,mu=mu,shape=shape,pmu=fit.ml$coefficients,pshape=.25)
gnlr(cbind(table.4.5$num.dead,table.4.5$litter.size-table.4.5$num.dead), distribution="beta binomial",linear=~group,mu=mu,pmu=coefficients(fit.bb)[c(1,3:5)],pshape=.25)


library(aod)
betabin(cbind(num.dead,litter.size-num.dead)~group,random=~1,data=table.4.5)

betabin(cbind(num.dead,litter.size-num.dead)~group,random=~group,data=table.4.5)    # different dispersion parameters by group


## quasi-LH 

library(aod)
quasibin(cbind(num.dead,litter.size-num.dead)~group,data=table.4.5) # QL(1)


# binomial ML

library(gnlm)
fit.ml<-bnlr(cbind(table.4.5$num.dead,table.4.5$litter.size-table.4.5$num.dead),link="logit",mu=~group,pmu=c(1,1,1,1))  

# figure 13.4
logit.inverse<-function(x) exp(x)/(1+exp(x))
n<-table.4.5$litter.size
Pi<-logit.inverse(fitted(fit.ml)/n)
r<-table.4.5$num.dead-n*Pi
std.pear.res<-r/sqrt(n*Pi*(1-Pi))

plot(n, std.pear.res, ylim=c(-4,4), ylab="Standardized residuals", xlab="Litter size",bty="L")


## Negative Binomial regression

homicide.fr<-read.table("homicide.txt", header=TRUE)
homicide.fr<-homicide.fr[,c(1,2,4)]

homicide.fr<-data.frame(freq=c(homicide.fr[,1],homicide.fr[,2]),response=rep(homicide.fr[,3],2), race=rep(c("white","black"),each=7))
homicide.fr$race<-factor(homicide.fr$race, levels=c("white","black"))

require(MASS)
fit.nb<-glm.nb(response~race, data=homicide.fr, weights=freq, link=log)
summary(fit.nb)


# predicted counts
unique(predict(fit.nb, type="response"))

round(dnbinom(0:6, size=fit.nb$theta, mu=.092, log = FALSE)*1149,1)
round(dnbinom(0:6, size=fit.nb$theta, mu=.522, log = FALSE)*159,1)


## Poisson Regression with Random Effects model

require(glmmML)

homicideA.fr<-data.frame(homicide.fr[rep(1:14,homicide.fr$freq),2:3],ID=factor(1:sum(homicide.fr$freq)))

options(contrasts=c("contr.treatment", "contr.poly"))

res.glmmML<-glmmML(response ~ race, cluster=ID,family=poisson, method="ghq", data=homicideA.fr)

# fitted marginal means
mus<-exp(res.glmmML$coefficients[1]+res.glmmML$coefficients[2]*(as.numeric(homicideA.fr$race)-1))
EY<-unique(mus*exp(.5*1.632^2))



require(lme4)

glmer(response ~ race + (1 | ID) , family = poisson(log), data = homicideA.fr,nAGQ=2,REML=F,verbose=T) # closest so far to SAS

require(MASS)
glmmPQL(response~race, random=~1|ID,data=homicideA.fr,family=poisson(log),verbose=T,start=c(-3.7,1.9))
