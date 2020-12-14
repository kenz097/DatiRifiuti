# Dungate Stats
 
  # ALERT

  # Values generated, do not execute one more time 
  # Use pre-filled array!!!

# Values generation

##########################################
# gvRaw <- rbinom(50, 600, 0.14)
# gvRaw
##########################################

data = c(84,  79,  76,  77,  75,  87, 84,  90,  79,  86,  91,  84,  83,  80,
         81,  85,  82,  84,  81,  83,  78, 105,  81,  68,  88,  73,  76,  86,
         91,  86,  94,  77,  91,  87,  96,  85,  85,  80,  82,  76,  79,  74,
         78,  73,  85,  81,  93,  89,  77,  82)
data

#freq section
frequences = (table(data)/length(data))
sort(frequences, decreasing = FALSE)

  # stima puntuale con 10 items
  subdata <- data[0:10]
  stima10 = mean(subdata)/600
  stima10
  
  # stima puntuale con 30 items
  subdata <- data[0:30]
  stima30 = mean(subdata)/600
  stima30
  
  # stima puntuale con 50 items
  subdata <- data[0:50]
  stima50 = mean(subdata)/600
  stima50

# stima intervallare

numberOfElements = 50
kparam = 600
alpha <- 1 - 0.95
zalpha <- qnorm(1-alpha/2, mean = 0, sd = 1)
medCamp <- sum(data)/numberOfElements

a2 <- kparam * (numberOfElements * kparam + zalpha^2)
a1 <- -kparam * (2 * numberOfElements * medCamp + zalpha^2) 
a0 <- numberOfElements * medCamp^2
polyroot(c(a0, a1, a2))

###

    # KENZUCCIO

###

#confidence interval estimate with known variance
#population
data
#media
media<-mean(data)
media
#alpha is a value between 0 and 1
alpha<-1-0.95
#calculate confidence interval
q<-qnorm(1-alpha/2,mean=0,sd=1)
#length data
n<-length(data)
#confidence interval estimate with known variance
media-q*8/sqrt(n)
media+q*8/sqrt(n)

#confidence interval estimate with not known variance
data
media
#sample standard deviation
devs<-sd(data)
devs
#alpha
alpha<-1-0.99
n
#quantity
quant<-qt(1-alpha/2,df=n-1)
quant
#confidence interval estimate with not known variance
media-quant*devs/sqrt(n)
media+quant*devs/sqrt(n)

#confidence interval estimate with known mean
data
media
#mu
mu<-70
#variance
varia<-var(data)
varia
#alpha
alpha<-1-0.95
#confidence interval estimate with known mean
qchi<-qchisq(alpha/2,df=n)
qchi
qchi2<-qchisq(1-alpha/2,df=n)
qchi2
((n-1)*varia+n*(media-mu)**2)/qchi2
((n-1)*varia+n*(media-mu)**2)/qchi

#confidence interval estimate with not known mean
data
media
varia
alpha<-1-0.95
qchis1<-qchisq(alpha/2,df=n-1)
qchis1
qchis2<-qchisq(1-alpha/2,df=n-1)
qchis2
(n-1)*varia/qchis2
(n-1)*varia/qchis1




## test unilateral and bilateral
p0=0.12
alpha<-0.05
qnorm(1-alpha/2,mean=0,sd=1)
n
(medCamp-kparam*p0)/sqrt((kparam*p0*(1-p0))/n)






