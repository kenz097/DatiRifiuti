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


## unilateral and bilateral test
# left unilateral test
p0=0.14
alpha<-0.05
qnorm(1-alpha,mean=0,sd=1)
n
(medCamp-kparam*p0)/sqrt((kparam*p0*(1-p0))/n)

#right unilateral test
p0=0.12
alpha<-0.05
qnorm(alpha,mean=0,sd=1)
n
(medCamp-kparam*p0)/sqrt((kparam*p0*(1-p0))/n)

value<-c(68,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,93,94,96,105)
#chi square
data
n
freq<-table(data)
freq
length(freq)
p<-numeric(4)
for(i in 1:4)
  p[i]<-qbinom(0.2*i,size=600,prob=stima50)
p
freq

temp_value<-pbinom(76,size=600,prob=stima50)
temp_value

temp_value1<-dbinom(77, size = 600, prob=stima50);
temp_value1<-temp_value1+dbinom(78, size = 600, prob=stima50);
temp_value1<-temp_value1+dbinom(79, size = 600, prob=stima50);
temp_value1<-temp_value1+dbinom(80, size = 600, prob=stima50);
temp_value1<-temp_value1+dbinom(81, size = 600, prob=stima50);
temp_value1

temp_value2<-dbinom(82, size = 600, prob=stima50);
temp_value2<-temp_value2+dbinom(83, size = 600, prob=stima50);
temp_value2<-temp_value2+dbinom(84, size = 600, prob=stima50);
temp_value2<-temp_value2+dbinom(85, size = 600, prob=stima50);
temp_value2

temp_value3<-dbinom(86, size = 600, prob=stima50);
temp_value3<-temp_value3+dbinom(87, size = 600, prob=stima50);
temp_value3<-temp_value3+dbinom(88, size = 600, prob=stima50);
temp_value3<-temp_value3+dbinom(89, size = 600, prob=stima50);
temp_value3<-temp_value3+dbinom(90, size = 600, prob=stima50);
temp_value3

temp_value4<-pbinom(90,size=600,prob=stima50,lower.tail = FALSE)
temp_value4


temp_value+temp_value1+temp_value2+temp_value3+temp_value4
totale<-numeric(5)
totale[1]<-temp_value
totale[2]<-temp_value1
totale[3]<-temp_value2
totale[4]<-temp_value3
totale[5]<-temp_value4

min(primopb,temp_value,temp_value2,temp_value3,temp_value4)

50*temp_value4

r<-5
nint<-numeric(r)
nint[1]<-length(which(data<p[1]))
nint[2]<-length(which((data>=p[1])&(data<p[2])))
nint[3]<-length(which((data>=p[2])&(data<p[3])))
nint[4]<-length(which((data>=p[3])&(data<p[4])))
nint[5]<-length(which(data>=p[4]))
nint
sum(nint)

chi2<-sum(((nint-n*totale)/sqrt(n*totale))^2)
chi2

r<-5
k<-1
alpha<-0.05
qchisq(alpha/2,df=r-k-1)
qchisq(1-alpha/2,df=r-k-1)

