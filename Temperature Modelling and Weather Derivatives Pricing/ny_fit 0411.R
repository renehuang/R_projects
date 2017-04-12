
setwd('D:/Columbia 17spring/stoch in fin/proj')

ny <- read.csv("nyc_daily_2001_2016.csv")
# peru <- read.csv("Peru.csv")
# drops <-c("X")
# peru <-peru[, !(names(peru) %in% drops)]
colnames(ny)[1]<-"date"

head(ny)
# head(peru)

ny$date<-as.Date(ny$date,"%m/%d/%Y")
# peru$date<-as.Date(peru$date,"%m/%d/%Y")

nytemp<-as.numeric(as.character(ny$avg))
# perutemp<-as.numeric(as.character(peru$avg))

t<-ny$date



# for ny
##reference:http://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
# which(is.na(nytemp))

nytemp[4046] = (nytemp[4045]+nytemp[4048])/2
nytemp[4047] = nytemp[4046]

# calculate per
ssp <- spectrum(nytemp)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]


# plot to check the difference
rg <- diff(range(nytemp, na.rm = TRUE), na.rm=TRUE)
plot(nytemp~t,ylim=c(min(nytemp,na.rm=TRUE)-0.1*rg,max(nytemp,na.rm=TRUE)+0.1*rg),cex=0.5)

# including the trend term t
# fit linear model
dates = 1:5844
reslm2 <- lm(nytemp ~ dates+sin(2*pi/per*dates)+cos(2*pi/per*dates))#+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm2)

t_mean<-function(t){
  t_mean <- 54.2310 + 0.0004689*t +12.8961*sin(2*pi/per * t) - 9.6925*cos(2*pi/per * t) 
  return (t_mean)
}

fitmean <- t_mean(dates)
lines(fitmean~t,col='red',lwd=4)    # solid red line is periodic with second harmonic




# fit non-linear model
W = 2*pi/per
reslm3 <- nls(nytemp ~ cons+A*dates+B*sin(W*dates)+C*cos(W*dates), start=list(cons=54.2312, A=0.0004681, B=12.900,W=W,C=-9.6935))#+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm3)


# the fitted model could be written into: T = 54.92 + 0.0003318*t -8.461*sin(0.0172 * t) - 20.029*cos(0.0172 * t) + sigma1 * W(t)
t_mean2<-function(t){
  t_mean2 <- 54.92 + 0.0003336*t -8.459*sin(0.0172 * t) - 20.28*cos(0.0172 * t) 
  return (t_mean2)
}
fitmean2 <- t_mean2(dates)
lines(fitmean2~t,col='blue',lwd=4)  


# sigma
sigma1=sd(diff(nytemp)) #using difference of y, the one on the paper
# sigma2=sd(nytemp-fitmean) #approximate residual standard error




## RUN SIMULATION !!!
tem0 = 25.40
v = sigma1

# number of simulation paths
N = 100

orit = as.Date("2001-01-01",format="%Y-%m-%d")
startt = as.Date("2017-03-01",format="%Y-%m-%d")
finalt = as.Date("2017-03-31",format="%Y-%m-%d")
# set the step to be 1
dt = 1
time1 = as.numeric(startt-orit)+1
time2= as.numeric(finalt-orit)+1


df<- data.frame(matrix(NA, nrow = time2-time1+3, ncol= N))

df[1,]= t_mean2(time1)
#HDD
df[time2-time1+2,] = max(65-t_mean2(time1),0)
#CDD
df[time2-time1+3,] = max(t_mean2(time1)-65,0)


for(a in 1:N){
  for(b in 1: (time2-time1)){
      dw = rnorm(1)*1;
      temp = df[b,a]
      df[b+1,a] = temp+(t_mean2(b+time1)-t_mean2(b+time1-1))+v*dw
      # HDD
      df[time2-time1+2,a] = df[time2-time1+2,a] + max(65-df[b+1,a],0)
      # CDD
      df[time2-time1+3,a] = df[time2-time1+3,a] + max(df[b+1,a]-65,0)
  }
}

finalsim<- rowMeans(df[1:(time2-time1+1),])
# seasonal HDD Nov to Mar
finalHDD <- rowMeans(df[time2-time1+2,])
# seasonal CDD May to Sep
finalCDD<- rowMeans(df[time2-time1+3,])


# the prediction line
lines(finalsim~seq(from=startt,to=finalt,by='days'),col='black',lwd=4) 

