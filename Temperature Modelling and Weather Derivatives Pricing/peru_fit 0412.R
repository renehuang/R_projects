setwd('D:/Columbia 17spring/stoch in fin/proj')
#data <- read.csv("nyc_daily_2001_2016.csv")

data<-read.csv("Peru.csv")
y<-data[,3]
y<-as.numeric(as.character(y))
#y<-y[!is.na(y)]
t<-1:length(y)
data$date<-as.Date(data$date,"%m/%d/%Y")
date<-data$date

##reference:http://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
ssp <- spectrum(y)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
summary(reslm)

rg <- diff(range(y))
plot(y~date,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg),cex=0.2)
lines(fitted(reslm)~date,col=4,lty=10,lwd=4)   # dashed blue line is sin fit

# for general model
# linear fit
reslm2 <- lm(y ~ t+sin(2*pi/per*t)+cos(2*pi/per*t))#+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm2)
lines(fitted(reslm2)~date,col='red',lwd=4)    # solid red line is periodic with second harmonic

W = 2*pi/per
reslm_1 <- nls(y ~ cons+A*t+B*sin(W*t)+C*cos(W*t), start=list(cons=66.13, A=0.0004943, B=-2.229,W=W,C=4.333))#+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm_1)
lines(fitted(reslm_1)~date,col='blue',lwd=4)   
fitmean2 <- fitted(reslm_1)

# sigma
sigma1=sd(diff(y)) #using difference of y, the one on the paper
sigma2=sd(y-fitted(reslm2)) #approximate residual standard error



#the condition that trigers El Nino
#El Nino 
for (i in 366:(length(y)-90)){
diff=mean(y[i:(i+30)])-mean(y[(i-365):(i-365+30)])
diff2=mean(y[(i+30):(i+60)])-mean(y[(i+30-365):(i-365+60)])
diff3=mean(y[(i+60):(i+90)])-mean(y[(i+60-365):(i-365+90)])
if (diff>0.1){
  if (diff2>0.1){
    if (diff3>0.1){
      {print(date[i])}
      print(i)}
  }}}


#get the range of El Nino days
#i is from 4906 to 5515
y2<-y[4906:5515]
t2<-4906:5515
date2<-data$date[4906:5515]
ssp <- spectrum(y2)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
#reslm3 <- lm(y2 ~ sin(2*pi/per*t2)+cos(2*pi/per*t2))
#summary(reslm3)


# to seperately model for El Nino part
rg <- diff(range(y2))
plot(y2~date2,ylim=c(min(y2)-0.1*rg,max(y2)+0.1*rg),cex=0.5)
lines(fitted(reslm3)~date2,col=4,lty=10,lwd=4)   # dashed blue line is sin fit


# linear fit
reslm4 <- lm(y2 ~ t2+sin(2*pi/per*t2)+cos(2*pi/per*t2))#+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm4)
lines(fitted(reslm4)~date2,col="red",lwd=4) 

# non-linear fit
reslm_2 <- nls(y2 ~ cons+A*t2+B*sin(W*t2)+C*cos(W*t2), start=list(cons=38.7837446, A=0.0058093, B=-1.7693087,W=W,C=-4.6415275))#+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm_2)
lines(fitted(reslm_2)~date2,col='blue',lwd=4)   
fitmean2 <- fitted(reslm_2)

# sigma
sigma3=sd(diff(y2)) #using difference of y, the one on the paper
sigma4=sd(y2-fitted(reslm_2)) #approximate residual standard error

