library(jpeg)
img<-readJPEG("C:\Users\Asus\Downloads/group15.jpg")
imgnew = img
plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)))
rasterImage(img,0,0,nrow(img),ncol(img))
nrow(img)
for(k in 1:400){
  mean = mean(imgnew[k,,])
  st_dev = sd(imgnew[k,,])
  upper  <-mean+(3*st_dev)
  lower <- mean-(3*st_dev)
  for(t in 1:400){
    if(imgnew[k,t,] < lower || imgnew[k,t,] > upper){
      imgnew[k,t,] = 0
    }
  }
}
plot(1:400, img[400,,1],ylim=c(-0.1,1.2),type="l")
abline(upper,0, col="blue")
abline(mean,0, col="red")
abline(lower,0, col="blue")

plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)))
rasterImage(imgnew,0,0,nrow(imgnew),ncol(imgnew))
ncol(img)
for(k in 1:400){
  mean = mean(imgnew[,k,])
  st_dev = sd(imgnew[,k,])
  upper  <-mean + 3*st_dev
  lower <- mean - 3*st_dev
  for(t in 1:400){
    if(imgnew[t,k,] < lower || imgnew[t,k,] > upper){
      imgnew[t,k,] = 0
    }
  }
}
plot(1:400, img[,400,1],ylim=c(-0.1,1.2),type="l")
abline(upper,0, col="blue")
abline(mean,0, col="red")
abline(lower,0, col="blue")
plot(NA,xlim=c(0,nrow(imgnew)),ylim=c(0,ncol(imgnew)))
rasterImage(imgnew,0,0,nrow(imgnew),ncol(imgnew))
library(jpeg)
img<-readJPEG("~/Desktop/group15.jpeg")
img_c<-img[,,1] 
hist(img_c)
str(img_c)
data_row<-matrix(NA,1,2601)
big_data<-matrix(NA,122500,2601)
for(i in 1:350){
  for(j in 1:350){
    patch<-img_c[i:(i+50),j:(j+50)]
    data_row<-as.vector(t(patch))
    big_data[(i-1)*350+j,1:2601]<-data_row[1:2601]
    
  }
}
big_data<-as.data.frame(big_data)
colnames(big_data)
center_pixel<-big_data[,1301]
big_data<-big_data[,-1301]
big_data<-cbind(center_pixel,big_data)
regmodel<-lm(center_pixel~.,data=big_data)
summary(regmodel)
plot(regmodel)
predicted_values<-predict(regmodel)
regmodel
predicted_values
plot(regmodel$residuals)
library("fitdistrplus")
fit_res <- fitdist (regmodel$residuals, distr = "norm")
plot(fit_res)
plot(regmodel$residuals, ylim = c(-1,1), main = "Residual Control Chart", xlab = "Period", ylab = "Residuals")
lines(regmodel$residuals)
x_bar <- mean(regmodel$residuals)
st_dev <- sd(regmodel$residuals)
CL = x_bar
UCL = x_bar + 3*st_dev
LCL = x_bar - 3*st-dev
abline(LCL,0,col = "red")
abline(CL,0,col = "blue")
abline(UCL,0,col = "red")
for(i in 1:122500){
  if(regmodel$residuals[i]>UCL){
    if((i%%350)==0){
      img_c[((i/350)+25),((i-1)%%350)+26]=0
    }
    else{
      img_c[((i-(i%%350))/350)+26,((i-1)%%350)+26]=0
    }
  }
  else if(regmodel$residuals[i]<LCL){
    if((i%%350)==0){
      img_c[((i/350)+25),((i-1)%%350)+26]=0
    }
    else{
      img_c[((i-(i%%350))/350)+26,((i-1)%%350)+26]=0
    }
  }
}
plot(1:400, type='n')
rasterImage(img_c, 1,1,400,400)
