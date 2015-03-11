library(prospectr)
data(NIRsoil)
class(NIRsoil)    #  data.frame
names(NIRsoil)    # "Nt"    "Ciso"  "CEC"   "train" "spc" 
########  Adding NOISE to the spectra ###################################
head(NIRsoil$spc) # De 1100 a 2498 nm, cada 2nm
str(NIRsoil)
X<-as.matrix(NIRsoil$spc)
length(X)                   #.........577500
noise=rnorm(length(X),0,0.001)             #(Generate noise)
length(noise)                              #.........577500
noise<-matrix(noise,nrow=825,ncol=700)     #(Constructing the Matrix)
dim(noise)
plot(colnames(X),noise[1,],type="l",xlab="wavelength",ylab="absorbance")
X.noisy<-X + noise
par(mfrow=c(2,1),ps=14)
plot(colnames(X),X[1,],type="l",xlab="wavelength",ylab="absorbance",col="blue")
plot(colnames(X),X.noisy[1,],type="l",xlab="wavelength",ylab="absorbance",col="red") 
########  Subscans and Scans  #####################################
#We take one spectrum and repeated 32 times
subscan<-X[1,]
length(subscan)
subscans<-rep(subscan,32)
subscans<-matrix(subscans,nrow=32,byrow=TRUE)
wavelength<-seq(1100,2498,by=2) 
matplot(wavelength,t(subscans),type="l",xlab="wavelength",ylab="absorbance")
#Now we add different spectra noise to each
noise32<-noise[1:32,]
subscans<-subscans + noise32
par(mfrow=c(2,1),ps=14)
matplot(wavelength,t(subscans),type="l",xlab="wavelength",ylab="absorbance")
subscan.avg<-as.matrix(colMeans(subscans))
matplot(wavelength,subscan.avg,type="l",xlab="wavelength",ylab="absorbance")
########### Moving Average  ##########################################################
library(prospectr)
X.movav<-movav(X.noisy,11)  # we truncate the spectra 5 data points an the beginning
                            # and 5 data points at the end.
plot(colnames(X),X.noisy[1,],type="l",col="red",xlim=c(1100,2500),ylim=c(0.28,0.38),
     xlab="wavelength",ylab="absorbance") 
par(new=TRUE)
plot(colnames(X.movav),X.movav[1,],type="l",lwd=2,xlim=c(1100,2500),ylim=c(0.28,0.38),
     xlab="wavelength",ylab="absorbance",col="blue")
legend("topleft",legend=c("moving average","raw"),lty=c(1,1),col=1:2)
