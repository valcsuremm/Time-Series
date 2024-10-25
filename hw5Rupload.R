## Problem 2.16
#loading necessary packages
install.packages("forecast")
install.packages("ggplot2")

# Load libraries
library(forecast)
library(ggplot2)

data ="sunspots.txt"
getwd()
# Set the working directory to where your .txt file is
setwd("/Users/valeriepersonal/desktop")


sunspots=read.table(data,sep="")
sunspots=as.matrix(sunspots); n.d=length(sunspots)

#subtracting the mean from the data
sunspots_subtract = sunspots - mean(sunspots)
sunspots_subtract

# ACF plot
# lag max - "up to lag 40"
acf(sunspots_subtract, lag.max = 40, main = "Sample ACF of Sunspot Data", col = "purple") 
#automatic boundaries

# PACF plot
pacf(sunspots_subtract, lag.max = 40, main = "Sample PACF of Sunspot Data", col = "darkturquoise")

#we need to fit an AR model
#arima function from the R help section
model = arima(sunspots_subtract, order = c(2, 0, 0)) #ar order 2, ma order 0 (middle value irrelevant)

model

#based on the model summary, phi(1) = 1.4076 and phi(2) = -.7128
model$sigma2
#sigma squared (variance) is 227.9284

# Compare the model and sample ACF and PACF... Print the graphs
model_acf = ARMAacf(ar = c(1.4076, -.7128), lag.max = 40)
model_pacf = c(1, ARMAacf(ar = c(1.4076, -.7128), lag.max = 40, pacf = TRUE)[-1])


summary(model_acf)
summary(model_pacf)

# ACF comparison
plot(acf(sunspots_subtract, plot = FALSE), main = "Sample ACF with Model ACF (Pink)") #actual
lines(0:40, model_acf, col = "pink3", lty = 2, lwd = 2) #fitted model!

# PACF comparison
plot(pacf(sunspots_subtract, plot = FALSE), main = "Sample PACF with Model PACF (Purple)")
lines(0:40, model_pacf, col = "purple3", lty = 2, lwd = 7)
model_pacf #values are so small we don't get the curved line

#the fitted model for Sample ACF seems to be a better fit as opposed to the fitted model for Sample PACF\

## Problem 3.2

parameters_ar = c(1.8, .81)
h=20 #sigma2h=20 #lag = 20
par(mfrow = c(1, 2)) #put side by side
acf_AR = ARMAacf(ar = parameters_ar, lag.max = h, pacf = FALSE) #acf
pacf_AR = ARMAacf(ar = parameters_ar, lag.max = h, pacf = TRUE) #pacf

acf_AR
pacf_AR
#plotting ACF
plot(0:h, acf_AR, type = "h", main = "ACF of AR(2) Process", ylab = "ACF", xlab = "Lag")
#something might be wrong with my ACF function

plot(0:(h-1), pacf_AR, type = "h", main = "PACF of AR(2) Process", ylab = "ACF", xlab = "Lag")

# Problem 3.9, b
getwd()
# Set the working directory to where your .txt file is
setwd("/Users/valeriepersonal/desktop")

getwd()

deaths=read.table("deaths.txt",sep="")
deaths=as.matrix(deaths); n.d=length(deaths)
#deaths

deaths_vector = as.vector(deaths) #easier to manipulate

delta_12 = diff(deaths_vector, lag = 12)

# computation of sample mean
sample_mean = mean(delta_12)

cat("The sample mean is: ", sample_mean, "\n")

# sample ACVF
autocovariances = numeric(21) #ACVF 0 to 20, 21

for (h in 0:20) { #r for loop
  autocovariances[h + 1] = mean(delta_12[1:(length(delta_12) - h)] * delta_12[(h + 1):length(delta_12)]) - sample_mean^2
  cat("\n The sample autocovariance at lag ", h, " is ", autocovariances[h+1])
}


print(autocovariances)

#problem 3.9, c
#lag 1, 11, and 12:
auto_1 = autocovariances[1]
auto_11 = autocovariances[11] #shouldn't this be 0?
auto_12 =autocovariances[12]
#lag 0
variance = var(delta_12) 
variance

#autocovariance for lag 1:
#theta_1^2 * variance = autocovariance 1, need to find theta
sqrt(auto_1/variance) #= theta_1

#theta_11=0
auto_12
#theta_12^2 =
-sqrt(10856.24/288714.5)

