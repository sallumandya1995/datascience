install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
## Installing package into 'C:/Users/tswaminathan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
## package 'rmarkdown' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
##  C:\Users\tswaminathan\AppData\Local\Temp\Rtmpu4w5f3\downloaded_packages
install.packages("forecast",repos = "http://cran.us.r-project.org")
## Installing package into 'C:/Users/tswaminathan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
## package 'forecast' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
##  C:\Users\tswaminathan\AppData\Local\Temp\Rtmpu4w5f3\downloaded_packages
install.packages("fpp",repos = "http://cran.us.r-project.org")



## The downloaded binary packages are in
##  C:\Users\tswaminathan\AppData\Local\Temp\Rtmpu4w5f3\downloaded_packages
install.packages("smooth",repos = "http://cran.us.r-project.org")
## Installing package into 'C:/Users/tswaminathan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
## package 'smooth' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
##  C:\Users\tswaminathan\AppData\Local\Temp\Rtmpu4w5f3\downloaded_packages
install.packages("readxl",repos = "http://cran.us.r-project.org")






library(ggplot2) #data visualization
library(forecast)
library(fpp)
library(dplyr) #data manipulation
library(ISLR) #for the datase
library(smooth)


train=read.csv(choose.files())
View(train)
str(train)

summary(train)
colnames(train)

ggplot(data = train, mapping = aes(x = PLAN_MONTH , y =PLAN_YEAR)) + 
  geom_point()

plot(train$TARGET_IN_EA,train$ACH_IN_EA)
attach(train)
pairs(train)
plot(TARGET_IN_EA,ACH_IN_EA)
SLSMAN_CD



