#Prepare a prediction model to Predict the price of the computer


Computer_data <- read.csv(file.choose())
View(Computer_data)
class(Computer_data)
 # To Transform the data from Character to Numeric
library(plyr)
 Computer_data1 <- Computer_data
Computer_data1$cd <- as.numeric(revalue(Computer_data1$cd,c("yes"=1, "no"=0)))
Computer_data1$multi <- as.numeric(revalue(Computer_data1$multi,c("yes"=1, "no"=0)))
Computer_data1$premium <- as.numeric(revalue(Computer_data1$premium,c("yes"=1, "no"=0)))
View(Computer_data1)
class(Computer_data1)
 attach(Computer_data1)

# Basically to avoid reference of Data Set  

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(Computer_data1)
 plot(speed, price)


plot(hd, price)


plot(ram, price)


plot(screen, price)


plot(cd, price)


plot(multi, price)


plot(premium, price)


plot(ads, price)


plot(trend, price)


windows()
# 7. Find the correlation between Output (Price) & inputs (speed,hd,ram,screen,cd,multi,premium,ads,trend) - SCATTER DIAGRAM
pairs(Computer_data1)


# 8. Correlation coefficient - Strength & Direction of correlation
cor(Computer_data1)
 # The Linear Model of interest
Model.Computer_data1 <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(Model.Computer_data1)  # Adjusted R2 Value - 0.7752
  
### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Computer_data1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


### Partial Correlation matrix - Pure correlation between the variables
 library(corpcor)
cor2pcor(cor(Computer_data1))
 library(mvinfluence)
 library(car)

# It is better to delete a single observation rather than entire variable to get rid of collinearity problem
# Deletion Diagnostics for identifying influential variable
influence.measures(Model.Computer_data1)
 
influenceIndexPlot(Model.Computer_data1, id.n=3) # Index Plots of the influence measures
influencePlot(Model.Computer_data1, id.n=3) # A user friendly representation of the above
 ## Regression after deleting the 49th and 50th observation, which is influential observation

# Logarthimic Transformation 
Model.Computer_dataLog <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_dataLog)      ## Adjusted R2 Value - 0.7441
 confint(Model.Computer_dataLog,level=0.95)
 predict(Model.Computer_dataLog,interval="predict")
  
Model.Computer_data2<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                         data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_data2) # Adjusted R2 Value is 0.7774
 # Exponential Transformation :
Model.Computer_exp<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                       data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_exp)  #Adjusted R2 Value is 0.7833 
# Quad Model
Model.Computer_Quad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                            +cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)
                          +ads+I(ads^2)+trend+I(trend^2),data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_Quad)  #Adjusted R2 value is 0.8049
 confint(Model.Computer_Quad,level=0.95)
 predict(Model.Computer_Quad,interval="predict")
  
# Poly Modal
Model.Computer_Poly <- lm(price~speed+I(speed^2)+I(speed^3)+
                            hd+I(hd^2)+I(hd^3)+
                            ram+I(ram^2)+I(ram^3)+
                            screen+I(screen^2)+I(screen^3)+
                            cd+I(cd^2)+I(cd^3)+
                            multi+I(multi^2)+I(multi^3)+
                            premium+I(premium^2)+I(premium^3)+
                            ads+I(ads^2)+I(ads^3)+
                            trend+I(trend^2)+I(trend^3),data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_Poly) #Adjusted R Square Value is 0.813
 ### Variance Inflation Factors is a formal way to check for collinearity
# vif(Model.Computer_Poly)  # VIF is > 10 => collinearity


# avPlots(corolla.price1, id.n=2, id.cex=0.7) # Added Variable Plots

# Final Model
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=Computer_data1[-c(1441,1701),])

summary(FinalModel) #Adjusted R2 Value = 0.813 
 Profit_Predict <- predict(FinalModel)
View(Profit_Predict)

finplot <- Computer_data1[-c(1441,1701),]
View(finplot)

plot1 <- cbind(finplot$price, Profit_Predict)
pairs(plot1)


Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profit_Predict)
 pairs(Final)


View(Final)

# Evaluate model LINE assumptions
plot(FinalModel)


# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot((FinalModel),id.n=5) # QQ plots of studentized residuals, helps identify outliers


## [1]  20 994
library("MASS")
stepAIC(Model.Computer_dataLog) # backward
 