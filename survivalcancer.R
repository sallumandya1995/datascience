
data <- read.csv("E:\\Datasets\\survivalcancerdata.csv")
library(survminer)
library(survival)
View(data)
attach(data)
str(data)
time <-  Months_from_diagnosis
event <-  Status
group <- Cell_size  # unemployment insurance can take 2 values 0 or 1 
# Descriptive statistics
summary(time)
summary(event)
# summary(X)
summary(group)
table(group)
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
ggsurvplot(kmsurvival, data=data, risk.table = TRUE)
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")
ggsurvplot(kmsurvival1, data=data, risk.table = TRUE)

