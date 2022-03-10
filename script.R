#Import data#
GunLawsRankedByState = read.csv("laws.csv")
statesdata = read.csv("statesdata.csv")
StatesGunData = cbind(statesdata, GunLawsRankedByState)


##Import Control variables##
GDPPerCapita = read.csv("usincome.csv")

ViolentCrimeRate = read.csv("crime.csv")

OpoidsPrescribedPerCapita = read.csv("opoidusage.csv")

EthanolConsumptionPerCapita = read.csv("alcoholconsumption.csv")

PercentageAA = read.csv("AApopulation.csv")

AffordableHousingShortage = read.csv("AffordableHousingShortage.csv")

SpendingPerStudent = read.csv("PublicEducationSpending.csv")

##Merge data frame to include control variables##
USGunData = cbind(StatesGunData, GDPPerCapita, ViolentCrimeRate, OpoidsPrescribedPerCapita, 
                  EthanolConsumptionPerCapita, PercentageAA,
                  AffordableHousingShortage, SpendingPerStudent)

#Regression1 without control variables#
R1 = lm(GunDeathsPerCapita ~ GunLawRanking, data = USGunData)
summary(R1)

#Regression2 with control variables#
R2 = lm(GunDeathsPerCapita ~ GunLawRanking + EthanolConsumptionPerCapita
        + OpoidsPrescribedPerCapita + AffordableHousingShortage
        + SpendingPerStudent + ViolentCrimeRate
        + PercentageAA + GDPPerCapita, data = USGunData)

summary(R2)

##Check for multicollinearity##
##Calculate the variance inflation factors of independent variables##
install.packages("car")
library(car)
vif(R2)

##Use stargazer package to report results##
install.packages("stargazer")
library(stargazer)
models = list(R1, R2)

stargazer(models, 
          title = "Association between firearm mortality and gun legistlation", 
          type = "text")

##Copy and paste the respective outputs below into latex editor for pdf results##
stargazer(R1)
stargazer(R2)
