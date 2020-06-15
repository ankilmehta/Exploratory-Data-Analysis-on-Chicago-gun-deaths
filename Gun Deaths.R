gun_deaths <- read.csv("C:/Users/ankil/OneDrive - University of Illinois at Chicago/uic/Fall Semester/Data Mining/Assignment/HW1/gun_deaths.csv")
View(gun_deaths)
nrow(gun_deaths)
colSums(is.na(gun_deaths))
str(gun_deaths)
gun_deaths$police <- as.factor(gun_deaths$police)
gun_deaths$month <- as.factor(gun_deaths$month)
gun_deaths$year <- as.factor(gun_deaths$year)
gun_deaths$id <- as.character(gun_deaths$id)
str(gun_deaths)

View(gun_deaths)

options(scipen=999)

# 1. a)

install.packages("knitr")
library(knitr)

DPM <- gun_deaths["month"]
DPMtable <- table(DPM)
kable(DPMtable, col.names = c("Month","Deaths"))

# b)

DPM2 <- DPM
DPM2$month <- factor(ifelse(DPM$month=="1","Jan",ifelse(DPM$month=="2","Feb",ifelse(DPM$month=="3","March",ifelse(DPM$month=="4","April",ifelse(DPM$month=="5","May",ifelse(DPM$month=="6","June",ifelse(DPM$month=="7","July",ifelse(DPM$month=="8","Aug",ifelse(DPM$month=="9","Sept",ifelse(DPM$month=="10","Oct",ifelse(DPM$month=="11","Nov","Dec"))))))))))))
DPMtable2 <- table(DPM2)
barplot(DPMtable2, main = "Gun Deaths per Month in America 2012-2014",
        xlab = "Month", ylab = "Deaths", ylim = c(0,10000), col = "red")
box()

# c)

IntentTab <- table(gun_deaths$intent)
barplot(IntentTab[order(IntentTab, decreasing = T)], main = "Gun Deaths by Intent in America 2012-2014",
        xlab = "Intent", ylab = "Deaths", col = "red", ylim = c(0,70000))
box()

# d)

boxplot(gun_deaths$age~gun_deaths$sex, col = "lightblue", ylab= "Age", xlab="Sex",
        main= "Age of Gun Death Victims by Sex")
FemaleVictims <- gun_deaths[gun_deaths$sex == "F",]
FemaleVictims2 <- na.omit(FemaleVictims$age)
mean(FemaleVictims2)
# Average age of female gun death victims is 43.7.

# e)

levels(gun_deaths$education)
WhiteMale2012 <- gun_deaths[gun_deaths$year == "2012" & gun_deaths$sex == "M",]
WhiteMale2012AtLeastHS <- WhiteMale2012[WhiteMale2012$education == "BA+" | WhiteMale2012$education == "HS/GED" |
                           WhiteMale2012$education == "Some college",]
nrow(WhiteMale2012AtLeastHS)
# 22153 white males with at least a high school education were killed by guns in 2012.

# f)

str(gun_deaths)
gun_deaths$DeathsbySeason <- factor(ifelse(gun_deaths$month == "1" | gun_deaths$month == "2" | gun_deaths$month == "3", "Winter",
                                ifelse(gun_deaths$month == "4" | gun_deaths$month == "5" | gun_deaths$month == "6", "Spring",
                                       ifelse(gun_deaths$month == "7" | gun_deaths$month == "8" | gun_deaths$month == "9", "Summer", "Fall"))))
table(gun_deaths$DeathsbySeason)
# Summer had the most gun deaths.

# g)

table(gun_deaths$intent[gun_deaths$race=="White"])
table(gun_deaths$intent[gun_deaths$race=="Black"])
table(gun_deaths$intent[gun_deaths$race=="Hispanic"])

# Whites are more likely to die by suicide. Blacks are more likely to die
# by homicide. Hispanics are more likely to die by homicide.

# h)

NoPolice <- gun_deaths[gun_deaths$police == "0",]
YesPolice <- gun_deaths[gun_deaths$police == "1",]

prop.table(table(YesPolice$race))
prop.table(table(NoPolice$race))

# Whites have the highest percentage of gun deaths whether or not the police
# were involved.

par(mfrow=c(1,2))
YesPoliceSeasonTab <- prop.table(table(YesPolice$DeathsbySeason))
barplot(YesPoliceSeasonTab, ylim = c(0,.3))
NoPoliceSeasonTab <- prop.table(table(NoPolice$DeathsbySeason))
barplot(NoPoliceSeasonTab, ylim = c(0,.3))

# When police are involved, spring and summer have proportionally more deaths than
# fall and winter relative to when they are not involved.


boxplot(YesPolice$age~YesPolice$sex)
boxplot(NoPolice$age~NoPolice$sex)

YesPoliceMale <- YesPolice[YesPolice$sex == "M",]
NoPoliceMale <- NoPolice[NoPolice$sex == "F",]

YesPoliceMale2 <- na.omit(YesPoliceMale$age)
NoPoliceMale2 <- na.omit(NoPoliceMale$age)

mean(YesPoliceMale2)
mean(NoPoliceMale2)

# When police are involved in gun deaths, the average age of males is about
# seven years younger than when they are not involved. Females are much
# closer in age when comparing the two.

# Overall, police involved gun deaths are not significantly different than
# other gun deaths.


