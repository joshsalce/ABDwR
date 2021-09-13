install.packages("retrosheet")
require(retrosheet)

install.packages("Lahman")
require(Lahman)
require("dplyr")

library(plyr)

install.packages("ggplot")
require(ggplot2)

#barplot(table(), xlab="", ylab="", main="")
#pie(table())

#windows(width=7, height=3.5) 
#stripchart(, method="jitter", pch=1,
#           xlab="")

#dotchart(table(), labels=, xlab="")

#hist(, xlab="", main="", breaks=)

#par(plt=c(.2, .94, .145, .883)) 
#stripchart( ~ , data=,
 #          method="jitter", pch=1, las=2)

#par(plt=c(.2, .94, .145, .883)) 
#boxplot( ~ , data=, las=2,
  #         horizontal=TRUE, xlab="")

#1
hofpitching <- read.csv(file.choose())
hofpitching$BF.group <- with(hofpitching, cut(BF, c(0, 10000, 15000, 20000, 30000), 
                              labels=c("Less than 10000", "(10000, 15000)", "(15000, 20000)", "more than 20000")))

barplot(table(hofpitching$BF.group))

pie(table(hofpitching$BF.group))

#2
hist(hofpitching$WAR, xlab="WAR", ylab="No. Pitchers", main="", breaks=seq(0, 180, by=20))

standouts <- subset(hofpitching, WAR > 120)
print(standouts$Name)

#3
hofpitching$WAR.Season <- with(hofpitching, WAR / Yrs)

par(plt=c(.2, .94, .145, .883)) 
stripchart(WAR.Season ~ BF.group, data=hofpitching,
         method="jitter", pch=1, las=1)

par(plt=c(.2, .94, .145, .883)) 
boxplot(WAR.Season ~ BF.group, data=hofpitching,
           method="jitter", pch=1, las=1)

#4
hofpitching$MidYear <- with(hofpitching, (From + To) / 2) 
hofpitching.recent <- subset(hofpitching, MidYear >= 1960)

hofpitching[order(hofpitching$WAR.Season),]

windows(width = 7, height = 10)
dotchart(hofpitching$WAR.Season, labels = hofpitching$X, cex = 0.5, pch = 19, xlab = "WAR", 
         main = c("Wins Above Replacement - Hall of Fame Pitchers"))

as.character(hofpitching$Name)[order(hofpitching$WAR.Season, decreasing = TRUE)][1:2]

#5
with(hofpitching, plot(WAR.Season ~ MidYear))

with(hofpitching, identify(WAR.Season, MidYear, Name, n = 2))

#6
library(Lahman)
data(Master, "Batting")


getinfo = function (firstname, lastname){
  playerline = subset(Master, nameFirst == firstname & nameLast == lastname)
  name.code = as.character(playerline$playerID)
  birthyear = playerline$birthYear
  birthmonth = playerline$birthMonth
  birthday = playerline$birthDay
  byear = ifelse(birthmonth <= 6, birthyear, birthyear + 1)
  list(name.code = name.code, byear = byear)
}

cobb = getinfo("Ty", "Cobb")
williams = getinfo("Ted", "Williams")
rose = getinfo("Pete", "Rose")

cobb.bat = subset(Batting, playerID == cobb$name.code)
williams.bat = subset(Batting, playerID == williams$name.code)
rose.bat = subset(Batting, playerID == rose$name.code[1])

cobb.bat$Age = cobb.bat$yearID - cobb$byear
williams.bat$Age = williams.bat$yearID - williams$byear
rose.bat$Age = rose.bat$yearID - rose$byear[1]

with(rose.bat, plot(cumsum(H) ~ Age, type = 'l', las = 1, lty = 1, ylab = "Hits"))

with(cobb.bat, lines(cumsum(H) ~ Age, lty = 2))
with(williams.bat, lines(cumsum(H) ~ Age, lty = 3))
legend("bottomright", legend = c("Cobb", "Rose", "Williams"),
       lty = c(2, 1, 3))

#7
library(devtools)
source_gist(8892981)
parse.retrosheet2.pbp(1998)
data <- read.csv("all1998.csv", header = FALSE)
roster <- read.csv("roster1998.csv")
fields = read.csv("fields.csv")
names(data) = fields[, "Header"]
retro.ids <- read.csv("https://raw.github.com/maxtoki/baseball_R/master/data/retrosheetIDs.csv")

# (a)
sosa.id = as.character(subset(retro.ids, FIRST == "Sammy" & LAST == "Sosa")$ID)
mac.id = as.character(subset(retro.ids, FIRST == "Mark" & LAST == "McGwire")$ID)
sosa.data = subset(data, BAT_ID == sosa.id)
mac.data = subset(data, BAT_ID == mac.id)

# (b)
sosa.data = subset(sosa.data, BAT_EVENT_FL == TRUE)
mac.data = subset(mac.data, BAT_EVENT_FL == TRUE)

# (c)
mac.data$PA <- 1:nrow(mac.data)
sosa.data$PA <- 1:nrow(sosa.data)

# (d)
mac.HR.PA <- mac.data$PA[mac.data$EVENT_CD == 23]
sosa.HR.PA <- sosa.data$PA[sosa.data$EVENT_CD == 23]

# (e)
mac.spacings <- diff(c(0, mac.HR.PA))
sosa.spacings <- diff(c(0, sosa.HR.PA))

# (f)
summary(mac.spacings); summary(sosa.spacings)
par(mfrow = c(1, 2))
hist(mac.spacings)
hist(sosa.spacings)

## clear .GlobalEnv ##
rm(list=ls())