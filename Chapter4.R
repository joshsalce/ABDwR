install.packages("retrosheet")
require(retrosheet)

install.packages("Lahman")
require(Lahman)
require("dplyr")

library(plyr)

install.packages("ggplot")
require(ggplot2)


data("Teams")
tail(Teams)

#1
sixties <- subset(Teams, yearID >= 1960, yearID <= 1970)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
sixties$RD <- with(sixties, R - RA) 
sixties$Wpct <- with(sixties, W / (W + L))
plot(sixties$RD, sixties$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=sixties)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

sixties$linWpct <- predict(linfit) 
sixties$linResiduals <- residuals(linfit)

plot(sixties$RD, sixties$linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)

##########################
seventies <- subset(Teams, yearID >= 1971, yearID <= 1980)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
seventies$RD <- with(seventies, R - RA) 
seventies$Wpct <- with(seventies, W / (W + L))
plot(seventies$RD, seventies$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=seventies)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

seventies$linWpct <- predict(linfit) 
seventies$linResiduals <- residuals(linfit)
plot(seventies$RD, seventies$linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)

#############################
eighties <- subset(Teams, yearID >= 1981, yearID <= 1990)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
eighties$RD <- with(eighties, R - RA) 
eighties$Wpct <- with(eighties, W / (W + L))
plot(eighties$RD, eighties$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=eighties)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

eighties$linWpct <- predict(linfit) 
eighties$linResiduals <- residuals(linfit)
plot(eighties$RD, eighties$linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)

#################################
nineties <- subset(Teams, yearID >= 1991, yearID <= 2000)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
nineties$RD <- with(nineties, R - RA) 
nineties$Wpct <- with(nineties, W / (W + L))
plot(nineties$RD, nineties$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=nineties)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

nineties$linWpct <- predict(linfit) 
nineties$linResiduals <- residuals(linfit)
plot(nineties$RD, nineties$linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)

points(c(134), c(.132), pch=19) 
text(134, .132, "?", pos=4, cex=.8) 

# 2
nineteenth <- subset(Teams, yearID <= 1900)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
nineteenth$RD <- with(nineteenth, R - RA) 
nineteenth$Wpct <- with(nineteenth, W/(W+L))
plot(nineteenth$RD, nineteenth$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=nineteenth)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

nineteenth$linWpct <- predict(linfit) 
nineteenth$linResiduals <- residuals(linfit)
plot(nineteenth$RD, nineteenth$linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)

###################
nineteenthPythag <- subset(Teams, yearID <= 1900)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
nineteenthPythag$RD <- with(nineteenthPythag, R - RA) 
nineteenthPythag$Wpct <- with(nineteenthPythag, R ^ 2 / (R ^ 2 + RA ^ 2))
plot(nineteenthPythag$RD, nineteenthPythag$Wpct, xlab="run differential", ylab="winning percentage")

linfit <- lm(Wpct ~ RD, data=nineteenthPythag)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

nineteenthPythag$linWpct <- predict(linfit) 
nineteenthPythag$linResiduals <- residuals(linfit)
plot(nineteenthPythag$RD, nineteenthPythag$linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)

#3
install.packages("retrosheet")
require(retrosheet)
??retrosheet
rs1970 <- getRetrosheet("game", 1970)
rs1970_EW <- subset(rs1970, rs1970$HmMgrNm == "Earl Weaver" | rs1970$VisMgrNm == "Earl Weaver")

rs1971 <- getRetrosheet("game", 1971)
rs1971_EW <- subset(rs1971, rs1971$HmMgrNm == "Earl Weaver" | rs1971$VisMgrNm == "Earl Weaver")

rs1972 <- getRetrosheet("game", 1972)
rs1972_EW <- subset(rs1972, rs1972$HmMgrNm == "Earl Weaver" | rs1972$VisMgrNm == "Earl Weaver")

rs1973 <- getRetrosheet("game", 1973)
rs1973_EW <- subset(rs1973, rs1973$HmMgrNm == "Earl Weaver" | rs1973$VisMgrNm == "Earl Weaver")

rs1974 <- getRetrosheet("game", 1974)
rs1974_EW <- subset(rs1974, rs1974$HmMgrNm == "Earl Weaver" | rs1974$VisMgrNm == "Earl Weaver")

rs1975 <- getRetrosheet("game", 1975)
rs1975_EW <- subset(rs1975, rs1975$HmMgrNm == "Earl Weaver" | rs1975$VisMgrNm == "Earl Weaver")

rs1976 <- getRetrosheet("game", 1976)
rs1976_EW <- subset(rs1976, rs1976$HmMgrNm == "Earl Weaver" | rs1976$VisMgrNm == "Earl Weaver")

rs1977 <- getRetrosheet("game", 1977)
rs1977_EW <- subset(rs1977, rs1977$HmMgrNm == "Earl Weaver" | rs1977$VisMgrNm == "Earl Weaver")

rs1978 <- getRetrosheet("game", 1978)
rs1978_EW <- subset(rs1978, rs1978$HmMgrNm == "Earl Weaver" | rs1978$VisMgrNm == "Earl Weaver")

rs1979 <- getRetrosheet("game", 1979)
rs1979_EW <- subset(rs1979, rs1979$HmMgrNm == "Earl Weaver" | rs1979$VisMgrNm == "Earl Weaver")


rs1970_EW$home_win <- ifelse(rs1970_EW$HmRuns > rs1970_EW$VisRuns,1,0)
rs1971_EW$home_win <- ifelse(rs1971_EW$HmRuns > rs1971_EW$VisRuns,1,0)
rs1972_EW$home_win <- ifelse(rs1972_EW$HmRuns > rs1972_EW$VisRuns,1,0)
rs1973_EW$home_win <- ifelse(rs1973_EW$HmRuns > rs1973_EW$VisRuns,1,0)
rs1974_EW$home_win <- ifelse(rs1974_EW$HmRuns > rs1974_EW$VisRuns,1,0)
rs1975_EW$home_win <- ifelse(rs1975_EW$HmRuns > rs1975_EW$VisRuns,1,0)
rs1976_EW$home_win <- ifelse(rs1976_EW$HmRuns > rs1976_EW$VisRuns,1,0)
rs1977_EW$home_win <- ifelse(rs1977_EW$HmRuns > rs1977_EW$VisRuns,1,0)
rs1978_EW$home_win <- ifelse(rs1978_EW$HmRuns > rs1978_EW$VisRuns,1,0)
rs1979_EW$home_win <- ifelse(rs1979_EW$HmRuns > rs1979_EW$VisRuns,1,0)

W_pct = 0
Pythag = 0

W_pt <- function(team, df) {
  W <- 0
  G <- 0
  for (i in 1:nrow(df)) {
    W <- W + ifelse(df$HmTm[i] == team, df$home_win[i],
                    ifelse(df$home_win[i] == 1, 0, 1))
    G <- G + 1
  }
  W_pct <- W/G
  return(W_pct)
}

calc_RD <- function(team, df) {
  Runs <- 0
  Runs_A <- 0
  for (i in 1:nrow(df)) {
    Runs <- Runs + ifelse(df$HmTm[i] == team, df$HmRuns[i], df$VisRuns[i])
    Runs_A <- Runs_A + ifelse(df$HmTm[i] != team, df$HmRuns[i], df$VisRuns[i])
  }
  run_diff <- Runs - Runs_A
  Pythag <- Runs ^ 2 / (Runs ^ 2 + Runs_A ^ 2)
  print(c(run_diff, Pythag))
}

calc_RD("BAL", rs1970_EW)
calc_RD("BAL", rs1971_EW)
calc_RD("BAL", rs1972_EW)
calc_RD("BAL", rs1973_EW)
calc_RD("BAL", rs1974_EW)
calc_RD("BAL", rs1975_EW)
calc_RD("BAL", rs1976_EW)
calc_RD("BAL", rs1977_EW)
calc_RD("BAL", rs1978_EW)
calc_RD("BAL", rs1979_EW)

run_diffs = c(218,212,89,193,47,129,21,66,26,175)
Pythags = c(0.6556264,0.6621622,0.5929653,0.6436732,0.5369283,0.6033261,0.5172504,0.5479939,0.5201157,0.6284996)
EW_plot = plot(run_diffs, Pythags, xlab="Run Differential", ylab="Pythagorean Win %")

linfit <- lm(Pythags ~ run_diffs)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

linWpct <- predict(linfit) 
linResiduals <- residuals(linfit)
plot(run_diffs, linResiduals, xlab="run differential", ylab="residual")
abline(h=0, lty=3)