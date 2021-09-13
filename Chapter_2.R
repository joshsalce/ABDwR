install.packages("retrosheet")
require(retrosheet)

install.packages("Lahman")
require(Lahman)
require("dplyr")

library(plyr)

install.packages("ggplot")
require(ggplot2)

## 1a


HoF_SB = data.frame(
  Name = c("Rickey Henderson","Lou Brock", "Ty Cobb", "Eddie Collins","Max Carey", "Joe Morgan","Luis Aparicio", "Paul Molitor", "Roberto Alomar"),
  SB = c(1406,938,897,741,738,689,506,504,474),
  CS = c(335,307,212,195,109,162,136,131,114),
  G = c(3081,2616,3034,2826,2476,2649,2599,2683,2379)
)

## 1b, 1c, 1d, 1e
players = c(1:9)
SB.Attempt = numeric()
SB.Success = numeric()
SB.Game = numeric()
for (player in players) {
  SB.Attempt <- c(SB.Attempt, HoF_SB[player,2] + HoF_SB[player,3])
  SB.Success <- c(SB.Success, signif(HoF_SB[player,2]/SB.Attempt[player] * 100, 3))
  SB.Game <- c(SB.Game, signif(HoF_SB[player,2]/HoF_SB[player,4],2))
  }
SB.Attempt
SB.Success
SB.Game

HoF_SB$SB_A <- SB.Attempt
HoF_SB$SB_Suc <- SB.Success
HoF_SB$SB_G <- SB.Game

HoF_SB

SBPlot <- plot(HoF_SB$SB_G, HoF_SB$SB_Suc)
SBPlot

## 2
outcomes = c("Single", "Out", "Out", "Single", "Out", "Double", "Out", "Walk", "Out", "Single")

tab <- table(outcomes)
tab
f.outcomes <- factor(outcomes, levels=c("Out", "Walk", "Single", "Double"))
f.tab <- table(f.outcomes)
f.tab

## 3 & 4
High_W <- data.frame(
  Name= c("Pete Alexander","Roger Clemens","Pud Galvin","Walter Johnson","Greg Maddux","Christy Mathewson","Kid Nichols","Warren Spahn","Cy Young"),
  W= c(373,354,364,417,355,373,361,363,511),
  L = c(208,184,310,279,227,188,208,245,316),
  SO = c(2198,4672,1806,3509,3371,2502,1868,2583,2803),
  BB = c(951,1580,745,1363,999,844,1268,1434,1217)
)

W_pt <- numeric()
for (player in players) {
  W_pt <- c(W_pt, signif(High_W[player,2]/(High_W[player,2] + High_W[player,3]) * 100, 3))
}
W_pt
High_W$W_pct <- W_pt

ordered_wpct = c(order(High_W$W_pct, decreasing = TRUE))

## 5
library(Lahman)
data(Pitching)
stats <- function(d){
  c.SO <- sum(d$SO, na.rm=TRUE)
  c.BB <- sum(d$BB, na.rm=TRUE)
  c.IPouts <- sum(d$IPouts, na.rm=TRUE) 
  c.midYear <- median(d$yearID, na.rm=TRUE) 
  data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts,midYear=c.midYear)
}

library(plyr)

career.pitching = ddply(Pitching, .(playerID), stats)

df = merge(Pitching, career.pitching, by = "playerID")

career.10000 = subset(career.pitching, IPouts >= 10000)

with(career.10000, plot(midYear, SO / BB))
with(career.10000, lines(lowess(midYear, SO / BB)))