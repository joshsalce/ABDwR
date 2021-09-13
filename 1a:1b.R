install.packages("retrosheet")
require(retrosheet)

install.packages("Lahman")
require(Lahman)
require("dplyr")

install.packages("ggplot")
require(ggplot2)

## 1a
total_walks = numeric()
years = c(1900:2010)

for (year in years) {
  walk_year <- subset(Teams, yearID == year) 
  walks <- sum(walk_year$BB)
  total_walks <- c(total_walks, walks)
}
total_walks

walks_per_year <- data.frame(years, total_walks)
colnames(walks_per_year) <- c('yearID','BB')

BBPlot <- ggplot(walks_per_year,aes(x=yearID,y=BB)) + geom_point(color = "blue")
BBPlot

## 1b
compute.sho <- function(yid){
  shutouts <- subset(Teams, yearID == yid) 
  print(sum(shutouts$SHO))
}
compute.sho(1968)
