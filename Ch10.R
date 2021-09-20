library(tidyverse)
williams = read.csv(file.choose())

williams %>% summarize(sum(H)/sum(AB))

williams %>% mutate(HIT = ifelse(H > 0, 1, 0)) -> williams
pull(williams, HIT)

streaks <- function(y) {
  x = rle(y)
  class(x) <- "list"
  return(as_tibble(x))
}

williams %>%
  pull(HIT) %>%
  streaks() %>%
  filter(values == 1) %>%
  pull(lengths)
##a 23 hit streak

williams %>%
  pull(HIT) %>%
  streaks() %>%
  filter(values == 0) %>%
  pull(lengths)
##b 5 hitless streak

##-----------------------------------------------------------
library(dplyr)
install.packages("zoo")
library(zoo)  
moving_average <- function(df, width) {  
  N <- nrow(df)  
  df %>%  
    transmute(Game = rollmean(1:N, k = width, fill = NA),  
              Average = rollsum(H, width, fill = NA)/  
                rollsum(AB, width, fill = NA))  
  } 

williams_ma = moving_average(williams, 5)

ggplot(williams_ma, aes(Game, Average)) +  
  geom_line() +  
  geom_hline(data = summarize(williams, bavg = sum(H)/sum(AB)),  aes(yintercept = bavg)) +  
  geom_rug(data = filter(williams, HIT == 1),  aes(Rk, .3 * HIT), sides = "b") 

##-----------------------------------------------------------
fields <- read_csv(file.choose())
data2016 <- read_csv(file.choose(), 
                     col_names = pull(fields, Header),
                     na = character())

##-----------------------------------------------------------
lemahieu_AB <- data2016 %>%
  filter(BAT_ID == "lemad001", AB_FL == TRUE)

lemahieu_AB %>%
  mutate(H = ifelse(H_FL > 0 , 1, 0),
         DATE = str_sub(GAME_ID, 4 , 12),
         AB = 1) %>%
  arrange(DATE) -> lemahieu_AB

lemahieu_AB %>%
  pull(H) %>%
  streaks() %>%
  filter(values == 1) %>%
  pull(lengths)
## 5 consecutive hits

lemahieu_AB %>%
  pull(H) %>%
  streaks() %>%
  filter(values == 0) -> lemahieu_out

lemahieu_out %>%
  pull(lengths)
## ofer 15- twice

lemahieu_out %>%
  group_by(lengths) %>%
  count()

lemahieu_AB %>%
  mutate(AB_Num = row_number()) %>%
  filter(H == 1) -> lemahieu.H

moving_average(lemahieu_AB, 30) %>%  
  ggplot(aes(Game, Average)) +  
  geom_line() + 
  xlab("AB") +  
  geom_hline(yintercept = mean(lemahieu_AB$H)) +  
  geom_rug(data = lemahieu.H,  
           aes(AB_Num, .3 * H), sides = "b") 

##-----------------------------------------------------------
lemahieu_S <- lemahieu_AB %>%
  pull(H) %>%
  streaks() %>%
  filter(values == 0) %>%
  summarize(C = sum(lengths ^ 2)) %>%
  pull()
lemahieu_S

random_mix <- function(y) {  
  y %>%  
    sample() %>%  
    streaks() %>%  
    filter(values == 0) %>%  
    summarize(C = sum(lengths ^ 2)) %>%  
    pull()  
} 

lemahieu.random <- replicate(1000, random_mix(lemahieu_AB$H))

ggplot(data.frame(lemahieu.random), aes(lemahieu.random)) +  
  geom_histogram(aes(y = stat(density)), bins = 20,  
                 color = "blue", fill = "white") +  
  geom_vline(xintercept = lemahieu_S, size = 2) +  
  annotate(geom = "text", x = lemahieu_S * 1.15,  
           y = 0.0010, label = "OBSERVED", size = 5) 

quantile(lemahieu.random, probs = 0:10/100)

##-----------------------------------------------------------
library(Lahman)

data2016 %>%
  mutate(AB = 1) -> data2016
  
data2016 %>%
  group_by(BAT_ID) %>%
  filter(sum(AB) >= 400) -> AB_400

##data2016 %>%
##  filter(BAT_ID == "polag001") -> polanco

players = unique(AB_400$BAT_ID)
players = sort(players)
players

library(dplyr)

AB_400 %>%
  group_by(BAT_ID, DATE) %>%
  arrange(DATE) %>%
  summarize(AB = sum(AB),
            H = sum(H),
            bAVG = sum(H)/sum(AB)) -> AB_avg
           ## wOBA = (
           ##   (((0.69*sum(ifelse(data2016$EVENT_CD == 16, 1, 0)))) + (0.72*sum(ifelse(data2016$EVENT_CD == 16, 1, 0))) + 
           ##             (0.89*sum(ifelse(data2016$EVENT_CD == 20, 1, 0))) +  (1.27*sum(ifelse(data2016$EVENT_CD == 21, 1, 0))) + 
           ##             (1.62*sum(ifelse(data2016$EVENT_CD == 22, 1, 0))) + (2.10*sum(ifelse(data2016$EVENT_CD == 23, 1, 0))) )/
           ##            (sum(AB)+sum(ifelse(data2016$EVENT_CD == 14, 1, 0))-sum(ifelse(data2016$EVENT_CD == 15, 1, 0))+sum(ifelse(data2016$EVENT_CD == 2, 1, 0))+sum(ifelse(data2016$EVENT_CD == 16, 1, 0))))) -> test
  

regroup <- function(data, group_size) {  
  out <- data %>%  
    mutate(id = row_number() - 1,  
           group_id = floor(id/ group_size))  
  #hack to avoid a small leftover bin!  
  if (nrow(data)%% group_size != 0) {  
    max_group_id <- max(out$group_id)  
    out <- out %>%  
      mutate(group_id = ifelse(group_id == max_group_id,  
                               group_id - 1, group_id))  
  }  
  out %>%  
    group_by(group_id) %>%  
    summarize(AB = n(), H = sum(H), avg = H/AB)  
} 

lemahieu_AB %>%
  regroup(5) -> test

summarize_streak_data <- function(data, name, group_size = 5) {  
  data %>%  
    filter(BAT_ID == name) %>%  
    arrange(DATE) %>%  
    regroup(group_size) %>%  
    summarize(total_hits = sum(H),  
              Average = avg,  
              SD = sd(avg))  
} 

lemahieu_sum <- summarize_streak_data(AB_400, "lemad001")


AB_400_results = players %>%
  map_df(summarize_streak_data, data = AB_400)
##-----------------------------------------------------------


##-----------------------------------------------------------