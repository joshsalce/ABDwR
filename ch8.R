library(tidyverse)
library(Lahman)

### 1

batting <- Batting %>%  
  replace_na(list(SF = 0, HBP = 0))

Master %>%  
  filter(nameFirst == "Willie", nameLast == "Mays") %>%  
  pull(playerID) -> mays_id 

get_stats <- function(player.id) {  
  batting %>%  
    filter(playerID == player.id) %>%  
    inner_join(Master, by = "playerID") %>%  
    mutate(birthyear = ifelse(birthMonth >= 7,  
                              birthYear + 1, birthYear),  
           Age = yearID - birthyear, 
           AVG = H / AB,
           HR_RATE = (HR / AB),
           SLG = (H - X2B - X3B - HR +  2 * X2B + 3 * X3B + 4 * HR)/ AB, 
           OBP = (H + BB + HBP)/ (AB + BB + HBP + SF), 
           OPS = SLG + OBP) %>%
    select(Age, AVG, HR_RATE, SLG, OBP, OPS)  
} 

mays = get_stats(mays_id)

ggplot(mays, aes(Age,OPS)) + geom_point()

fit_model <- function(d) {  
  fit <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d)  
  b <- coef(fit)  
  Age.max <- 30 - b[2]/ b[3]/ 2  
  Max <- b[1] - b[2] ^ 2/ b[3]/ 4  
  list(fit = fit, Age.max = Age.max, Max = Max)  
  } 

mays2 = fit_model(mays)

ggplot(mays, aes(Age, OPS)) + geom_point() +  
  geom_smooth(method = "lm", se = FALSE, size = 1.5,  
              formula = y ~ poly(x, 2, raw = TRUE)) +  
  geom_vline(xintercept = mays2$Age.max,  
             linetype = "dashed", color = "darkgrey") +  
  geom_hline(yintercept = mays2$Max,  
             linetype = "dashed", color = "darkgrey") +  
  annotate(geom = "text", x = c(29, 20), y = c(0.72, 1.1),  
           label = c("Peak age", "Max"), size = 5) 

### 2
vars <- c("G", "AB", "R", "H", "X2B", "X3B", 
          "HR", "RBI", "BB", "SO", "SB")

Batting %>% 
  group_by(playerID) %>% 
  summarize_at(vars, sum, na.rm = TRUE) -> C.totals

C.totals %>%
  mutate(AVG = H / AB,
         SLG = (H - X2B - X3B - HR + 2 * X2B +
                  3 * X3B + 4 * HR) / AB) ->
C.totals

Fielding %>% 
  group_by(playerID, POS) %>%
  summarize(Games = sum(G)) %>% 
  arrange(playerID, desc(Games)) %>% 
  filter(POS == first(POS)) -> Positions

C.totals %>%
  inner_join(Positions, by = "playerID") %>%
  mutate(Value.POS = case_when(
    POS == "C" ~ 240,
    POS == "SS" ~ 168,
    POS == "2B" ~ 132,
    POS == "3B" ~ 84,
    POS == "OF" ~ 48,
    POS == "1B" ~ 12, 
    TRUE ~ 0)) -> C.totals

similar <- function(p, number = 6) {
  C.totals %>% filter(playerID == p) -> P
  C.totals %>% 
    mutate(sim_score = 1000 -
             floor(abs(G - P$G) / 20) -
             floor(abs(AB - P$AB) / 75) -
             floor(abs(R - P$R) / 10) -
             floor(abs(H - P$H) / 15) -
             floor(abs(X2B - P$X2B) / 5) -
             floor(abs(X3B - P$X3B) / 4) -
             floor(abs(HR - P$HR) / 2) -
             floor(abs(RBI - P$RBI) / 10) -
             floor(abs(BB - P$BB) / 25) -
             floor(abs(SO - P$SO) / 150) -
             floor(abs(SB - P$SB) / 20) - 
             floor(abs(AVG - P$AVG) / 0.001) - 
             floor(abs(SLG - P$SLG) / 0.002) -
             abs(Value.POS - P$Value.POS)) %>%
    arrange(desc(sim_score)) %>% 
    head(number)
}

s_player <- similar(mays_id, number = 6) %>% 
  select(playerID) %>% pull()
s_player

get_stats2 <- function(playerid){
  d <- get_stats(playerid)
  d %>% mutate(Player = playerid)
}
map_df(s_player, get_stats2) -> alldata
ggplot(alldata, aes(Age, OPS)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 3)

### 3
batting %>% 
  group_by(playerID) %>%
  summarize(Hs = sum(H)) %>% 
  arrange(playerID, desc(Hs)) %>% 
  filter(Hs >= 3200) -> h3200

h3200_id = h3200$playerID
  
h3200_data = map_df(h3200_id, get_stats2)

ggplot(h3200_data, aes(Age, AVG)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 3)

### 4
batting %>%
  group_by(playerID) %>%
  summarize(HRs = sum(HR)) %>%
  arrange(playerID, desc(HRs)) -> top_HR

top10_HR = head(top_HR[order(top_HR$HRs, decreasing= T),], n = 10)

top10HR_id = top10_HR$playerID

top10_HR_data = map_df(top10HR_id, get_stats2)

ggplot(top10_HR_data, aes(Age, HR_RATE)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 3)

### 5
batting <- batting %>%
  group_by(playerID) %>%
  mutate(Career_AB = cumsum(AB))

#A
batting %>%
  group_by(playerID) %>%
  filter(yearID >= 1940, yearID <= 1945, Career_AB >= 2000) -> forties

forties_ID = forties$playerID

forties_data = map_df(forties_ID, get_stats2)

#B
batting %>%
  group_by(playerID) %>%
  filter(yearID >= 1970, yearID <= 1975, Career_AB >= 2000) -> seventies

seventies_ID = seventies$playerID

seventies_data = map_df(seventies_ID, get_stats2)

#C
ggplot(forties_data, aes(Age, OPS)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 3)

ggplot(seventies_data, aes(Age, OPS)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              size = 1.5) +
  facet_wrap(~ Player, ncol = 3)