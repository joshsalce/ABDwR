library(tidyverse)

P <- matrix(c(.3, .7, 0, 0,  
              0, .3, .7, 0,  
              0, 0, .3, .7,  
              0, 0, 0, 1), 4, 4, byrow = TRUE) 

P2 = P %*% P
head(P2)


N <- solve(diag(c(1, 1, 1)) - P[-4, -4]) 

## ------------------------------------------------------------------------
simulate_half_inning <- function(P) {  
  s <- 1  
  path <- NULL  
  while(s < 4){  
    s.new <- sample(1:4, 1, prob = P[s,])  
    path <- c(path, s.new)  
    s <- s.new  
  }  
  length(path)
} 

lengths <- replicate(1000, simulate_half_inning(P))

table(lengths)
sum(lengths >= 4) / 1000
mean(lengths)


## ------------------------------------------------------------------------

make_schedule <- function(teams, k) {
  n.teams <- length(teams)
  Home <- rep(rep(teams, each = n.teams), k)
  Visitor <- rep(rep(teams, n.teams), k)
  schedule <- tibble(Home = Home, Visitor = Visitor) %>%
    filter(Home != Visitor)
}
NL_2 <- c("CHC", "CIN", "BRO",
        "NYG", "PHI", "PIT", "BSN", "STL")
schedule_2 <- bind_rows(make_schedule(NL_2, 11))

st.talent = 0.25
talents_2 <- rnorm(25, 0, st.talent)
TAL <- tibble(Team = teams, League = league,
              Talent = talents)
SCH <- schedule %>%
  inner_join(TAL, by = c("Home" = "Team")) %>%
  rename(Talent.Home = Talent) %>%
  inner_join(TAL, by = c("Visitor" = "Team", "League")) %>%
  rename(Talent.Visitor = Talent)

## ------------------------------------------------------------------------
SCH %>% 
  mutate(prob.Home = exp(Talent.Home) /
           (exp(Talent.Home) + exp(Talent.Visitor))) -> SCH

## ------------------------------------------------------------------------
head(SCH)

## ------------------------------------------------------------------------
SCH %>%
  mutate(outcome = rbinom(nrow(.), 1, prob.Home),
         winner = ifelse(outcome, Home, Visitor)) -> SCH

## ------------------------------------------------------------------------
SCH %>%
  select(Visitor, Home, prob.Home, outcome, winner) %>% 
  head()

## ------------------------------------------------------------------------
SCH %>% 
  group_by(winner) %>%
  summarize(Wins = n()) %>%
  inner_join(TAL, by = c("winner" = "Team")) -> RESULTS

## ------------------------------------------------------------------------





## ------------------------------------------------------------------------





