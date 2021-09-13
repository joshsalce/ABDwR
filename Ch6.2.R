library(ggplot2)
library(tidyverse)
library(retrosheet)
library(dplyr)

fields = read.csv(file.choose())
data2016 <- read_csv(file.choose(), 
                     col_names = pull(fields, Header),
                     na = character())

data2016 <- data2016 %>%  
  mutate(pseq = gsub("[.>123N*]","", PITCH_SEQ_TX)) 

data2016 <- data2016 %>%  
  mutate(c10 = grepl("^[BIPV]", pseq)) 

b <- "[BIPV]"  
s <- "[CFKLMOQRST]"  

data2016 <- data2016 %>%  
  mutate(c32 = grepl(paste0("^", s, "*", b, s,  "*", b, s, "*", b), pseq)  & grepl(paste0("^", b, "*", s, b, "*", s),  pseq)) 

data2016 %>%  
  filter(c32 == 1) %>%  
  group_by(c32) %>%  
  summarize(N = n(), mean_run_value = mean()) 

data2016 %>% 
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = 
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + 
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
  data2016

data2016 %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(EVENT_OUTS_CT), 
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) ->
  half_innings

data2016 %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
  data2016

data2016 %>%
  mutate(BASES = 
           paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                 ifelse(BASE2_RUN_ID > '', 1, 0),
                 ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) -> 
  data2016

data2016 %>%
  mutate(NRUNNER1 = 
           as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 = 
           as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | 
                        BAT_DEST_ID == 2),
         NRUNNER3 = 
           as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 |
                        RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2, 
                           NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)) ->
  data2016

data2016 %>% 
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) ->
  data2016

data2016 %>%
  filter(Outs.Inning == 3) -> data2016C

## ------------------------------------------------------------------------
data2016C %>% 
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

## ------------------------------------------------------------------------
RUNS_out <- matrix(round(RUNS$Mean, 2), 8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011", 
                             "100", "101", "110", "111")

## ------------------------------------------------------------------------
RUNS.2002 <- matrix(c(.51, 1.40, 1.14,  1.96, .90, 1.84, 1.51, 
                      2.33, .27,  .94,  .68,  1.36, .54, 1.18,
                      .94, 1.51, .10,  .36,  .32,   .63, .23, 
                      .52,   .45, .78), 8, 3)
dimnames(RUNS.2002) <- dimnames(RUNS_out)
cbind(RUNS_out, RUNS.2002)

## ----warning=FALSE-------------------------------------------------------
data2016 %>%
  left_join(select(RUNS, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS, -Outs), 
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> data2016

## 1a------------------------------------------------------------------------
data2016 %>% 
  filter(EVENT_CD == 3) -> K

mean_K <- K %>%
  summarize(mean_run_value = mean(run_value))
mean_K

data2016 %>% 
  filter(EVENT_CD == 14) -> BB

mean_BB <- BB %>%
  summarize(mean_run_value = mean(run_value))
mean_BB

data2016 %>%
  select(GAME_ID, EVENT_ID, RUNS.ROI, c32) %>%
  filter(c32 == "TRUE") %>%
head()

data2016 %>%
  filter(c32 == "TRUE") -> mean.c32

mean.c32 <- mean.c32 %>%
  summarize(mean_run_value = mean(run_value)) -> mean_c32
mean_c32

run_val_c10 = 0.037 - 0.001
run_val_c20 = 0.092 - 0.037  
#...
run_val_c30 = 0.189 - run_val_c20


## 2------------------------------------------------------------------------
data2016 <- data2016 %>%  
  mutate(pseq = gsub("[.>123N+*]","", PITCH_SEQ_TX)) 

# nchar(data2016$PITCH_SEQ_TX[2])
data2016 %>%
  filter(BAT_LINEUP_ID == 1) -> leadoff

all_pitches = 0
for (i in 1:nrow(leadoff)) {
  row_pitch = nchar(leadoff$PITCH_SEQ_TX[i])
  all_pitches = all_pitches + row_pitch
}
all_pitches

all_pitches/nrow(leadoff)
###### 4.20

data2016 %>%
  filter(BAT_LINEUP_ID == 2) -> two_hole

all_pitches2 = 0
for (i in 1:nrow(two_hole)) {
  row_pitch2 = nchar(two_hole$PITCH_SEQ_TX[i])
  all_pitches2 = all_pitches2 + row_pitch2
}
all_pitches2

all_pitches2/nrow(two_hole)
###### 4.40

data2016 %>%
  filter(BAT_LINEUP_ID == 3) -> three_hole

all_pitches3 = 0
for (i in 1:nrow(three_hole)) {
  row_pitch3 = nchar(three_hole$PITCH_SEQ_TX[i])
  all_pitches3 = all_pitches3 + row_pitch3
}
all_pitches3

all_pitches3/nrow(three_hole)
###### 4.33

data2016 %>%
  filter(BAT_LINEUP_ID == 4) -> cleanup

all_pitches4 = 0
for (i in 1:nrow(cleanup)) {
  row_pitch4 = nchar(cleanup$PITCH_SEQ_TX[i])
  all_pitches4 = all_pitches4 + row_pitch4
}
all_pitches4

all_pitches4/nrow(cleanup)
###### 4.32

data2016 %>%
  filter(BAT_LINEUP_ID == 5) -> five_hole

all_pitches5 = 0
for (i in 1:nrow(five_hole)) {
  row_pitch5 = nchar(five_hole$PITCH_SEQ_TX[i])
  all_pitches5 = all_pitches5 + row_pitch5
}
all_pitches5

all_pitches5/nrow(five_hole)
###### 4.22

data2016 %>%
  filter(BAT_LINEUP_ID == 6) -> six_hole

all_pitches6 = 0
for (i in 1:nrow(six_hole)) {
  row_pitch6 = nchar(six_hole$PITCH_SEQ_TX[i])
  all_pitches6 = all_pitches6 + row_pitch6
}
all_pitches6

all_pitches6/nrow(six_hole)
###### 4.24

data2016 %>%
  filter(BAT_LINEUP_ID == 7) -> seven_hole

all_pitches7 = 0
for (i in 1:nrow(seven_hole)) {
  row_pitch7 = nchar(seven_hole$PITCH_SEQ_TX[i])
  all_pitches7 = all_pitches7 + row_pitch7
}
all_pitches7

all_pitches7/nrow(seven_hole)
###### 4.26

data2016 %>%
  filter(BAT_LINEUP_ID == 8) -> eight_hole

all_pitches8 = 0
for (i in 1:nrow(eight_hole)) {
  row_pitch8 = nchar(eight_hole$PITCH_SEQ_TX[i])
  all_pitches8 = all_pitches8 + row_pitch8
}
all_pitches8

all_pitches8/nrow(eight_hole)
###### 4.27


data2016 %>%
  filter(BAT_LINEUP_ID == 9) -> nine_hole

all_pitches9 = 0
for (i in 1:nrow(nine_hole)) {
  row_pitch9 = nchar(nine_hole$PITCH_SEQ_TX[i])
  all_pitches9 = all_pitches9 + row_pitch9
}
all_pitches9

all_pitches9/nrow(nine_hole)
###### 4.31

##(b)- filter by teams in league c()'s

##(c)- TL;DC

## 3------------------------------------------------------------------------
install.packages("stringr")
library(stringr)

pickoffs <- data2016[str_detect(data2016$PITCH_SEQ_TX, '\\+'), ] 
pickoffs2 = subset(pickoffs, pickoffs$BASE2_RUN_ID == "")

pickoffs %>%
  select(BASE1_RUN_ID) %>%
  table() -> most_pickoffs

sort(most_pickoffs)
## Brett Gardner and Rajai Davis

## 4------------------------------------------------------------------------
load(file.choose())

umpires_rhb <- umpires %>%  
  filter(batter_hand == "R", pitch_type == "FF") 

umpires_lhb <- umpires %>%  
  filter(batter_hand == "L", pitch_type == "FF") 

ump_rhb_sample <- umpires_rhb %>%
  sample_n(500)
k_zone_plot <- ggplot(ump_rhb_sample, aes(x = px, y = pz)) + 
  geom_rect(xmin = -0.947, xmax = 0.947, ymin = 1.5, 
            ymax = 3.6, fill = "lightgray", alpha = 0.01) + 
  coord_equal() + 
  scale_x_continuous("Horizontal location (ft.)", 
                     limits = c(-2, 2)) + 
  scale_y_continuous("Vertical location (ft.)", 
                     limits = c(0, 5))
k_zone_plot

ump_rhb_loess <- loess(called_strike ~ px + pz, data = umpires_rhb, 
                     control = loess.control(surface = "direct"))