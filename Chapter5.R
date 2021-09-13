source("global_config.R")



fields <- read_csv(file.choose())
data2016 <- read_csv(file.choose(), 
                     col_names = pull(fields, Header),
                     na = character())

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

  ## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
Master %>% 
  filter(nameFirst == "Jose", nameLast == "Altuve") %>%
  pull(retroID) -> altuve.id

## ------------------------------------------------------------------------
data2016 %>% 
  filter(BAT_ID == altuve.id,
         BAT_EVENT_FL == TRUE) -> altuve

## ------------------------------------------------------------------------
altuve %>%  
  select(STATE, NEW.STATE, run_value) %>%  
  slice(1:3) 

 ## ------------------------------------------------------------------------
altuve %>% 
  group_by(BASES) %>% 
  summarize(N = n())

## ----runs1, fig.cap="Stripchart of run values of Jos\\'e Altuve for all 2016 plate appearances as a function of the runners state. The points have been jittered since there are many plate appearances with identical run values."----
ggplot(altuve, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("RUNNERS")

## ------------------------------------------------------------------------
altuve %>% 
  group_by(BASES) %>%
  summarize(RUNS = sum(run_value),
            PA = n()) -> Runs_Altuve
Runs_Altuve

## ------------------------------------------------------------------------
Runs_Altuve %>% summarize(RE24 = sum(RUNS))

## ------------------------------------------------------------------------
data2016 %>% filter(BAT_EVENT_FL == TRUE) -> data2016b

## ------------------------------------------------------------------------
data2016b %>% 
  group_by(BAT_ID) %>%
  summarize(RE24 = sum(run_value),
            PA = length(run_value),
            Runs.Start = sum(Runs.State)) -> runs

## ------------------------------------------------------------------------
runs %>% 
  filter(PA >= 400) -> runs400
head(runs400)

## ----runvalues2, fig.cap="Scatterplot of total run value against the runs potential for all players in the 2016 season with at least 400 plate appearances. A smoothing curve is added to the scatterplot -- this shows that players who had more run potential tend to have large run values."----
ggplot(runs400, aes(Runs.Start, RE24)) +
  geom_point() + 
  geom_smooth() +
  geom_hline(yintercept = 0, color = "blue") -> plot1
plot1

## ----runvalues3, fig.cap="Scatterplot of RE24 against the runs potential for all players in the 2016 season with unusual players identified."----
runs400 %>%
  inner_join(Master, by = c("BAT_ID" = "retroID")) -> runs400
library(ggrepel)
plot1 + 
  geom_text_repel(data = filter(runs400, RE24 >= 40), 
                  aes(label = nameLast)) 

## ------------------------------------------------------------------------
data2016 %>%
  inner_join(runs400, by = "BAT_ID") -> regulars

regulars %>% 
  group_by(BAT_ID, BAT_LINEUP_ID) %>%
  summarize(N = n()) %>% 
  arrange(desc(N)) %>% 
  mutate(Position = first(BAT_LINEUP_ID)) -> positions

runs400 %>%
  inner_join(positions, by = "BAT_ID") -> runs400

## ----lineup, fig.cap="Scatterplot of total run value against the runs potential for all players in the 2016 season with at least 400 plate appearances. The points are labeled by the position in the batting lineup and the large point corresponds to Jos\\'e Altuve.\\index{Altuve, Jos\\'e}"----
ggplot(runs400, aes(Runs.Start, RE24, label = Position)) +
  geom_text() +
  geom_hline(yintercept = 0, color = "blue") +
  geom_point(data = filter(runs400, BAT_ID == altuve.id),
             size = 4, shape = 16, color = "blue")

## ------------------------------------------------------------------------
data2016 %>% filter(EVENT_CD == 23) -> home_runs

## ------------------------------------------------------------------------
home_runs %>%
  select(STATE) %>%
  table()

## ------------------------------------------------------------------------
home_runs %>%
  select(STATE) %>%
  table() %>%
  prop.table() %>%
  round(3)

## ------------------------------------------------------------------------
mean_hr <- home_runs %>%
  summarize(mean_run_value = mean(run_value))
mean_hr

## ----homeruns, fig.cap="Histogram of the run values of the home runs hit during the 2016 season. The vertical line shows the location of the mean run value of a home run."----
ggplot(home_runs, aes(run_value)) +
  geom_histogram() + 
  geom_vline(data = mean_hr, aes(xintercept = mean_run_value), 
             color = "blue", size = 1.5) +
  annotate("text", 1.7, 2000, 
           label = "Mean Run\nValue", color = "blue")

## ------------------------------------------------------------------------
home_runs %>% 
  arrange(desc(run_value)) %>%
  select(STATE, NEW.STATE, run_value) %>%
  head(1)

## ----singles, fig.cap="Histogram of the run values of the singles hit during the 2016 season. The vertical line shows the location of the mean run value of a single."----
data2016 %>% 
  filter(EVENT_CD == 20) -> singles

mean_singles <- singles %>%
  summarize(mean_run_value = mean(run_value))
  mean_singles

ggplot(singles, aes(run_value)) + 
  geom_histogram(bins = 40) +
  geom_vline(data = mean_singles, color = "blue", 
             aes(xintercept = mean_run_value), size = 1.5) +
  annotate("text", 0.8, 4000, 
           label = "Mean Run\nValue", color = "blue")

## ------------------------------------------------------------------------
singles %>%
  select(STATE) %>%
  table()

## ------------------------------------------------------------------------
singles %>% 
  arrange(desc(run_value)) %>%
  select(STATE, NEW.STATE, run_value) %>%
  slice(1)

## ------------------------------------------------------------------------
singles %>% 
  arrange(run_value) %>%
  select(STATE, NEW.STATE, run_value) %>%
  slice(1)

## ------------------------------------------------------------------------
data2016 %>% 
  filter(EVENT_CD %in% c(4, 6)) -> stealing

## ------------------------------------------------------------------------
stealing %>% 
  group_by(EVENT_CD) %>% 
  summarize(N = n()) %>% 
  mutate(pct = N / sum(N))

## ------------------------------------------------------------------------
stealing %>% group_by(STATE) %>% summarize(N = n())

## ----stealing, fig.cap="Histogram of the run values of all steal attempts during the 2016 season."----
ggplot(stealing, aes(run_value, fill = factor(EVENT_CD))) + 
  geom_histogram() +
  scale_fill_manual(name = "EVENT_CD",
                    values = c("blue", "gray70"),
                    labels = c("Stolen Base (SB)",
                               "Caught Stealing (CS)"))

## ------------------------------------------------------------------------
stealing %>% filter(STATE == "100 1") -> stealing.1001

## ------------------------------------------------------------------------
stealing.1001 %>% 
  group_by(EVENT_CD) %>% 
  summarize(N = n()) %>% 
  mutate(pct = N / sum(N))

## ------------------------------------------------------------------------
stealing.1001 %>% 
  group_by(NEW.STATE) %>% 
  summarize(N = n()) %>% 
  mutate(pct = N / sum(N))

## ------------------------------------------------------------------------
stealing.1001 %>% summarize(Mean = mean(run_value))

##1111111111111111111111111111-------------------------------------------------------------------------

data2016 %>%
  filter(EVENT_CD == 21) -> 
  doubles

## ------------------------------------------------------------------------
doubles %>%
  select(STATE) %>%
  table()

## ------------------------------------------------------------------------
doubles %>%
  select(STATE) %>%
  table() %>%
  prop.table() %>%
  round(3)

## ------------------------------------------------------------------------
mean_2B <- doubles %>%
  summarize(mean_run_value = mean(run_value))
mean_2B

## ----homeruns, fig.cap="Histogram of the run values of the home runs hit during the 2016 season. The vertical line shows the location of the mean run value of a home run."----
ggplot(doubles, aes(run_value)) +
  geom_histogram() + 
  geom_vline(data = mean_2B, aes(xintercept = mean_run_value), 
             color = "blue", size = 1.5) +
  annotate("text", 1.7, 2000, 
           label = "Mean Run\nValue", color = "blue")

## ------------------------------------------------------------------------
doubles %>% 
  arrange(desc(run_value)) %>%
  select(STATE, NEW.STATE, run_value) %>%
  head(1)

doubles %>% 
  arrange(run_value) %>%
  select(STATE, NEW.STATE, run_value) %>%
  head(1)

data2016 %>%
  filter(EVENT_CD == 22) -> triples

##-------------------------------------------------------------------------
triples %>%
  select(STATE) %>%
  table()

## ------------------------------------------------------------------------
triples %>%
  select(STATE) %>%
  table() %>%
  prop.table() %>%
  round(3)

## ------------------------------------------------------------------------
mean_3B <- triples %>%
  summarize(mean_run_value = mean(run_value))
mean_3B

## ----homeruns, fig.cap="Histogram of the run values of the home runs hit during the 2016 season. The vertical line shows the location of the mean run value of a home run."----
ggplot(triples, aes(run_value)) +
  geom_histogram() + 
  geom_vline(data = mean_3B, aes(xintercept = mean_run_value), 
             color = "blue", size = 1.5) +
  annotate("text", 1.7, 2000, 
           label = "Mean Run\nValue", color = "blue")

## ------------------------------------------------------------------------
triples %>% 
  arrange(desc(run_value)) %>%
  select(STATE, NEW.STATE, run_value) %>%
  head(1)

triples %>% 
  arrange(run_value) %>%
  select(STATE, NEW.STATE, run_value) %>%
  head(1)

# 1B: 0.439 , 2B: 0.739, 3B: 1.01, HR: 1.38

##------------------------------------------------------222222222222222222
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

data2016 %>% 
  filter(EVENT_CD == 16) -> HBP

mean_HBP <- HBP %>%
  summarize(mean_run_value = mean(run_value))
mean_HBP

data2016 %>% 
  filter(EVENT_CD == 14) -> BB

mean_BB <- BB %>%
  summarize(mean_run_value = mean(run_value))
mean_BB

##------------------------------------------------------333333333333333333
data2016 %>% filter(BAT_ID == "eatoa002" | BAT_ID == "marts002",
                 BAT_EVENT_FL == TRUE) %>% 
  group_by(BAT_ID) %>% 
  summarize(N = n(),
            M = mean(run_value),
            S = sum(run_value))

##------------------------------------------------------444444444444444444
data2016 %>% 
  filter(Outs.Inning == 3) %>% 
  group_by(STATE) %>%
  summarize(P = mean(RUNS.ROI >= 1))  -> PROB
PROB

##------------------------------------------------------6666666666666666


## (b)
singles %>%
  group_by(STATE, NEW.STATE) %>%
  summarize(N = n()) 

## (c)
singles %>% 
  mutate(BASES = substr(STATE, 1, 3), NEW.BASES = substr(NEW.STATE, 1, 3)) %>%  
  filter(BASES == "100") %>% 
  group_by(NEW.BASES) %>% 
  summarize(N = n())

## (d)
singles %>% 
  mutate(BASES = substr(STATE, 1, 3), NEW.BASES = substr(NEW.STATE, 1, 3)) %>%  
  filter(BASES == "110") %>% 
  group_by(NEW.BASES) %>% 
  summarize(N = n())

##------------------------------------------------------6666666666666666

library(Lahman)
Master %>% 
  filter(nameFirst == "Mike", nameLast == "Trout") %>%
  pull(retroID) -> trout.id

data2016 %>% 
  filter(BAT_ID == trout.id,
         BAT_EVENT_FL == TRUE) -> trout

trout %>% 
  group_by(BASES) %>% 
  summarize(N = n())

## ----runs1, fig.cap="Stripchart of run values of Jos\\'e Altuve for all 2016 plate appearances as a function of the runners state. The points have been jittered since there are many plate appearances with identical run values."----
ggplot(trout, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("RUNNERS")

trout %>% 
  group_by(BASES) %>%
  summarize(RUNS = sum(run_value),
            PA = n()) -> Runs_Trout
Runs_Trout

Runs_Trout %>% summarize(RE24 = sum(RUNS))



Master %>%
  filter(nameFirst == "Josh", nameLast == "Donaldson") %>%
  pull(retroID) -> JD.id
  
data2016 %>%
  filter(BAT_ID == JD.id, BAT_EVENT_FL == TRUE) -> JD

JD %>%
  group_by(BASES) %>%
  summarize(N = n())

ggplot(JD, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("RUNNERS")

JD %>% 
  group_by(BASES) %>%
  summarize(RUNS = sum(run_value),
            PA = n()) -> Runs_JD
Runs_JD

Runs_JD %>% summarize(RE24 = sum(RUNS))



Master %>%
  filter(nameFirst == "Corey", nameLast == "Seager") %>%
  pull(retroID) -> seager.id

data2016 %>%
  filter(BAT_ID == seager.id, BAT_EVENT_FL == TRUE) -> seager

seager %>%
  group_by(BASES) %>%
  summarize(N = n())

ggplot(seager, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("RUNNERS")

seager %>% 
  group_by(BASES) %>%
  summarize(RUNS = sum(run_value),
            PA = n()) -> Runs_seag
Runs_seag

Runs_seag %>% summarize(RE24 = sum(RUNS))
