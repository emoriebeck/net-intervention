library(qgraph)
library(graphicalVAR)
library(psych)
library(Amelia)
library(gridExtra)
library(rmarkdown)
library(forcats)
library(knitr)
library(kableExtra)
library(plyr)
library(stringr)
library(tidyverse)
library(flexdashboard)

wd <- "/Volumes/beck/IPCS"
files <- list.files(wd)

load(sprintf("%s/clean data/clean_data_2017-10-29.RData", wd))
codebook <- readxl::read_xlsx("~/Box Sync/network/interventions study/ESM_items.xlsx", sheet = "codebook")

BFI.wide <- BFI %>% filter(!is.na(item)) %>%  
  select(-trial_index, -trait, -facet, -answer) %>%
  spread(key = item, value = responses2) %>%
  mutate_at(vars(A1:O9), funs(as.numeric))

jitter_fun <- function(df){
  z <- apply(df, 2, function(x)sum(!is.na(x)))
  df <- df[,z > 1]
  sd_fun <- function(x){if(sd(x, na.rm = T) == 0) jitter(x, amount = runif(1,0,.05)) else x}
  df2 <- data.frame(apply(df, 2, sd_fun))
  colnames(df2) <- colnames(df2)
  return(df2)
}

f.BFI.wide <- 
  BFI.wide %>%
  group_by(SID) %>%
  arrange(SID, Date, Hour, Minute) %>%
  mutate(all_beeps = seq(1, n(),1)) %>% 
  select(SID, all_beeps, A1:O9) %>%
  nest() %>%
  mutate(data2 = map(data, jitter_fun)) %>%
  unnest(data2, .drop = T) %>%
  mutate(SID = as.character(SID))

short.facets <- c("O_AesSens", "E_Assert", "N_Depr", "N_EmoVol", "O_IntCur", "C_Org", "A_Rspct", "C_Rspnbl", "A_Cmpn", "O_CrtvImag", "E_EnerLev", "C_Prdctv", "A_Trust", "N_Anxty", "E_Scblty")

BFI.facet <- f.BFI.wide %>%
  gather(key = item, value = value, A1:O6) %>%
  # mutate(itemnum = str_extract(item, "[0-9]+"),
  #        trait = str_replace(item, "[0-9]+", ""),
  #        facet = mapvalues(itemnum, 1:12, rep(1:3, each = 4))) %>%
  # group_by(SID, trait, facet, all_beep) %>%
  # summarize(value = mean(value))
  left_join(BFI %>% filter(!is.na(item)) %>% 
              select(SID, Hour, Minute, item, facet, trait)) %>%
  group_by(SID, all_beeps, facet, trait) %>%
  summarize(value = mean(value, na.rm = T)) %>% 
  ungroup() %>%
  filter(!is.na(facet)) %>%
  mutate(value = ifelse(is.nan(value) == T, NA_real_, value),
         facet2 = mapvalues(facet, unique(facet), short.facets)) %>%
  select(SID, all_beeps, facet2, value) %>%
  spread(key = facet2, value = value)

mi <- data.frame(unclass(BFI.facet)) %>%
  amelia(., m = 1, ts = "all_beeps", cs = "SID")

BFI.mi <- mi$imputations[[1]]

DS8.wide <- DS8 %>% filter(!is.na(item)) %>% 
  select(-trial_index, -trait, -facet, -answer) %>%
  spread(key = item, value = responses2) %>%
  mutate_at(vars(D1:D8), funs(as.numeric))

emo.wide <- emotion %>% filter(!is.na(item)) %>% 
  select(-trial_index, -trait, -facet, -answer) %>%
  spread(key = item, value = responses2) %>%
  mutate_at(vars(E_afraid:E_tired), funs(as.numeric))

sit.wide <- unique(sit %>% filter(!is.na(item)) %>%
  select(-trial_index, -item, -facet, -answer)) %>%
  rename(item = trait) %>%
  group_by(item, type, SID, Date, Hour, Minute) %>% 
  summarize(responses2 = mean(as.numeric(responses2))) %>%
  spread(key = item, value = responses2) %>%
  mutate_at(vars(AnxSWk:TV), funs(as.numeric))

RAT.summ <- RAT %>% filter(!is.na(item) & item != "item") %>% 
  mutate(responses2 = str_to_lower(responses2),
         responses2 = str_replace(responses2, " $", ""),
         responses2 = ifelse(responses2 == "0", "", responses2)) %>%
  separate(item, sep = " / ", c("i1", "i2", "i3")) %>%
  mutate(responses2 = str_replace(responses2, i1, ""),
         responses2 = str_replace(responses2, i2, ""),
         responses2 = str_replace(responses2, i3, ""),
         responses2 = str_replace(responses2, " ", ""),
         accuracy = ifelse(responses2 == answer, 1, 0),
         accuracy = ifelse(trial_index == 0, accuracy*1,
                    ifelse(trial_index == 1, accuracy*2,
                    ifelse(trial_index == 2, accuracy*3, NA)))) %>%
  group_by(SID, Hour, Minute) %>% 
  summarize(accuracy = sum(accuracy)/6)

subs <- unique(BFI$SID)

subs <- as.character((BFI.wide %>% group_by(SID) %>% summarize(n = n()) %>% filter(n >= 10))$SID)

for (i in 13:length(subs)) {
  BFI.i     <- BFI     %>% filter(SID == subs[i])
  BFI.mi.i  <- BFI.mi  %>% filter(SID == subs[i])
  DS8.i     <- DS8     %>% filter(SID == subs[i])
  emo.i     <- emotion %>% filter(SID == subs[i])
  sit.i     <- sit     %>% filter(SID == subs[i])
  RAT.i     <- RAT     %>% filter(SID == subs[i])
  condition <- "Procrastination"
  render(paste0(wd, '/results/', "Rmd_Output.Rmd"),
         output_file = paste0(wd, '/results/reports/', 'report.', subs[i], '.html'))
}
