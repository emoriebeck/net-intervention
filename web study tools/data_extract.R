library(tidyverse)
library(stringr)

extract_fun <- function(x){
  z <- str_split(x, "[,]")[[1]]
  if(grepl("undefined", z[2])){z <- z[-2]}
  z <- str_replace(z, '\":\"\"', '":0"')
  len <- length(z)
  str_c(combine(lapply(1:len, function(a){str_split(str_replace_all(z[a], '["]', ""),":")[[1]][2]})), collapse=",")
}

clean_fun <- function(x){
  if(any(colnames(x) == "0") | x$type == "RAT"){
    if(any(colnames(x) == "0")){
    x <- x %>% select(-item) %>% rename(item = `0`)}
  }
  if(x$trial_type == "survey-checkbox"){
    x <- x %>% mutate(responses2 = str_replace_all(responses2, ",,", ",0,"))
  }
  y <- x  %>% select(item, trait, facet, answer, type, responses2)
  cols <- colnames(y)[apply(y, 2, function(x) grepl("[,]", x))]
  cols <- which(colnames(y) %in% cols)
  if(length(cols) > 0){
    y <- y %>% separate_rows(cols, sep = ",", convert = T)
  }
  if(any(is.na(y$answer))){y <- y %>% mutate(answer = NA_character_)}
  y <- y %>% select(item, trait, facet, answer, type, responses2) %>%
    mutate_all(funs(ifelse(is.na(.) == T, NA_character_, .))) %>%
    mutate(SID = sub, Date = as.Date(date, "%m-%d"), Hour = hour, Minute = minute,
           responses2 = as.character(responses2))
}

wd <- "/Volumes/beck/IPCS/data/"
files <- list.files(wd)

# files <- sample(files, 10)

for(i in 1:length(files)){
  file <- files[i]
  print(sprintf("%s:%s", i, file))
  sub <- str_split(file, "_")[[1]][1]
  date <- str_split(file, "_")[[1]][2]
  hour <- str_split(file, "_")[[1]][3]
  minute <- str_split(file, "_")[[1]][4]
  file <- paste(wd,file,sep="")
  dat_in <- read_csv(file) %>%
    mutate(responses2 = map(responses, possibly(extract_fun, NA_real_)),
           responses2 = str_replace(responses2, "\\}", ""))
  if(any(colnames(dat_in) == "platform")){
    if(dat_in$platform == "MacIntel"){next}
  }
  dat_in_long <- dat_in %>%
    group_by(trial_index) %>% nest() %>%
    mutate(responses2 = map(data, clean_fun)) %>% 
    unnest(responses2, .drop = T)
  if(i == 1){dat <- dat_in_long}
  else{dat <- dat %>% full_join(dat_in_long)}
}

BFI <- dat %>% filter(type == "BFI2")
emotion <- dat %>% filter(type == "emotion")
RAT <- dat %>% filter(type == "RAT")
DS8 <- dat %>% filter(type == "DIAMONDS")
sit <- dat %>% filter(type == "situation")
memory <- dat %>% filter(is.na(type)) %>% mutate(type = "memory")

save(BFI, emotion, RAT, DS8, sit, memory, file = sprintf("/Volumes/beck/IPCS/clean data/clean_data_%s.RData", Sys.Date()))

