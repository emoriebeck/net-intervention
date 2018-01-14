new_sub_fun <- function(path = "/Volumes/beck/IPCS", input = "template", subject){
  
  ## read in and modify html
  rawHTML <- paste(readLines(sprintf("%s/%s.html", path, input)), collapse="\n")
  rawHTML <- stringr::str_replace(rawHTML, "SID_name = \"template\";", sprintf("SID_name = \"%s\";", subject))
  rawHTML <- stringr::str_replace_all(rawHTML, "template.csv", sprintf("%s.csv", subject))
  writeLines(rawHTML, paste(path, sprintf("%s.html", subject), sep = "/"))
  
  ## read in and modify csv
  RATcsv <- read.csv(sprintf("%s/rat/%s.csv", path, input), header = T, stringsAsFactors = F)
  RATcsv$used <- ""
  # write.csv(RATcsv, file = sprintf("%s/rat/%s.csv", path, subject), row.names = F)
}

# path to server on a mac: "/Volumes/beck/IPCS/rat"
# template file should be called: "template.csv" or "template.html"

many_new_subs <- function(path = "/Volumes/beck/IPCS", input = "template", subs){
  lapply(subs, function(x){new_sub_fun(path, input, x)})
}
