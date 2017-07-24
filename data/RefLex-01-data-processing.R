# RefLex data: processing
# -----------------------
# Mark Dingemanse 2017


# Clear workspace
rm(list=ls())

# check for /in/ and /out/ directories (create them if needed)
add_working_dir <- function(x) { if(file.exists(x)) { cat(x,"dir:",paste0(getwd(),"/",x,"/")) } else { dir.create(paste0(getwd(),"/",x)) 
  cat("subdirectory",x,"created in",getwd()) } }
add_working_dir("in")
add_working_dir("out")
basedir <- getwd()
indir <- paste0(getwd(),"/in")

# Packages and useful functions
list.of.packages <- c("tidyverse","readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

`%notin%` <- function(x,y) !(x %in% y) 

# Load data
# ---------

setwd(indir)

file.list <- list.files(path = indir, pattern="*.xlsx")
d.raw <- bind_rows(lapply(file.list, read_excel,col_types=c(rep("text",24))))

setwd(basedir)
d.meta <- read_excel(paste0(basedir,"/reflex_meta_atleast5000records.xlsx"))

# Combine data and metadata, clean up colnames, set data types
d <- left_join(d.raw,d.meta[,c(9:16,18)], by="langue")
names(d) <- tolower(names(d))
tofactors <- c("cgr","cgo","langue","language","metalanguage")
d[,tofactors] <- lapply(d[,tofactors],as.factor)

# remove rows that have only "see also" content; split fr / en translations where relevant, populate tra_fr and tra_en  
d <- d %>% 
  filter(cgr %notin% c(">")) %>%      
  group_by(metalanguage) %>% do({     
  if(.$metalanguage[1] == "en/fr") 
    separate(., tra, c("tra_en", "tra_fr"), " / ", remove=FALSE) 
  else if(.$metalanguage[1] == "en")
    mutate(., tra_en = tra)
  else
    mutate(., tra_fr = tra)
  }) %>% as.data.frame()


# TO DO clean up grammatical category

unique(d$cgr) # cgr is a bit of mess for a 'unified' grammatical category
d %>%
  group_by(cgr) %>%
  summarise(count=n()) %>%
  View()

# Save
setwd(basedir)
save(d, file="reflex-data.Rdata")
