# RefLex data: basic processing
# -----------------------------
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

# Details about RefLex data
# -------------------------
# The lexical data processed here is sampled from RefLex, cited as follows:
# Segerer G., Flavier S., 2011-2016 RefLex: Reference Lexicon of Africa, Version 1.1. Paris, Lyon. http://reflex.cnrs.fr/

# The sample aimed for is a convenience sample selected with an eye for (1) 
# lexicon size and (2) phylogenetic diversity. We first selected sources with 
# >5000 records in RefLex; there were 36 of these in July 2017. Within that 
# sample we prioritised sources according to phylogenetic diversity, striving to
# select representatives from the major African phyla and subphyla recognised by
# Glottolog (http://glottolog.org).

# When there are multiple sources for one languoid or tight cluster (e.g. Wolof,
# Sereer, Fulfulde), we prioritise the source with the biggest number of records
# and exclude the others to avoid duplicate or near-duplicate lemmas. When there
# are sources for multiple languages in a larger group (e.g. Bantu, Dogon), we
# select multiple based on number of records and group-internal diversity.

# RefLex metadata was enriched by adding ISO codes, Glottolog 3.0 codes, language names, and full citations to the original sources.


# Load data ---------------------------------------------------------------

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


# Clean up data -----------------------------------------------------------

# remove rows that have only "see also" or otherwise redundant content; split fr
# / en translations where relevant, populate tra_fr and tra_en (the separate()
# statement throws expected errors).
d <- d %>% 
  filter(cgr %notin% c(">","Nv","var")) %>%
  group_by(metalanguage) %>% do({     
  if(.$metalanguage[1] == "en/fr") 
    separate(., tra, c("tra_en", "tra_fr"), " / ", remove=FALSE) 
  else if(.$metalanguage[1] == "en")
    mutate(., tra_en = tra)
  else
    mutate(., tra_fr = tra)
  }) %>% as.data.frame()



# Clean up grammatical category by adding unified and simplified fields

# dput(levels(d$cgr))
# unique(d[which(d$cgr == "Vd"),]$language)
# length(d[which(d$cgr == "cl"),]$language)
# unique(d[which(d$cgr == "part"),c("fun","tra","cgr","language")])
# View(unique(d[which(d$cgr == "var"),c("fun","tra","cgr","language")]))

# After harmonising upper/lowercase, data has been recoded as follows: N/adj ->
# N (10 cases); N/adv -> adv (15 cases); {"N, V",V N} -> V; {B, N?, N<, Ndep,
# NP, NPc, Np} -> N; adv conj -> conj; "PART +" -> NEG (1 case); onom -> id;
# {adp, post, postp, prep} -> adp; {pr, pro} -> pro; qlt -> adj; qnt -> adv; rel
# -> pro; temp -> adv; var -> NA; part, pr -> pcl; {Aux, aux, Vaux, cop, COP} ->
# V

# Explanation of less common categories: adp adposition, exp expression, id
# ideophone, gr grammatical morpheme, Nc nominal compound, pcl particle

cgu_levels <- c("V", NA, NA, NA, "adj", "adj", "adj", "adj", "adj", 
                "adj", "adp", "adv", "conj", "adv", "V", "N", "cl", "cl", 
                "cl", "conj", "conn", "cop", "cop", "dem", "dem", "gr", "det", 
                "exp", "gr", "id", "id", "det", "interrog", "interrog", "intj", "intj", 
                "loc", "N", "N", "N", "N", "N", "adv", "N", 
                "adv", "N", "V", "N", "N", "N", "Nc", "Nc", "N", 
                "N", "N", "neg", "neg", "N", "N", "N", "N", "num", 
                "num", "N", "id", "id", "pcl", "pcl", "neg", "poss", 
                "poss", "adp", "adp", "adp", "pro", "adp", "pro", "adj", 
                "adv", "pro", "adv", "adv", "V", "V", "V", "V", "V", 
                "V", "V", NA, "V", "V", "V", "V", "V")
d$cgu <- d$cgr
levels(d$cgu) <- cgu_levels
dput(levels(d$cgu))

# pos = cgu simplified again. All grammatical morphemes lumped together under
# grm, all function words under fun, leaving only 9 main pos categories
pos_levels <- c("V", "adj", "fun", "adv", "fun", "N", "grm", "fun", "V", 
                "grm", "grm", "grm", "exp", "id", "fun", "intj", "fun", 
                "N", "grm", "fun", "fun", "grm", "grm")
d$pos <- d$cgu
levels(d$pos) <- pos_levels

d %>%
  group_by(pos) %>%
  summarise(count=n()) %>%
  View()


# Save --------------------------------------------------------------------

save(d, file="reflex-data.Rdata")
write.table(d,file="reflex-data.txt",fileEncoding = "UTF-8")
