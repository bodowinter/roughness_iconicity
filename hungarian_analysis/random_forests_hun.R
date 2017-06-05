## Marton Soskuthy, based on original code from Bodo Winter
## 5 June, 2017
## Analysis of /r/ vs roughness, with major snippets taken from Bodo's PhD thesis:
## https://github.com/bodowinter/phd_thesis/
## Random forests

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Load in libraries:

mainPath <- '~/documents/research/current/2017/bodo_rough/github/roughness_iconicity/hungarian_analysis'
setwd(mainPath)
source('../libraries.R')

## Set options:

options(stringsAsFactors = F)

## Load in data:

hun <- read.csv('hun_roughness_ratings.csv')

## Get phoneme extraction functions:

source("phoneme_extraction_hun.R")

## Create a binary split of roughness norms to ...
## ... turn random forest into a classification problem:

hun <- mutate(hun,
	RoughCat = ifelse(hun_roughness >= 1, 'rough', 'smooth'),
	RoughCat = as.factor(RoughCat))

## Create separate column with only onset of first syllable
## (Hungarian has consistent initial stress)

hun <- mutate(hun, StressedOnset = str_match(hun_phonemes, "(.*?)[IiYyEeØøAaOoUu]")[,2])

## Function for checking accuracy of random forest:

rforest_acc <- function(x, y) {
	xtab <- table(x, y)
	return(sum(diag(xtab)) / sum(xtab))
	}

seed(123)

## Perform random forest
##    (1): first onset only

initial.cs <- get_consonants(hun$StressedOnset)
# remove columns with all 0's:
initial.cs <- initial.cs[,colSums(initial.cs) > 0]
nvar.initial <- ncol(initial.cs)
forest.formula.initial <- as.formula(paste("RoughCat ~", paste(colnames(initial.cs), collapse=" + ")))
myfor.initial <- cforest(forest.formula.initial,
                         data = cbind(hun, initial.cs),
                         controls = cforest_unbiased(ntree = 2000, mtry = floor(sqrt(nvar.initial))))

##    (2): all consonants

all.cs <- (get_consonants(hun$hun_phonemes) > 0) * 1  ## just presence of consonants, not counts
nvar.all <- ncol(all.cs)
forest.formula.all <- as.formula(paste("RoughCat ~", paste(colnames(all.cs), collapse=" + ")))
myfor.all <- cforest(forest.formula.all,
                      data = cbind(hun, all.cs),
                      controls = cforest_unbiased(ntree = 2000, mtry = floor(sqrt(nvar.all))))



## Extract predictions:

rough.preds.initial <- predict(myfor.initial)
rough.preds.all <- predict(myfor.all)

## Check accuracy:

baseline <- max(table(hun$RoughCat)/nrow(hun)) # 50.6%
rforest_acc(rough.preds.initial, hun$RoughCat)	# 58%
rforest_acc(rough.preds.all, hun$RoughCat)	# 74%

## Extract variable importances:

rough.varimps.initial <- sort(varimp(myfor.initial, conditional = T))
rough.varimps.all <- sort(varimp(myfor.all, conditional = T))


# Create plot for all vars - Bodo: if you want, this can easily be swapped to
#    just stressed onsets
rough.varimps <- rough.varimps.all

# setting up IPA labels:
labs <- names(rough.varimps)
labs <- gsub("J", "tʃ",
             gsub("T", "ts", 
                  chartr("DSZH", "ɟʃʒɲ", labs)
             )
        )


## Plot raw variable importance:

quartz('', 9, 6)
par(mai = c(1, 1, 0.15, 0.5))
plot(1, 1, type = 'n', bty = 'n',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
	xlim = c(-0.002, 0.05), ylim = c(0.25, 23.25))
abline(h = 1:length(rough.varimps), col = 'darkgrey')
points(rough.varimps, 1:length(rough.varimps), pch = 15, cex = 1.5)
axis(side = 1, at = seq(0, 0.05, 0.01),
	font = 2, cex.axis = 1.25, lwd = 2, lwd.ticks = 2)
axis(side = 2, at = 1:length(rough.varimps),
	las = 2, font = 2, tick = F, cex.axis = 1.15, labels = labs,
	line = -0.5)
segments(x0 = 0, y0 = 0, y1 = 23.75, lwd = 2, lty = 2)
mtext(side = 1, 'Variable Importance', line = 3.25, cex = 2, font = 2)




