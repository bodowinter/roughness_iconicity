## Bodo Winter
## October 21, 2016; Adapted June 2, 2017
## Analysis of /r/, with major snippets taken from PhD thesis:
## https://github.com/bodowinter/phd_thesis/
## Random forests

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Load in libraries:

mainPath <- '/Users/winterb/Research/senses_sensory_modalities/r_is_for_rough/analysis/'
setwd(mainPath)
source('libraries.R')

## Set options:

options(stringsAsFactors = F)

## Load in data:

setwd('/Users/winterb/Research/senses_sensory_modalities/r_is_for_rough/analysis/data')
stadt <- read.csv('stadt_cleaned_pron.csv')

## Get phoneme extraction functions:

source(file.path(mainPath, 'phoneme_extraction.R'))

## Create a binary split of roughness norms to ...
## ... turn random forest into a classification problem:

stadt <- mutate(stadt,
	RoughCat = ifelse(Roughness >= 1, 'rough', 'smooth'),
	RoughCat = as.factor(RoughCat))

## Function for checking accuracy of random forest:

rforest_acc <- function(x, y) {
	xtab <- table(x, y)
	return(sum(diag(xtab)) / sum(xtab))
	}

### EVENTUALLY I NEED RANDOM FORESTS FOR ALL THE DIFFERENT VERSIONS
## (THAT IS, ONSET, CODA ETC.) ... and compare accuracy:

## Perform random forest:

nvar <- ncol(select(cbind(stadt, get_consonants(stadt$StressedOnset)), T:tS))
myfor <- cforest(RoughCat ~ T + f + v + p + t + k + b + d + g +
	 S + Z + s + r + l + m + n + w + h + tS,
	 data = cbind(stadt, get_consonants(stadt$StressedOnset)),
	 controls = cforest_unbiased(ntree = 2000, mtry = floor(sqrt(nvar))))

## Extract predictions:

rough.preds <- predict(myfor)

## Check accuracy:

rforest_acc(rough.preds, stadt$RoughCat)	# 68%

## Extract variable importances:

rough.varimps <- varimp(myfor, conditional = T)	# check later
rough.varimps <- sort(rough.varimps)

## Assign actual IPA characters:

rough.varimps_names <- c('s', 'θ', 'v', 'p', 't', 'd', 'ʃ', 'ʒ',
	'm', 'n', 'ʋ', 'h', 'tʃ', 'g', 'b', 'f', 'k', 'l', 'r')

## Plot raw variable importance:

quartz('', 9, 6)
par(mai = c(1, 1, 0.15, 0.5))
plot(1, 1, type = 'n', bty = 'n',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
	xlim = c(-0.002, 0.05), ylim = c(0.25, 19.25))
abline(h = 1:length(rough.varimps), col = 'darkgrey')
points(rough.varimps, 1:length(rough.varimps), pch = 15, cex = 1.5)
axis(side = 1, at = seq(0, 0.05, 0.01),
	font = 2, cex.axis = 1.25, lwd = 2, lwd.ticks = 2)
axis(side = 2, at = 1:length(rough.varimps),
	las = 2, font = 2, tick = F, cex.axis = 1.15, labels = rough.varimps_names,
	line = -0.5)
segments(x0 = 0, y0 = 0, y1 = 20, lwd = 2, lty = 2)
mtext(side = 1, 'Variable Importance', line = 3.25, cex = 2, font = 2)




