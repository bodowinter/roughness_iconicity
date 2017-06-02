## Bodo Winter
## October 21, 2016; Adapted June 2, 2017
## Analysis of /r/, with major snippets taken from PhD thesis:
## https://github.com/bodowinter/phd_thesis/
## Code for phonemic features

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



##------------------------------------------------------------------
## Extract phonemic features: single phonemes
##------------------------------------------------------------------

## Get phoneme extraction functions:

source(file.path(mainPath, 'phoneme_extraction.R'))

## Explanation of next section:

## onsetCons only contains onset consonant matrix
## onsetVowel contains vowel matrix
## onsetWithVowel is the onset + nucleus
## codaCons has the coda consonants
## fullCons has all consonants in the word
## fullStress has all consonants of stressed syllable (onset + coda)
## fullSyll has vowel of stressed syllable plus all consonants of stressed

## Extract 'em:

onsetCons <- get_consonants(stadt$StressedOnset)
onsetVowel <- get_vowels(stadt$StressedVowel)
onsetWithVowel <- cbind(onsetCons, onsetVowel)
codaCons <- get_consonants(stadt$Coda)
fullCons <- get_consonants(stadt$Consonantal)
fullStress <- get_consonants(stadt$Stressed)
fullSyll <- cbind(fullStress, get_vowels(stadt$StressedVowel))
## 'barbed' is the only word that has twice the same consonant in the root for 'fullCons'

## Get rid of those phonemes that aren't in the onset/coda at all anyway:

onsetCons <- onsetCons[, -which(colSums(onsetCons) == 0)]
onsetVowel <- onsetVowel[, -which(colSums(onsetVowel) == 0)]
onsetWithVowel <- onsetWithVowel[, -which(colSums(onsetWithVowel) == 0)]
codaCons <- codaCons[, -which(colSums(codaCons) == 0)]
fullCons <- fullCons[, -which(colSums(fullCons) == 0)]
fullStress <- fullStress[, -which(colSums(fullStress) == 0)]
fullSyll <- fullSyll[, -which(colSums(fullSyll) == 0)]

## Compute distance matrices in form space:

onsetCons.dist <- dist(onsetCons, method = 'manhattan')
onsetVowel.dist <- dist(onsetVowel, method = 'manhattan')
onsetWithVowel.dist <- dist(onsetWithVowel, method = 'manhattan')
codaCons.dist <- dist(codaCons, method = 'manhattan')
fullCons.dist <- dist(fullCons, method = 'manhattan')
fullStress.dist <- dist(fullStress, method = 'manhattan')
fullSyll.dist <- dist(fullSyll, method = 'manhattan')

## Compute roughness distance:

rough.dist <- dist(stadt$Roughness,
	method = 'euclidian')

## Compute Mantel test correlations:

mantel.randtest(rough.dist, onsetCons.dist)	# r = 0.11 * 
mantel.randtest(rough.dist, onsetVowel.dist)	# r = -0.02
mantel.randtest(rough.dist, onsetWithVowel.dist)	# r = 0.07
mantel.randtest(rough.dist, codaCons.dist)	# r = -0.03
mantel.randtest(rough.dist, fullStress.dist)	# r = 0.05 *
mantel.randtest(rough.dist, fullSyll.dist)	# r = 0.04

## Do the same thing with Levenshtein edit distances:

onsetCons.lev <- as.dist(adist(stadt$StressedOnset))
onsetVowel.lev <- as.dist(adist(stadt$StressedVowel))
onsetWithVowel.lev <- as.dist(adist(paste0(stadt$Coda, stadt$StressedVowel)))
codaCons.lev <- as.dist(adist(stadt$Coda))
fullCons.lev <- as.dist(adist(stadt$Consonantal))
fullStress.lev <- as.dist(adist(stadt$Stressed))
fullSyll.lev <- as.dist(adist(paste0(stadt$Stressed, stadt$StressedVowel)))

adist(paste0(stadt$StressedOnset, stadt$StressedVowel)))

## Perform correlations with Levenshtein edit distances:

mantel.randtest(rough.dist, onsetCons.lev)	# r = 0.09 *
mantel.randtest(rough.dist, onsetVowel.lev)	# r = 0.05 n.s.
mantel.randtest(rough.dist, onsetWithVowel.lev)	# r = ~0
mantel.randtest(rough.dist, codaCons.lev)	# r = -0.03
mantel.randtest(rough.dist, fullCons.lev)	# r = 0.06 *
mantel.randtest(rough.dist, fullStress.lev)	# r = 0.04
mantel.randtest(rough.dist, fullSyll.lev)	# r = 0.05



##------------------------------------------------------------------
## Make a plot of the distance correlations:
##------------------------------------------------------------------

## Make a plot of this:

set.seed(542)
quartz('', 9, 6.5)
par(mai = c(1.5, 2, 1.25, 0.25))
plot(1, 1, type = 'n',
	xlim = c(0, 14), ylim = c(-0.5, 5.5),
	xlab = '', ylab = '',
	xaxt = 'n', yaxt = 'n', bty = 'n')
## Axes:
axis(side = 1, at = seq(0, 14, 3.5), font = 2,
	lwd.ticks = 4, lwd = 4, labels = F)
axis(side = 1, at = seq(0, 14, 3.5),
	font = 2, tick = F, labels = T, cex.axis = 2, line = 0.5)
mtext(text = 'Roughness Distance', side = 1,
	line = 5, font = 2, cex = 2.5)
axis(side = 2, at = seq(0, 5, 1), font = 2,
	lwd.ticks = 4, cex.axis = 2, las = 2, lwd = 4)
mtext(text = 'Form Distance', side = 2,
	line = 4, font = 2, cex = 2.5)
## The data:
points(x = rough.dist,
	y = jitter(onset.dist),
	pch = 21, cex = 1.25,
	bg = rgb(0, 0, 0, 0.4), col = NA)



##------------------------------------------------------------------
## Find contributing words:
##------------------------------------------------------------------

## Maximum correlation taken as gold standard:

r <- mantel.randtest(rough.dist, onsetCons.dist)$obs

## Perform mantel tests again without each word and store drop in correlation:

all_r <- numeric(nrow(stadt))
for (i in 1:nrow(stadt)) {
	this_sound <- dist(onsetCons[-i, ], method = 'manhattan')
	this_meaning <- dist(stadt[-i, ]$Roughness, method = 'euclidian')
	all_r[i] <- mantel.randtest(this_sound, this_meaning)$obs
	}
stadt$r_diff <- all_r - r	# positive number = correlation increased

## Look at results and save:

dplyr::select(stadt, Word, r_diff) %>% arrange(r_diff) %>%
	mutate(r_diff = round(r_diff, 3)) -> word_cors
write.table(word_cors, file = 'word_cors.csv', sep = ',', row.names = F)

## How many words contributed to the correlation?

sum(word_cors$r_diff < 0)	# 38 that lead to a drop in 'r'
sum(word_cors$r_diff < 0) / nrow(word_cors)	# 38%

## This suggests that there is a set of 'participating words' ...
## ... and a set of 'non-participating words'.
## How does the correlation look within these words only?

stadt_red <- filter(stadt, r_diff < 0)
stadt_red.OnsetCons <- get_consonants(stadt_red$StressedOnset)
stadt_red.OnsetCons.dist <- dist(stadt_red.OnsetCons,
	method = 'manhattan')
stadt_red.rough.dist <- dist(stadt_red$Roughness,
	method = 'euclidian')
mantel.randtest(stadt_red.rough.dist, stadt_red.OnsetCons.dist)	# r = 0.71



##------------------------------------------------------------------
## Do contributing words correlate with iconicity ratings?
##------------------------------------------------------------------

## Merge iconicity norms into this dataset:

icon <- read.csv('iconicity_ratings.csv')
stadt <- left_join(stadt, icon)

## Correlate:

with(stadt, cor(r_diff, Iconicity))	# r = -0.21
with(stadt, cor.test(r_diff, Iconicity))	# p = 0.03, yes!

## Plot this correlation, first color ramp blue-gold:

icon01 <- (stadt$Iconicity - min(stadt$Iconicity)) / diff(range(stadt$Iconicity))
grad.fnc <- colorRamp(c('steelblue', 'goldenrod3'))

## Construct a linear model:

summary(xmdl_plot <- lm(Iconicity ~ r_diff, data = stadt))

## Get predictions for linear model:

newdata <- data.frame(r_diff = seq(-0.02, 0.02, 0.0001))
newdata <- cbind(newdata,
	as.data.frame(predict(xmdl_plot, newdata, se.fit = T)[1:2]))
newdata$UB <- newdata$fit + 1.96 * newdata$se.fit
newdata$LB <- newdata$fit - 1.96 * newdata$se.fit

## Make a plot:
######## (SOMETHING IS OFF HERE, NEED TO FIX):

quartz('', 9, 6.5)
par(mai = c(1.5, 2, 1.25, 0.25))
plot(1, 1, type = 'n',
	xlim = c(-0.02, 0.02), ylim = c(-2.5, 5),
	xlab = '', ylab = '',
	xaxt = 'n', yaxt = 'n', bty = 'n')
axis(side = 1, at = seq(-0.02, 0.02, 0.01), font = 2,
	lwd.ticks = 4, lwd = 4, labels = F)
axis(side = 1, at = seq(-0.02, 0.02, 0.01),
	font = 2, tick = F, labels = T, cex.axis = 2, line = 0.5)
mtext(text = 'Change in correlation', side = 1,
	line = 5, font = 2, cex = 2.5)
axis(side = 2, at = seq(-2.5, 5, 2.5), font = 2,
	lwd.ticks = 4, cex.axis = 2, las = 2, lwd = 4)
mtext(text = 'Iconicity ratings', side = 2,
	line = 5, font = 2, cex = 2.5)
## Regression line:
polygon(x = c(newdata$r_diff, rev(newdata$r_diff)),
	y = c(newdata$UB, rev(newdata$LB)),
	border = NA, col = rgb(0, 0, 0, 0.2))
points(x = newdata$r_diff, y = newdata$fit, lwd = 4, type = 'l')
## Points second set:
text(x = stadt$r_diff,
	y = stadt$Iconicity,
	pch = 21, cex = 2,
	labels = stadt$Word,
	bg = rgb(grad.fnc(icon01), alpha = 180, max = 255), col = NA)



##------------------------------------------------------------------
## Find contributing phonemes:
##------------------------------------------------------------------

## Maximum correlation taken as gold standard:

r <- mantel.randtest(rough.dist, onsetCons.dist)$obs

## Perform mantel tests again without each word and store drop in correlation:

all_r <- numeric(ncol(onsetCons))
all_p <- numeric(ncol(onsetCons))
for (i in 1:ncol(onsetCons)) {
	this_sound <- dist(onsetCons[, -i], method = 'manhattan')
	all_r[i] <- mantel.randtest(this_sound, rough.dist)$obs
	all_p[i] <- mantel.randtest(this_sound, rough.dist)$pvalue
	}
r_phon <- data.frame(Phoneme = colnames(onsetCons),
	r_drop = all_r - r, p_val = all_p)
r_phon <- arrange(r_phon, r_drop) %>% mutate(r_drop = round(r_drop, 4))
write.table(r_phon, 'phon_cors.csv', sep = ',', row.names = F)




##------------------------------------------------------------------
## Test roughness difference based on phoneme:
##------------------------------------------------------------------

## Do this systematically for all phonemes, Bonferroni corrected:

stadt.cons <- get_consonants(stadt$StressedOnset)
stadt.cons <- stadt.cons[, -which(colSums(stadt.cons) == 0)]

## Datasets to store results of loop in:

allphons <- colnames(stadt.cons)
statements <- numeric(ncol(stadt.cons))
all.cohen <- numeric(ncol(stadt.cons))

## Function for correcting multiple comparisons:

dunnsidak <- function (pval, ntest) 1 - ((1 - pval) ^ ntest)

## Run the loop:

for (i in 1:length(allphons)) {
	xtest <- try(t.test(stadt$Roughness ~ stadt.cons[, i],
		var.equal = T), silent = T)
	xcohen <- try(cohen.d(stadt$Roughness ~ stadt.cons[, i]),
		silent = T)
	if (class(xtest) == 'try-error') { statements <- ''
		} else {
			tval <- paste0('t = ', round(xtest$statistic, 2))
			df <- paste0('df = ', xtest$parameter)
			pval <- paste0('p = ', round(dunnsidak(xtest$p.value,
				ntest = ncol(stadt.cons)), digits = 3) )
			xall <- paste(c(tval, df, pval), collapse = ', ')
			statements[i] <- xall
			all.cohen[i] <- xcohen$estimate
			}
	}
(xbyphoneme <- data.frame(Phoneme = allphons,
	Statement = statements, Cohen.D = all.cohen))

## Descriptive percentages by binary split:

stadt <- mutate(stadt,
	RoughCat = ifelse(Roughness >= 0, 'rough', 'smooth'))
stadt <- mutate(stadt,
	RoughCat = as.factor(RoughCat))
stadt.forsplit <- cbind(stadt, stadt.cons)
r.tab <- with(stadt.forsplit, table(RoughCat, r))
s.tab <- with(stadt.forsplit, table(RoughCat, s))
l.tab <- with(stadt.forsplit, table(RoughCat, l))

round(prop.table(r.tab, 1), 2)
round(prop.table(s.tab, 1), 2)
round(prop.table(l.tab, 1), 2)




##------------------------------------------------------------------
## Report descriptive means:
##------------------------------------------------------------------

stadt <- mutate(stadt, RoughCat = ifelse(Roughness > 0, 'rough', 'smooth'))
stadt2 <- cbind(stadt, get_consonants(stadt$StressedOnset))

prop.table(table(stadt2$RoughCat, stadt2$r), 1)
prop.table(table(stadt2$RoughCat, stadt2$l), 1)
prop.table(table(stadt2$RoughCat, stadt2$s), 1)



##------------------------------------------------------------------
## Analysis of roughness by position for /r/:
##------------------------------------------------------------------

## Types of /r/ for onset:

stadt$R_OnsetType <- 'no r'
stadt[stadt$StressedOnset == 'r', ]$R_OnsetType <- 'r initial'
r_any <- stadt[grep('r', stadt$StressedOnset), ]$StressedOnset
r_clust <- unique(r_any[r_any != 'r'])
stadt[stadt$StressedOnset %in% r_clust, ]$R_OnsetType <- 'r cluster'

## /r/ in coda or not:

stadt$R_CodaType <- 'no r'
stadt[stadt$Coda == 'r', ]$R_CodaType <- 'r only'
r_coda_types <- stadt$Coda[grep('r', stadt$Coda)]
r_codas <- r_coda_types[r_coda_types != 'r']
stadt[stadt$Coda %in% r_codas, ]$R_CodaType <- 'r combination'

## /r/ in onset versus coda:

stadt$R_Both <- 'no r'
coda_rs <- c('r combination', 'r')
stadt[!(stadt$R_OnsetType %in% c('r initial', 'r cluster')) & stadt$R_CodaType %in% coda_rs, ]$R_Both <- 'coda only'
stadt[stadt$R_OnsetType %in% c('r initial', 'r cluster') & !(stadt$R_CodaType %in% coda_rs), ]$R_Both <- 'onset only'

## Since there is no word that has r's in both onset and coda, we can merge categories
# str_count(stadt$Consonantal, 'r')	 # (all either 0 or 1)

stadt$R_Type <- stadt$R_OnsetType
stadt[stadt$R_CodaType %in% coda_rs, ]$R_Type <- 'r coda'

## Quick and dirty boxplot:

boxplot(Roughness ~ R_Type, stadt)

## Aggregates:

arrange(aggregate(Roughness ~ R_OnsetType, stadt, mean), desc(Roughness))
aggregate(Roughness ~ R_OnsetType, stadt, sd)

arrange(aggregate(Roughness ~ R_Type, stadt, mean), desc(Roughness))
aggregate(Roughness ~ R_Type, stadt, sd)

## ANOVA for /r/ in onset or not:

summary(xmdl.ons <- lm(Roughness ~ R_OnsetType, stadt))
anova(xmdl.ons)

## Post-hoc analysis for cluster versus initial:

r_ons_only <- stadt[stadt$R_OnsetType %in% c('r cluster', 'r initial'), ]
t.test(Roughness ~ R_OnsetType, r_ons_only, var.equal = T)
cohen.d(Roughness ~ R_OnsetType, r_ons_only, var.equal = T)

## ANOVA for /r/ in coda or not coda:

summary(xmdl.coda <- lm(Roughness ~ R_CodaType, stadt))
anova(xmdl.coda)

## Post-hoc analysis for /r/ coda cluster or not:

r_ons_only <- stadt[stadt$R_CodaType %in% c('r only', 'r combination'), ]
t.test(Roughness ~ R_CodaType, r_ons_only, var.equal = T)
cohen.d(Roughness ~ R_CodaType, r_ons_only, var.equal = T)



##------------------------------------------------------------------
## Analysis of roughness by position for /l/:
##------------------------------------------------------------------

## Types of /l/ for onset:

stadt$L_OnsetType <- 'no l'
stadt[stadt$StressedOnset == 'l', ]$L_OnsetType <- 'l initial'
l_any <- stadt[grep('l', stadt$StressedOnset), ]$StressedOnset
l_clust <- unique(l_any[l_any != 'l'])
stadt[stadt$StressedOnset %in% l_clust, ]$L_OnsetType <- 'l cluster'

## /l/ in coda or not:

stadt$L_CodaType <- 'no l'
stadt[stadt$Coda == 'l', ]$L_CodaType <- 'l only'
l_coda_types <- stadt$Coda[grep('l', stadt$Coda)]
l_codas <- l_coda_types[l_coda_types != 'l']
stadt[stadt$Coda %in% l_codas, ]$L_CodaType <- 'l combination'

## /r/ in onset versus coda:

stadt$L_Both <- 'no l'
coda_ls <- c('l combination', 'l')
stadt[!(stadt$L_OnsetType %in% c('l initial', 'l cluster')) & stadt$L_CodaType %in% coda_ls, ]$L_Both <- 'coda only'
stadt[stadt$L_OnsetType %in% c('l initial', 'l cluster') & !(stadt$L_CodaType %in% coda_ls), ]$L_Both <- 'onset only'

## Since there is no word that has l's in both onset and coda, we can merge categories
# str_count(stadt$Consonantal, 'l')	 # (all either 0 or 1)

stadt$L_Type <- stadt$L_OnsetType
stadt[stadt$L_CodaType %in% c('l combination', 'l only'), ]$L_Type <- 'l coda'

## Quick and dirty boxplot:

boxplot(Roughness ~ L_Type, stadt)

## Aggregates:

arrange(aggregate(Roughness ~ L_OnsetType, stadt, mean), desc(Roughness))
aggregate(Roughness ~ L_OnsetType, stadt, sd)

arrange(aggregate(Roughness ~ L_Type, stadt, mean), desc(Roughness))
aggregate(Roughness ~ L_Type, stadt, sd)

## ANOVA for /l/ in onset or not:

summary(xmdl.ons <- lm(Roughness ~ L_OnsetType, stadt))
anova(xmdl.ons)

## Post-hoc analysis for cluster versus initial:

l_ons_only <- stadt[stadt$L_OnsetType %in% c('l cluster', 'l initial'), ]
t.test(Roughness ~ L_OnsetType, l_ons_only, var.equal = T)
cohen.d(Roughness ~ L_OnsetType, l_ons_only, var.equal = T)

## ANOVA for /r/ in coda or not coda:

summary(xmdl.coda <- lm(Roughness ~ L_CodaType, stadt))
anova(xmdl.coda)

## Post-hoc analysis for /l/ coda cluster or not:

l_ons_only <- stadt[stadt$L_CodaType %in% c('l only', 'l combination'), ]
t.test(Roughness ~ L_CodaType, l_ons_only, var.equal = T)
cohen.d(Roughness ~ L_CodaType, l_ons_only, var.equal = T)



##------------------------------------------------------------------
## Clean plot of /r/ and /l/ distribution:
##------------------------------------------------------------------

## Model for clean plot:

stadt$R_Type <- factor(stadt$R_Type, levels = c('no r', 'r initial', 'r cluster', 'r coda'))
stadt$L_Type <- factor(stadt$L_Type, levels = c('no l', 'l initial', 'l cluster', 'l coda'))
xmdl.r <- lm(Roughness ~ R_Type, stadt)
xmdl.l <- lm(Roughness ~ L_Type, stadt)

## Get categories:

rs <- c('no r', 'r initial', 'r cluster', 'r coda')
ls <- c('no l', 'l initial', 'l cluster', 'l coda')

## Get predictions:

r.pred <- data.frame(R_Type = rs)
l.pred <- data.frame(L_Type = ls)
r.pred <- data.frame(predict(xmdl.r, newdata = r.pred, se.fit = T)[1:2])
l.pred <- data.frame(predict(xmdl.l, newdata = l.pred, se.fit = T)[1:2])

## Make plots:

xfac <- 0.1
set.seed(42)
quartz('', 11.75, 5)
par(mfrow = c(1, 2), mai = c(0.25, 0.15, 0.25, 0.05), omi = c(0.5, 1.15, 0.25, 0.25))
## Plot 1:
plot(1, 1, type = 'n', xlim = c(0.5, 4.5), ylim = c(-8, 10.5),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 10, labels = '(a)', font = 2, cex = 1.5)
abline(h = c(-7, 7), lty = 2)
axis(side = 2, at = seq(-7, 7, 3.5), las = 2, cex.axis = 1.25, lwd.ticks = 2, font = 2)
axis(side = 1, at = 1:4, labels = rs, cex.axis = 1.5, font = 2, lwd.ticks = 2)
mtext('Roughness ratings', side = 2, line = 4, cex = 2, font = 2)
for (i in 1:length(rs)) {
	this_cat <- rs[i]
	these_rs <- filter(stadt, R_Type == this_cat)$Roughness
	points(x = jitter(rep(i, length(these_rs)), 1.5), y = these_rs, pch = 21,
		bg = rgb(0, 0, 0, 0.3), col = NA)
	arrows(x0 = i + xfac, y0 = r.pred[i, ]$fit - r.pred[i, ]$fit,
		y1 = r.pred[i, ]$fit + r.pred[i, ]$fit, length = 0.08, angle = 90, code = 3, lwd = 2)
	points(x = i + xfac, y = r.pred[i, ]$fit, pch = 22, bg = 'black', col = 'black', cex = 1.25)
	}
box(lwd = 2)
## Plot 2:
plot(1, 1, type = 'n', xlim = c(0.5, 4.5), ylim = c(-8, 10.5),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
abline(h = c(-7, 7), lty = 2)
text(x = 0.6, y = 10, labels = '(b)', font = 2, cex = 1.5)
axis(side = 1, at = 1:4, labels = ls, cex.axis = 1.5, font = 2, lwd.ticks = 2)
for (i in 1:length(ls)) {
	this_cat <- ls[i]
	these_ls <- filter(stadt, L_Type == this_cat)$Roughness
	points(x = jitter(rep(i, length(these_ls)), 1.5), y = these_ls, pch = 21,
		bg = rgb(0, 0, 0, 0.3), col = NA)
	arrows(x0 = i + xfac, y0 = l.pred[i, ]$fit - l.pred[i, ]$fit,
		y1 = l.pred[i, ]$fit + l.pred[i, ]$fit, length = 0.08, angle = 90, code = 3, lwd = 2)
	points(x = i + xfac, y = l.pred[i, ]$fit, pch = 22, bg = 'black', col = 'black', cex = 1.25)
	}
box(lwd = 2)

## Test of the same without the 'no r' category:

summary(xmdl.r <- lm(Roughness ~ R_Type, filter(stadt, R_Type != 'no r')))
summary(xmdl.l <- lm(Roughness ~ L_Type, filter(stadt, L_Type != 'no l')))
anova(xmdl.r)
anova(xmdl.l)
TukeyHSD(aov(Roughness ~ R_Type, filter(stadt, R_Type != 'no r')))
TukeyHSD(aov(Roughness ~ L_Type, filter(stadt, L_Type != 'no l')))



##------------------------------------------------------------------
## Analysis of roughness by position for /s/:
##------------------------------------------------------------------

## Types of /s/ for onset:

stadt$S_OnsetType <- 'no s'
stadt[stadt$StressedOnset == 's', ]$S_OnsetType <- 's initial'
s_any <- stadt[grep('s', stadt$StressedOnset), ]$StressedOnset
s_clust <- unique(s_any[s_any != 's'])
stadt[stadt$StressedOnset %in% s_clust, ]$S_OnsetType <- 's cluster'

## /s/ in coda or not:

stadt$S_CodaType <- 'no s'
stadt[stadt$Coda == 's', ]$S_CodaType <- 's only'
s_coda_types <- stadt$Coda[grep('s', stadt$Coda)]
s_codas <- s_coda_types[s_coda_types != 's']
stadt[stadt$Coda %in% s_codas, ]$S_CodaType <- 's combination'

## /s/ in onset versus coda:

stadt$S_Both <- 'no s'
coda_ss <- c('s combination', 's')
stadt[!(stadt$S_OnsetType %in% c('s initial', 's cluster')) & stadt$S_CodaType %in% coda_ss, ]$S_Both <- 'coda only'
stadt[stadt$S_OnsetType %in% c('s initial', 's cluster') & !(stadt$S_CodaType %in% coda_ss), ]$S_Both <- 'onset only'

## Since there is no word that has l's in both onset and coda, we can merge categories
# str_count(stadt$Consonantal, 'l')	 # (all either 0 or 1)

stadt$S_Type <- stadt$S_OnsetType
stadt[stadt$S_CodaType %in% coda_ss, ]$S_Type <- 's coda'

## Quick and dirty boxplot:

boxplot(Roughness ~ S_Type, stadt)

## For 's' it really seems to matter what it is paired with:

filter(stadt, S_Type == 's cluster')
filter(stadt, S_Type == 's initial')
filter(stadt, S_Type == 's coda')

## Aggregates:

arrange(aggregate(Roughness ~ S_OnsetType, stadt, mean), desc(Roughness))
aggregate(Roughness ~ S_OnsetType, stadt, sd)

arrange(aggregate(Roughness ~ S_Type, stadt, mean), desc(Roughness))
aggregate(Roughness ~ S_Type, stadt, sd)

## ANOVA for /s/ in onset or not:

summary(xmdl.ons <- lm(Roughness ~ S_OnsetType, stadt))
anova(xmdl.ons)

## Post-hoc analysis for cluster versus initial:

s_ons_only <- stadt[stadt$S_OnsetType %in% c('s cluster', 's initial'), ]
t.test(Roughness ~ S_OnsetType, s_ons_only, var.equal = T)
cohen.d(Roughness ~ S_OnsetType, s_ons_only, var.equal = T)

## ANOVA for /s/ in coda or not coda:

summary(xmdl.coda <- lm(Roughness ~ S_CodaType, stadt))
anova(xmdl.coda)

## Post-hoc analysis for /s/ coda cluster or not:

s_ons_only <- stadt[stadt$S_CodaType %in% c('s only', 's combination'), ]
t.test(Roughness ~ S_CodaType, s_ons_only, var.equal = T)
cohen.d(Roughness ~ S_CodaType, s_ons_only, var.equal = T)


