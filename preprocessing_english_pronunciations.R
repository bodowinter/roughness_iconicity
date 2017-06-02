## Bodo Winter
## October 21, 2016; Adapted June 2, 2017
## Analysis of /r/, with major snippets taken from PhD thesis:
## https://github.com/bodowinter/phd_thesis/
## Pre-processing phonemic representations

##------------------------------------------------------------------
## Linguistic comments:
##------------------------------------------------------------------

## When looking at coda I'm not taking the maximum onset principle into account
## aBRASive  - BRAS is counted for phonemes
## I used ELP / Unisyn lexicon rather than CMU coz it also encodes stress information
## I changed 'barbed' ('Arbd') towards 'Arb' because the 'd' is morphological


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
stadt <- read.csv('stadtlander_roughness_norms.csv')
ELP <- read.csv('ELP_pronunciations.csv')

## Merge ELP pronunciations into Stadtlander dataset:

stadt <- left_join(stadt, ELP, by = 'Word')



##------------------------------------------------------------------
## Only take adjectives:
##------------------------------------------------------------------

## Get only adjectives:

SUBTL <- read.csv('SUBTLEX_POS.csv')
stadt <- left_join(stadt, SUBTL)

## Hand-fix remaining adjectives:

stadt[is.na(stadt$POS), ]$POS <- 'Adjective'
	# (all NA's turn out to be adjectives)

## Get rid of non-adjectives:

sum(!is.na(stadt$Roughness))	# 123
stadt <- filter(stadt, POS == 'Adjective')
sum(!is.na(stadt$Roughness))	# 100



##------------------------------------------------------------------
## Add additional pronunciations by hand:
##------------------------------------------------------------------

## Where possible, the root was identified in the ELP pronunciations
## In addition, MacMillan and Webster were consulted for this
## No strict syllabification has been undertaken (not relevant)

stadt[stadt$Word == 'callused',]$Pron <- 'k"al@st'
stadt[stadt$Word == 'craterous',]$Pron <- 'kr"e.4@`.r@s'
stadt[stadt$Word == 'cushiony',]$Pron <- 'k"US.n=i'
stadt[stadt$Word == 'goopy',]$Pron <- 'g"u.pi'
stadt[stadt$Word == 'grainy',]$Pron <- 'gr"en.i'
stadt[stadt$Word == 'holey',]$Pron <- 'h"oli'
stadt[stadt$Word == 'nonbreakable',]$Pron <- 'n"Vn.br"ek.@.bl='
stadt[stadt$Word == 'pointy',]$Pron <- 'p"OInt.i'
stadt[stadt$Word == 'scrunchy',]$Pron <- 'skr"VntS.i'
stadt[stadt$Word == 'smooshy',]$Pron <- 'sm"VS.i'
stadt[stadt$Word == 'squeezable',]$Pron <- 'skw"iz.@.bl='
stadt[stadt$Word == 'squishy',]$Pron <- 'skw"IS.i'
stadt[stadt$Word == 'wafflish',]$Pron <- 'w"A.flIS'

## Replace inter-medial dentals with their voiced/voiceless counterparts:

stadt[stadt$Pron == 'b"i4.@d',]$Pron <- 'b"id.@d'
stadt[stadt$Pron == 'br"I.4l=',]$Pron <- 'br"I.tl='
stadt[stadt$Pron == 'k"o4.@d',]$Pron <- 'k"ot.@d'
stadt[stadt$Pron == 'k"Vm.f@`4.@.bl=',]$Pron <- 'k"Vm.f@`t.@.bl='
stadt[stadt$Pron == 'k@`.r"o4.@d',]$Pron <- 'k@`.r"od.@d'
stadt[stadt$Pron == 'k"V.4l=.i',]$Pron <- 'k"V.dl=.i'
stadt[stadt$Pron == 'd"En4.@d',]$Pron <- 'd"Ent.@d'
stadt[stadt$Pron == 'dIs.dZ"OIn4.@d',]$Pron <- 'dIs.dZ"OInt.@d'
stadt[stadt$Pron == 'dZ@.l"a.4@.n@s',]$Pron <- 'dZ@.l"a.t@.n@s'
stadt[stadt$Pron == 'l"u.br@.k%e4.@d',]$Pron <- 'l"u.br@.k%et.@d'
stadt[stadt$Pron == 'm"a4.@d',]$Pron <- 'm"at.@d'
stadt[stadt$Pron == 'p"en4.@d',]$Pron <- 'p"ent.@d'
stadt[stadt$Pron == 'p"3`.f@`.r%e4.@d',]$Pron <- 'p"3`.f@`.r%et.@d'
stadt[stadt$Pron == 's"an4.i',]$Pron <- 's"and.i'
stadt[stadt$Pron == 'v"El.v@4.i',]$Pron <- 'v"El.v@t.i'
stadt[stadt$Pron == 'sI.r"e4.@d',]$Pron <- 'sI.r"et.@d'
stadt[stadt$Pron == 't"En.4@`',]$Pron <- 't"En.d@`'
stadt[stadt$Pron == 'n"I4.@d',]$Pron <- 'n"It.@d'
stadt[stadt$Pron == 'kr"e.4@`.r@s',]$Pron <- 'kr"e.t@`.r@s'



##------------------------------------------------------------------
## Split by onset/offset/coda etc.:
##------------------------------------------------------------------

## Get stuff in onset of first stressed syllable:

stadt$StressedOnset <- sapply(strsplit(stadt$Pron, '"'),
	FUN = function(x) x[1])
multiple <- grep('\\.', stadt$StressedOnset, value = F)

## Hand-code those with more than one syllable before stressed syllable:

stadt[multiple, ]	# which ones
stadt[stadt$Word == 'abrasive', ]$StressedOnset <- 'br'
stadt[stadt$Word == 'ceramic', ]$StressedOnset <- 'r'
stadt[stadt$Word == 'corroded', ]$StressedOnset <- 'r'
stadt[stadt$Word == 'disjointed', ]$StressedOnset <- 'dZ'
stadt[stadt$Word == 'gelatinous', ]$StressedOnset <- 'l'
stadt[stadt$Word == 'indestructible', ]$StressedOnset <- 'str'
stadt[stadt$Word == 'inflexible', ]$StressedOnset <- 'fl'
stadt[stadt$Word == 'metallic', ]$StressedOnset <- 't'
stadt[stadt$Word == 'serrated', ]$StressedOnset <- 'r'

## Get stuff in coda of first stressed syllable:

stadt$StressedOffset <- sapply(strsplit(stadt$Pron, '"'),
	FUN = function(x) x[2])
multisyll <- grep('\\.', stadt$StressedOffset)
splitted <- strsplit(stadt$StressedOffset[multisyll], '\\.')
stadt[multisyll, ]$StressedOffset <- sapply(splitted,
	FUN = function(x) x[1])

## The "`" is best interpreted as /r/:

stadt$StressedOffset <- gsub('`', 'r', stadt$StressedOffset)

## 'Callused' is actually multisyllabic:

stadt[stadt$StressedOffset == 'al@st', ]$StressedOffset <- 'al'

## Get rid of 'ed' forms:

stadt$OffsetMono <- stadt$StressedOffset
stadt[grep('ed', stadt$Word), ]$OffsetMono <- c('Arb',
	'i', 'al', 'ot', 'od', 'ak',
	'IN', 'Vm', 'VS', 'U',
	'Ent', 'OInt',
	'EtS', 'E', 'e',
	'Arl', 'uv', 'ag',
	'I', 'u', 'at',
	'Elt', 'ent', '3r',
	'A', 'ag', 'Ib',
	'IdZ', 'I', 'V',
	'i', 'et', 'ArtS',
	'Eks', 'Ar')

## Get the vowel:

stadt$StressedVowel <- str_extract(stadt$OffsetMono,
	'(aI|e|u|OI|O|A|i|V|o|a|I|3|E|U)')
stadt$Coda <- sapply(strsplit(stadt$OffsetMono,
	split = stadt$StressedVowel), FUN = function(x) x[2])
stadt[is.na(stadt$Coda), ]$Coda <- ''

## Combine onset and coda (non-vowel):

stadt$Consonantal <- paste0(stadt$StressedOnset,
	stadt$Coda)

## Combine all into full stressed syllable:

stadt$Stressed <- paste0(stadt$StressedOnset, stadt$StressedOffset)

## Take only rough ones:

stadt <- filter(stadt, !is.na(Roughness))

## Write table (without "Pron" coz leads to problems):

write.table(select(stadt, -Pron), 'stadt_cleaned_pron.csv',
	sep = ',', row.names = F)


