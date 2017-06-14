## Adapted to Hungarian by Marton Soskuthy
## 5 June, 2017

## original code:
## Bodo Winter
## June 2, 2017
## Phoneme extraction functions

## Function for getting matrix of consonants:

get_consonants <- function(string) {
	all_res <- c()
	
	patterns <- c('p', 't', 'c', 'k', 'b', 'd', 'D', 'g', 'T', 'J', 'f', 
	              's', 'S', 'h', 'v', 'z', 'Z', 'm', 'n', 'H', 'l', 'r', 'j')
	
	# c = voiceless palatal stop; D = voiced palatal stop; T = ts; J = tS
	# H = palatal nasal
	
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- patterns # ok, since none of the patterns here are real regexps

	return(all_res)
	}

## Function for getting matrix of vowels:

get_vowels <- function(string) {
	all_res <- c()
	patterns <- c("I", "i", "Y", "y", "E", "e", "Ø", "ø", 
	              "A", "a", "O", "o", "U", "u")
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- patterns # again, OK, since patterns not regexps

	return(all_res)	
	}