## Bodo Winter
## June 2, 2017
## Phoneme extraction functions

## Function for getting matrix of consonants:

get_consonants <- function(string) {
	all_res <- c()
	patterns <- c('T', 'D', 'f', 'v', 'p', 't[^S]|t$',
		'k', 'b', 'd[^Z]|d$', 'g', 'S', 'Z|dZ', 's', 'z', 'r', 'l',
		'm', 'n', 'N', 'w', 'j', 'h', 'tS')
		# Z|dZ are together since they are not differentiated for the non-words either
	
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- c('T', 'D', 'f', 'v', 'p', 't',
		'k', 'b', 'd', 'g', 'S', 'Z', 's', 'z', 'r', 'l',
		'm', 'n', 'N', 'w', 'j', 'h', 'tS')

	return(all_res)
	}

## Function for getting matrix of vowels:

get_vowels <- function(string) {
	all_res <- c()
	patterns <- c('@', 'e', 'I', 'u[^I]', 'O[^I]', 'A',
		'i', 'aI', 'V', 'a[^I]', 'o', 'E', 'U', '3', 'OI')
	
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- c('AT', 'e', 'I', 'u', 'O', 'A',
		'i', 'aI', 'V', 'a', 'o', 'E', 'U', 'ET', 'OI')

	return(all_res)	
	}