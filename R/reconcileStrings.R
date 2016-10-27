reconcileStrings <- function(char.vector1, char.vector2, quiet = FALSE) {
	# This function will return char.vector1 with values changed to match char.vector2.
	require(Hmisc)	
	# which wards in char.vector1 do not appear in char.vector2
	unmatched.str1 <- sort(unique(char.vector1[which(!char.vector1 %in% char.vector2)]))
	# which wards in char.vector2r do not appear in char.vector1
	unmatched.str2 <- sort(unique(char.vector2[which(!char.vector2 %in% char.vector1) ]))
	if(length(unmatched.str1)==0 & length(unmatched.str2)==0) {
	  message("No unmatched characters, returning original string")
	  return(char.vector1)
	}
	
	for(i in 1:length(unmatched.str1)) {
		replacement <- unmatched.str2[
			agrep(unmatched.str1[i],
						unmatched.str2, 
						max.distance = 0.1)]
		if(length(replacement)==1 ) {
			char.vector1[unmatched.str1[i] == char.vector1] <- replacement
		} else{
			if(!quiet)print("try pmatch")
			replacement <- unmatched.str2[pmatch(unmatched.str1[i], unmatched.str2)]
			if(!is.na(replacement)) {
				char.vector1[unmatched.str1[i] == char.vector1] <- replacement
				if(!quiet) print("success")
			} else{
				if(!quiet)print("failure")
				replacement <- unmatched.str2[pmatch(unmatched.str2,
																						 unmatched.str1[i]) == 1]
				replacement <- replacement[!is.na(replacement)]
				if(length(replacement) != 0) {
					char.vector1[unmatched.str1[i] == char.vector1] <- replacement
					if(!quiet)print("success")
				} else {
					if(!quiet)print("failure")
					replacement <- unmatched.str2[pmatch(orderWords(unmatched.str1[i]), 
																							 orderWords(unmatched.str2))]
					if(!is.na(replacement)) {
						char.vector1[unmatched.str1[i] == char.vector1] <- replacement
						if(!quiet)print("success")
					} else{
						if(!quiet)print("failure")
					}}}}}
	
	for(i in 1:length(unmatched.str1)) {
		replacement <- unmatched.str2[
			agrep(unmatched.str1[i],
						unmatched.str2, 
						max.distance = 0.2)]
		if(length(replacement)==1) {
			char.vector1[unmatched.str1[i] == char.vector1] <- replacement
		}
	}
	unmatched.str1 <- sort(unique(char.vector1[which(!char.vector1 %in% char.vector2)]))
	unmatched.str2 <- sort(char.vector2[which(!char.vector2 %in% char.vector1)])
	
	if(length(unmatched.str1)!=0 & length(unmatched.str2)!=0 & !quiet) {
		warning("not all strings could be matched. Returning vector as best we could")
		print(length(unique(unmatched.str1)))
		print(length(unique(unmatched.str2)))
	}
	return(char.vector1)
}