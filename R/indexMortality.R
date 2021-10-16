#' @export indexMortality
	indexMortality <- function(mortality.table, index.year = 1967, valid.years, elec.gap) {
	
	mortality.table$yob <- mortality.table$Year - mortality.table$Age
	if(max(mortality.table$Year)<(floor(index.year) + elec.gap + 2)) {
		message("Sample included years not present in mortality data, shifting years by 5")
		mortality.table$Year <- mortality.table$Year + 5
		mortality.table$yob <- mortality.table$yob + 5
	}
	
	mortality.table <- mortality.table %>% arrange(yob, Year)
	
	mortality.table$Female[which(mortality.table$Female > 0.99)] <- 0.98
	mortality.table$Male[which(mortality.table$Male > 0.99)] <- 0.98
	mortality.table$Total[which(mortality.table$Total > 0.99)] <- 0.98
	
	mortality.table <- mortality.table[mortality.table$yob %in% valid.years, ]
  mortality.table <- mortality.table[mortality.table$Year>=floor(index.year), ]
  
  for(year in unique(mortality.table$Year)) {
  	miss.years <- valid.years[!valid.years %in% mortality.table$yob[mortality.table$Year==year]]
  	extra.mort.row <- mortality.table %>% filter(OpenInterval & Year==year)
  	miss.years
  	extra.mort.row <- extra.mort.row[rep(1, length(miss.years)), ]
  	extra.mort.row$yob <- miss.years
  	extra.mort.row$Age <- extra.mort.row$Year - extra.mort.row$yob
  	mortality.table <- rbind(mortality.table, extra.mort.row)
  }
  
  mortality.table <- mortality.table %>% arrange(yob, Year)
  
  mortality.table$maleliferate <- 1 - mortality.table$Male
  mortality.table$femaleliferate <- 1 - mortality.table$Female
  mortality.table$totalliferate <- 1 - mortality.table$Total
  
  mortality.table$cumdeathfemale <- 1- unlist(tapply(mortality.table$femaleliferate, mortality.table$yob, cumprod))
  mortality.table$cumdeathmale <- 1- unlist(tapply(mortality.table$maleliferate, mortality.table$yob, cumprod))
  mortality.table$cumdeathtotal <- 1- unlist(tapply(mortality.table$totalliferate, mortality.table$yob, cumprod))
  
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathmale), ]
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathfemale), ]
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathtotal), ]
  
  pickCumMort <- function(cum, yearsgoneby) {
  	
  	cumd <- dtf(cum = cum, yearsout = 1:length(cum))
  	cumd <- rbind(dtf(cum = 0, yearsout = 0), cumd)
  	
  	if(any(cumd$yearsout==yearsgoneby)) {
  		return(cumd$cum[cumd$yearsout==yearsgoneby])
  	}
  	
  	year.diffs <- (yearsgoneby - cumd$yearsout)
  	
  	minmort <- cumd$cum[max(which(year.diffs>0))]
  	maxmort <- cumd$cum[min(which(year.diffs<0))]
  	
  	pred.dead <- predict(newdata = dtf(yearsout = yearsgoneby), 
  											 scam(data = cumd, formula = cum ~ s(yearsout, bs = "mpi"), 
  											 		 family = "quasibinomial"), type = "response")
  	
  	if(pred.dead>maxmort) {
  		return(maxmort)
  	}
  	if(pred.dead<minmort) {
  		return(minmort)
  	}
  	return(pred.dead)
  }
  panel.mortality <- rbind(dtf(mortality = tapply(mortality.table$cumdeathmale, 
  																								mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), 
  														 gender = "Male", yob = unique(mortality.table$yob)), 
  												 dtf(mortality = tapply(mortality.table$cumdeathfemale, 
  												 											 mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), 
  												 		gender = "Female", yob = unique(mortality.table$yob)), 
  												 dtf(mortality = tapply(mortality.table$cumdeathtotal, 
  												 											 mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), 
  												 		gender = "Total", yob = unique(mortality.table$yob)))
 
  return(panel.mortality)
}
