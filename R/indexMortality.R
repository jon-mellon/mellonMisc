#' @export indexMortality
indexMortality <- function(mortality.table, index.year = 1967, valid.years, elec.gap) {
  mortality.table <- mortality.table[mortality.table$yob %in% valid.years, ]
  mortality.table <- mortality.table[mortality.table$Year>=index.year, ]
  mortality.table$maleliferate <- 1 - mortality.table$Male
  mortality.table$femaleliferate <- 1 - mortality.table$Female
  mortality.table$totalliferate <- 1 - mortality.table$Total
  
  mortality.table$cumdeathfemale <- 1- unlist(tapply(mortality.table$femaleliferate, mortality.table$yob, cumprod))
  mortality.table$cumdeathmale <- 1- unlist(tapply(mortality.table$maleliferate, mortality.table$yob, cumprod))
  mortality.table$cumdeathtotal <- 1- unlist(tapply(mortality.table$totalliferate, mortality.table$yob, cumprod))
  
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathmale), ]
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathfemale), ]
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathtotal), ]
  
  mortality.table <- mortality.table[!(is.na(mortality.table$Female) & is.na(mortality.table$Male) & is.na(mortality.table$Total)), ]
  
  pickCumMort <- function(cum, yearsgoneby) {
    cum <- dtf(cum = cum, yearsout = 1:length(cum))
    cum <- rbind(dtf(cum = 0, yearsout = 0), cum)
    
    pred.dead <- predict(newdata = dtf(yearsout = yearsgoneby), loess(data= cum, formula = cum ~ yearsout))
    return(pred.dead)
  }
  
  panel.mortality <- rbind(dtf(mortality = tapply(mortality.table$cumdeathmale, mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), gender = "Male",
                               yob = unique(mortality.table$yob)),
                           dtf(mortality = tapply(mortality.table$cumdeathfemale, mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), gender = "Female",
                               yob = unique(mortality.table$yob)),
                           dtf(mortality = tapply(mortality.table$cumdeathtotal, mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), gender = "Total",
                               yob = unique(mortality.table$yob)))
  
  return(panel.mortality)
}
