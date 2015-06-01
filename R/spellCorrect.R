spellCorrect <- function(text, sensitivity = 0.11) {
  # auto-spell check 
  new.length <- 1
  old.length <- 0
  subs <- matrix(nrow = 0, ncol = 2)
  print("Calculating distances")
  all.words <- table(unlist(strsplit(text, split = " ")))
  all.words <- sort(all.words)
  main.dist.matrix <- adist(names(all.words))
  colnames(main.dist.matrix) <- names(all.words)
  rownames(main.dist.matrix) <- names(all.words)
  main.dist.matrix[main.dist.matrix==0] <- NA
  
  while(old.length != new.length) {
    old.length <- new.length 
    all.words <- sort(table(unlist(strsplit(text, split = " "))))
    all.words <- all.words[nchar(names(all.words))!=1]
    
    dist.matrix <- main.dist.matrix[rownames(main.dist.matrix) %in% 
                                      names(all.words),
                                    colnames(main.dist.matrix) %in% names(all.words)]
    
    dist.matrix <- dist.matrix[match(names(all.words),
                                     rownames(dist.matrix)),
                               match(names(all.words), 
                                     colnames(dist.matrix))]
    
    min.dists <- apply(dist.matrix, MARGIN = 1, min, na.rm = TRUE)
    min.dist.locs <- apply(dist.matrix, MARGIN = 1, which.min)
    min.dist.locs[((1:length(min.dist.locs)) - min.dist.locs) > 0] <- NA
    
    for(i in 1:ncol(dist.matrix)) {
      if(!is.na(min.dist.locs[i]) ) {
        if(min.dists[i] / nchar(names(all.words[i])) < sensitivity) {
          new.row <- c(names(all.words[i]), 
                       names(all.words[min.dist.locs[i]]))
          print(new.row)
          subs <- rbind(subs, new.row)
        }
      }
    }
    
    text <- replaceValues(text, subs)
    new.length <- nrow(subs)
  }
  return(list(text = text, subs = subs))
}


