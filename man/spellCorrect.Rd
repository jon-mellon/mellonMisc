\name{spellCorrect}
\alias{spellCorrect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merges similar spellings in text
}
\description{
Clusters similar tokens iteratively. Useful for cleaning informal text prior to machine learning. 
}
\usage{
spellCorrect(text, sensitivity = 0.11)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
Character vector of plain text segments. 
}
  \item{sensitivity}{
The maximum distance between two tokens for a merge to take place. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character vector of the same length as the input text, with tokens substituted according to the clustering algorithm. 

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (text, sensitivity = 0.11) 
{
    new.length <- 1
    old.length <- 0
    subs <- matrix(nrow = 0, ncol = 2)
    print("Calculating distances")
    all.words <- table(unlist(strsplit(text, split = " ")))
    all.words <- sort(all.words)
    main.dist.matrix <- adist(names(all.words))
    colnames(main.dist.matrix) <- names(all.words)
    rownames(main.dist.matrix) <- names(all.words)
    main.dist.matrix[main.dist.matrix == 0] <- NA
    while (old.length != new.length) {
        old.length <- new.length
        all.words <- sort(table(unlist(strsplit(text, split = " "))))
        all.words <- all.words[nchar(names(all.words)) != 1]
        dist.matrix <- main.dist.matrix[rownames(main.dist.matrix) \%in\% 
            names(all.words), colnames(main.dist.matrix) \%in\% 
            names(all.words)]
        dist.matrix <- dist.matrix[match(names(all.words), rownames(dist.matrix)), 
            match(names(all.words), colnames(dist.matrix))]
        min.dists <- apply(dist.matrix, MARGIN = 1, min, na.rm = TRUE)
        min.dist.locs <- apply(dist.matrix, MARGIN = 1, which.min)
        min.dist.locs[((1:length(min.dist.locs)) - min.dist.locs) > 
            0] <- NA
        for (i in 1:ncol(dist.matrix)) {
            if (!is.na(min.dist.locs[i])) {
                if (min.dists[i]/nchar(names(all.words[i])) < 
                  sensitivity) {
                  new.row <- c(names(all.words[i]), names(all.words[min.dist.locs[i]]))
                  print(new.row)
                  subs <- rbind(subs, new.row)
                }
            }
        }
        text <- replaceValues(text, subs)
        new.length <- nrow(subs)
    }
    return(text)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
