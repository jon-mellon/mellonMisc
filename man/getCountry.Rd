\name{getCountry}
\alias{getCountry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the country from an address using DSTK
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCountry(locations)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locations}{
%%     ~~Describe \code{locations} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
function (locations) 
{
    urls <- paste0("http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false \n&address=", 
        sapply(locations, gsub, pattern = "[[:space:]]+", replacement = "+"))
    coords <- as.list(rep(NA, length(urls)))
    for (i in 1:length(urls)) {
        coords[[i]] <- readLines(urls[i])
    }
    parseCoord <- function(test) {
        pos <- which(grepl("location", test))[1]
        if (!is.na(pos)) {
            loc <- test[(pos + 1):(pos + 2)]
            loc <- gsub("\"|[[:alpha:]]|[, :]", "", loc)
            loc <- as.numeric(loc)
            return(loc)
        }
        else {
            return(c(NA, NA))
        }
    }
    coords <- t(sapply(coords, parseCoord))
    locations <- getLocationFromCoord(latitudes = coords[, 2], 
        longitudes = coords[, 1], type = "country")
    return(locations)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
