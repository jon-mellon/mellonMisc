\name{getLocationFromCoord}
\alias{getLocationFromCoord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a particular location identifier based on a coordinates list.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getLocationFromCoord(latitudes, longitudes, n = 200, type = "county")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latitudes}{
%%     ~~Describe \code{latitudes} here~~
}
  \item{longitudes}{
%%     ~~Describe \code{longitudes} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (latitudes, longitudes, n = 200, type = "county") 
{
    require(RCurl)
    require(rjson)
    if (length(latitudes) != length(longitudes)) {
        stop("Different length vectors.")
    }
    coord2pol <- function(x) {
        output <- getURL(paste0("http://www.datasciencetoolkit.org/coordinates2politics/", 
            toJSON(x), ""))
        output <- fromJSON(output)
        return(output)
    }
    all.pos <- mapply(c, round(latitudes, 3), round(longitudes, 
        3), SIMPLIFY = FALSE)
    n <- ceiling(length(latitudes)/n)
    chunk <- function(x, n) split(x, factor(sort(rank(x)\%\%n)))
    chunks <- chunk(x = 1:length(all.pos), n = n)
    all.output <- as.list(rep(NA, length(chunks)))
    for (i in 1:length(chunks)) {
        print(i)
        current.pos <- all.pos[chunks[[i]]]
        temp.output <- coord2pol(current.pos)
        getType <- function(part) {
            type <- part$politics[sapply(part$politics, function(x) x$friendly_type) == 
                type]
            type <- unlist(type)[3:4]
            return(type)
        }
        temp.output <- lapply(temp.output, getType)
        temp.output[sapply(temp.output, length) == 0] <- NA
        temp.output <- do.call(rbind, temp.output)
        all.output[[i]] <- temp.output
    }
    all.output <- do.call(rbind, all.output)
    return(all.output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
