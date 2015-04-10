\name{getFirstNames}
\alias{getFirstNames}

\title{
Separate out the first names from free text
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getFirstNames(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    x <- gsub(" +", " ", x)
    x <- gsub("\\.", "", x)
    x <- strsplit(x, " ")
    first.names <- sapply(x, function(x) x[1])
    first.names <- toupper(first.names)
    removals <- c("DR", "THE", "MR", "MRS", "MS")
    first.names[nchar(first.names) == 1] <- NA
    first.names[first.names \%in\% removals] <- sapply(x, function(x) x[2])[first.names \%in\% 
        removals]
    return(first.names)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
