\name{factorInteractions}
\alias{factorInteractions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function creating a new factor containing interactions of all factor levels of two other factors.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
factorInteractions(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y) 
{
    if (!is.factor(x)) {
        stop("X is not a factor")
    }
    if (!is.factor(y)) {
        stop("Y is not a factor")
    }
    levels <- unlist(lapply(levels(x), paste, levels(y)))
    z <- paste(x, y)
    z[which(is.na(x))] <- NA
    z[which(is.na(y))] <- NA
    z <- factor(z, labels = levels)
    return(z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
