\name{manyDistToLoc}
\alias{manyDistToLoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the distance of a set of a points from a reference location
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
manyDistToLoc(points, reference.point)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{
%%     ~~Describe \code{points} here~~
}
  \item{reference.point}{
%%     ~~Describe \code{reference.point} here~~
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
function (points, reference.point) 
{
    R <- 6371
    p1rad <- points * pi/180
    p2rad <- reference.point * pi/180
    d <- sin(p1rad[, 2]) * sin(p2rad[2]) + cos(p1rad[, 2]) * 
        cos(p2rad[2]) * cos(abs(p1rad[, 1] - p2rad[1]))
    d <- acos(d)
    return(R * d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
