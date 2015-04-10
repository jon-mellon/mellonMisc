\name{getSignatures}
\alias{getSignatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the signatures from a change.org petition using a petition ID
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSignatures(petition.id, page)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{petition.id}{
%%     ~~Describe \code{petition.id} here~~
}
  \item{page}{
%%     ~~Describe \code{page} here~~
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
function (petition.id, page) 
{
    output <- fromJSON(readLines(paste0("https://api.change.org/v1/petitions/", 
        petition.id, "/signatures/?page=", page, "page_size=500&api_key=", 
        api)))
    output$signatures <- t(simplify2array(output$signatures))
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
