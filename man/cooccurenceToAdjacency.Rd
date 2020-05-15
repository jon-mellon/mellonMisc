\name{cooccurenceToAdjacency}
\alias{cooccurenceToAdjacency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function converting cooccurence matrix to adjacency matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cooccurenceToAdjacency(term.document.matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term.document.matrix}{
%%     ~~Describe \code{term.document.matrix} here~~
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
function (term.document.matrix) 
{
    countCooccurence <- function(term1, term2, term.doc.matrix) {
        return(sum(term.doc.matrix[, term1] & term.doc.matrix[, 
            term2], na.rm = T))
    }
    fillAdjacencyRow <- function(current.term, all.terms) {
        return(apply(X = as.matrix(all.terms), MARGIN = 1, FUN = countCooccurence, 
            term1 = current.term, term.doc.matrix = term.document.matrix))
    }
    all.terms <- colnames(term.document.matrix)
    adjacency <- apply(as.matrix(all.terms), MARGIN = 1, FUN = fillAdjacencyRow, 
        all.terms = all.terms)
    colnames(adjacency) <- all.terms
    rownames(adjacency) <- all.terms
    return(adjacency)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
