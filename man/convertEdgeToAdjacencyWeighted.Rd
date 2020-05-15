\name{convertEdgeToAdjacencyWeighted}
\alias{convertEdgeToAdjacencyWeighted}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a weighted edge list to a weighted adjacency matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convertEdgeToAdjacencyWeighted(weighted.edge.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weighted.edge.list}{
%%     ~~Describe \code{weighted.edge.list} here~~
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
function (weighted.edge.list) 
{
    all.actors <- unique(c(weighted.edge.list[, 1], weighted.edge.list[, 
        2]))
    fillInAdjacencyRow <- function(actor, all.actors, weighted.edge.list) {
        actor.edges <- weighted.edge.list[weighted.edge.list[, 
            1] == actor, 2:3]
        row <- rep(0, length(all.actors))
        row[match(actor.edges[, 1], all.actors)] <- actor.edges[, 
            2]
        return(row)
    }
    adjacency <- sapply(all.actors, FUN = fillInAdjacencyRow, 
        all.actors = all.actors, weighted.edge.list = weighted.edge.list)
    adjacency <- t(adjacency)
    rownames(adjacency) <- all.actors
    colnames(adjacency) <- all.actors
    return(adjacency)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
