\name{replaceValues}
\alias{replaceValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Performs a series of word substitutions on a plain text character vector. 
}
\usage{
replaceValues(text, subs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
A character vector of plain text, with words separated by spaces. 
}
  \item{subs}{
A two column character matrix. Column 1 being the words to replace and column 2 being their replacements. 
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
function (text, subs) 
{
    text.list <- strsplit(text, split = " ")
    replace <- function(part) {
        tmp.subs <- subs[subs[, 1] \%in\% part, , drop = FALSE]
        tmp.subs <- unique(tmp.subs)
        if (nrow(tmp.subs) != 0) {
            for (i in 1:nrow(tmp.subs)) {
                part[part == tmp.subs[i, 1]] <- tmp.subs[i, 2]
            }
        }
        return(part)
    }
    text.list <- lapply(text.list, replace)
    text <- sapply(text.list, paste, collapse = " ")
    return(text)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
