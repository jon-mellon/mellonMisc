\name{reconcileStrings}
\alias{reconcileStrings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Throw the kitchen sink at string matching
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reconcileStrings(char.vector1, char.vector2, quiet = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{char.vector1}{
%%     ~~Describe \code{char.vector1} here~~
}
  \item{char.vector2}{
%%     ~~Describe \code{char.vector2} here~~
}
  \item{quiet}{
%%     ~~Describe \code{quiet} here~~
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
function (char.vector1, char.vector2, quiet = FALSE) 
{
    require(Hmisc)
    unmatched.str1 <- sort(unique(char.vector1[which(!char.vector1 \%in\% 
        char.vector2)]))
    unmatched.str2 <- sort(unique(char.vector2[which(!char.vector2 \%in\% 
        char.vector1)]))
    for (i in 1:length(unmatched.str1)) {
        replacement <- unmatched.str2[agrep(unmatched.str1[i], 
            unmatched.str2, max.distance = 0.1)]
        if (length(replacement) == 1) {
            char.vector1[unmatched.str1[i] == char.vector1] <- replacement
        }
        else {
            if (!quiet) 
                print("try pmatch")
            replacement <- unmatched.str2[pmatch(unmatched.str1[i], 
                unmatched.str2)]
            if (!is.na(replacement)) {
                char.vector1[unmatched.str1[i] == char.vector1] <- replacement
                if (!quiet) 
                  print("success")
            }
            else {
                if (!quiet) 
                  print("failure")
                replacement <- unmatched.str2[pmatch(unmatched.str2, 
                  unmatched.str1[i]) == 1]
                replacement <- replacement[!is.na(replacement)]
                if (length(replacement) != 0) {
                  char.vector1[unmatched.str1[i] == char.vector1] <- replacement
                  if (!quiet) 
                    print("success")
                }
                else {
                  if (!quiet) 
                    print("failure")
                  replacement <- unmatched.str2[pmatch(orderWords(unmatched.str1[i]), 
                    orderWords(unmatched.str2))]
                  if (!is.na(replacement)) {
                    char.vector1[unmatched.str1[i] == char.vector1] <- replacement
                    if (!quiet) 
                      print("success")
                  }
                  else {
                    if (!quiet) 
                      print("failure")
                  }
                }
            }
        }
    }
    for (i in 1:length(unmatched.str1)) {
        replacement <- unmatched.str2[agrep(unmatched.str1[i], 
            unmatched.str2, max.distance = 0.2)]
        if (length(replacement) == 1) {
            char.vector1[unmatched.str1[i] == char.vector1] <- replacement
        }
    }
    unmatched.str1 <- sort(unique(char.vector1[which(!char.vector1 \%in\% 
        char.vector2)]))
    unmatched.str2 <- sort(char.vector2[which(!char.vector2 \%in\% 
        char.vector1)])
    if (length(unmatched.str1) != 0 & length(unmatched.str2) != 
        0 & !quiet) {
        warning("not all strings could be matched. Returning vector as best we could")
        print(length(unique(unmatched.str1)))
        print(length(unique(unmatched.str2)))
    }
    return(char.vector1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
