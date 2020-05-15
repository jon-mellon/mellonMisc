\name{plotDiscrete}
\alias{plotDiscrete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use ggplot2 to plots discrete variables
}
\description{
Wrapper for ggplot
}
\usage{
plotDiscrete(var, data, delete.values = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{delete.values}{
%%     ~~Describe \code{delete.values} here~~
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
function (var, data, delete.values = c()) 
{
    require(ggplot2)
    require(Hmisc)
    x <- data[, var]
    x <- x[!is.na(x)]
    x <- x[!x \%in\% delete.values]
    levels(x) <- gsub("_", " ", levels(x))
    var <- gsub("_|\\.", " ", var)
    var <- gsub("incomeb", "income", var)
    var <- capitalize(var)
    discrete <- data.frame(table(x)/sum(!is.na(x)) * 100)
    plot <- ggplot(aes(y = Freq, x = x), data = discrete) + geom_bar(aes(stat = "identity")) + 
        ylab("Percentage of responses") + xlab("") + ggtitle(var) + 
        opts(axis.text.x = theme_text(angle = -90)) + ylim(0, 
        100)
    return(plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
