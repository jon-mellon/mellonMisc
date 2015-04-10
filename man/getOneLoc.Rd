\name{getOneLoc}
\alias{getOneLoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets the coordinates of a free text location from DSTK
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getOneLoc(name, google = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{google}{
%%     ~~Describe \code{google} here~~
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
function (name, google = FALSE) 
{
    name <- toupper(name)
    print(name)
    if (!google) {
        url <- paste0("http://www.datasciencetoolkit.org/text2places/", 
            paste0("[\"", name, "\"]"))
        text <- readLines(url)
        text <- text[grepl("latitude|longitude", text)]
        if (length(text) == 0) {
        }
        else {
            print("success")
        }
        return(text)
    }
    if (google) {
        url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=", 
            name, "&key=", "AIzaSyD7HHcKkrP0TsLzaa_ltmtH1oXQKgHHMKI")
        text <- readLines(url)
        text <- text[grepl("\"lat\"|\"lng\"", text)][1:2]
        return(text)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
