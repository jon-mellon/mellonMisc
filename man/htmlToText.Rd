\name{htmlToText}
\alias{htmlToText}
Converts html page to text. 
\title{
Converts html to text (borrowed)
}
\description{
Code from Tony Breyal (original available at https://github.com/tonybreyal/Blog-Reference-Functions/tree/master/R)
}
\usage{
htmlToText(input, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (input, ...) 
{
    require(RCurl)
    require(XML)
    evaluate_input <- function(input) {
        if (file.exists(input)) {
            char.vec <- readLines(input, warn = FALSE)
            return(paste(char.vec, collapse = ""))
        }
        if (grepl("</html>", input, fixed = TRUE)) 
            return(input)
        if (!grepl(" ", input)) {
            if (!file.exists("cacert.perm")) 
                download.file(url = "http://curl.haxx.se/ca/cacert.pem", 
                  destfile = "cacert.perm")
            return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
        }
        return(NULL)
    }
    convert_html_to_text <- function(html) {
        doc <- htmlParse(html, asText = TRUE)
        text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
            xmlValue)
        return(text)
    }
    collapse_text <- function(txt) {
        return(paste(txt, collapse = " "))
    }
    html.list <- lapply(input, evaluate_input)
    text.list <- lapply(html.list, convert_html_to_text)
    text.vector <- sapply(text.list, collapse_text)
    return(text.vector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
