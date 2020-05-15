\name{codeIssueFragments}
\alias{codeIssueFragments}
\title{
Code text based on an existing model
}
\description{
Takes an existing model, substitution set and matrix and predicts labels for new text. 
}
\usage{
codeIssueFragments(text, substitute = all.subs, model, doc_matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{substitute}{
%%     ~~Describe \code{substitute} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{doc_matrix}{
%%     ~~Describe \code{doc_matrix} here~~
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
function (text, substitute = all.subs, model, doc_matrix) 
{
    require(tm)
    text <- cleanText(text)
    text <- replaceValues(text, subs = substitute)
    new_mat <- create_matrix(text, language = "english", removeNumbers = TRUE, 
        stemWords = TRUE, removeSparseTerms = 0.998, ngramLength = 1, 
        originalMatrix = doc_matrix)
    container <- create_container(new_mat, labels = NA, trainSize = 1:length(text), 
        testSize = 1:length(text), virgin = TRUE)
    predictions <- classify_model(container, model)
    return(predictions)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
