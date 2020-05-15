\name{create_matrix}
\alias{create_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bug fixed from RTextTools package, will be deprecated as soon as the CRAN version is updated. 
}
\description{
Bug fixed from RTextTools package, will be deprecated as soon as the CRAN version is updated. 
}
\usage{
create_matrix(textColumns, language = "english", minDocFreq = 1, maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE, removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, weighting = weightTf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textColumns}{
%%     ~~Describe \code{textColumns} here~~
}
  \item{language}{
%%     ~~Describe \code{language} here~~
}
  \item{minDocFreq}{
%%     ~~Describe \code{minDocFreq} here~~
}
  \item{maxDocFreq}{
%%     ~~Describe \code{maxDocFreq} here~~
}
  \item{minWordLength}{
%%     ~~Describe \code{minWordLength} here~~
}
  \item{maxWordLength}{
%%     ~~Describe \code{maxWordLength} here~~
}
  \item{ngramLength}{
%%     ~~Describe \code{ngramLength} here~~
}
  \item{originalMatrix}{
%%     ~~Describe \code{originalMatrix} here~~
}
  \item{removeNumbers}{
%%     ~~Describe \code{removeNumbers} here~~
}
  \item{removePunctuation}{
%%     ~~Describe \code{removePunctuation} here~~
}
  \item{removeSparseTerms}{
%%     ~~Describe \code{removeSparseTerms} here~~
}
  \item{removeStopwords}{
%%     ~~Describe \code{removeStopwords} here~~
}
  \item{stemWords}{
%%     ~~Describe \code{stemWords} here~~
}
  \item{stripWhitespace}{
%%     ~~Describe \code{stripWhitespace} here~~
}
  \item{toLower}{
%%     ~~Describe \code{toLower} here~~
}
  \item{weighting}{
%%     ~~Describe \code{weighting} here~~
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
function (textColumns, language = "english", minDocFreq = 1, 
    maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, 
    ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE, 
    removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, 
    stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, 
    weighting = weightTf) 
{
    stem_words <- function(x) {
        split <- strsplit(x, " ")
        return(wordStem(unlist(split), language = language))
    }
    tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, 
        method = "string", n = n)))))
    control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)), 
        language = language, tolower = toLower, removeNumbers = removeNumbers, 
        removePunctuation = removePunctuation, stopwords = removeStopwords, 
        stripWhitespace = stripWhitespace, wordLengths = c(minWordLength, 
            maxWordLength), weighting = weighting)
    if (ngramLength > 1) {
        control <- append(control, list(tokenize = tokenize_ngrams), 
            after = 7)
    }
    else {
        control <- append(control, list(tokenize = scan_tokenizer), 
            after = 4)
    }
    if (stemWords == TRUE && ngramLength == 1) 
        control <- append(control, list(stemming = stem_words), 
            after = 7)
    trainingColumn <- apply(as.matrix(textColumns), 1, paste, 
        collapse = " ")
    trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
        iconv, to = "UTF8", sub = "byte")
    corpus <- Corpus(VectorSource(trainingColumn), readerControl = list(language = language))
    matrix <- DocumentTermMatrix(corpus, control = control)
    if (removeSparseTerms > 0) 
        matrix <- removeSparseTerms(matrix, removeSparseTerms)
    if (!is.null(originalMatrix)) {
        terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) \%in\% 
            colnames(matrix))])
        weight <- 0
        if (attr(weighting, "acronym") == "tf-idf") 
            weight <- 1e-09
        amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
        colnames(amat) <- terms
        rownames(amat) <- rownames(matrix)
        fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) \%in\% 
            colnames(originalMatrix))], amat), weighting = weighting)
        matrix <- fixed
    }
    matrix <- matrix[, sort(colnames(matrix))]
    gc()
    return(matrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
