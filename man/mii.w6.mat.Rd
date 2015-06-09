\name{mii.w6.mat}
\alias{mii.w6.mat}
\docType{data}
\title{
MII auto-coding term-doc matrix
}
\description{
Term-document matrix for BES wave 6 auto-coding. Object necessary to use issue coder on new data. 
}
\usage{data("mii.w6.mat")}
\format{
  The format is:
List of 6
 $ i       : int [1:228264] 1 2 2 4 4 4 6 7 8 9 ...
 $ j       : int [1:228264] 23 60 108 9 48 88 23 76 76 38 ...
 $ v       : num [1:228264] 1 1 1 1 1 2 1 1 1 1 ...
 $ nrow    : int 188602
 $ ncol    : int 114
 $ dimnames:List of 2
  ..$ Docs : chr [1:188602] "1" "2" "3" "4" ...
  ..$ Terms: chr [1:114] "afford" "asylum" "auster" "benefit" ...
 - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
 - attr(*, "weighting")= chr [1:2] "term frequency" "tf"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mii.w6.mat)
}
\keyword{datasets}
