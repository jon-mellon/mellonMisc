\name{mii.w6.model}
\alias{mii.w6.model}
\docType{data}
\title{
SVM model for wave 6 MII coding
}
\description{
 Object necessary to use issue coder on new data. 
}
\usage{data("mii.w6.model")}
\format{
  The format is:
List of 29
 $ call           : language svm.default(x = container@training_matrix, y = container@training_codes,      kernel = kernel, cost = cost, cross = cross, probability = TRUE, method = method)
 $ type           : num 0
 $ kernel         : num 2
 $ cost           : num 100
 $ degree         : num 3
 $ gamma          : num 0.00877
 $ coef0          : num 0
 $ nu             : num 0.5
 $ epsilon        : num 0.1
 $ sparse         : logi TRUE
 $ scaled         : logi [1:114] FALSE FALSE FALSE FALSE FALSE FALSE ...
 $ x.scale        : NULL
 $ y.scale        : NULL
 $ nclasses       : int 47
 $ levels         : chr [1:47] "aging population" "apathy" "banks" "civil liberties" ...
 $ tot.nSV        : int 9135
 $ nSV            : int [1:47] 158 763 638 136 14 38 254 5 281 923 ...
 $ labels         : int [1:47] 34 15 20 14 36 6 38 16 37 9 ...
 $ SV             :Formal class 'matrix.csr' [package "SparseM"] with 4 slots
  .. ..@ ra       : num [1:9878] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..@ ja       : int [1:9878] 60 108 76 13 59 13 59 13 59 13 ...
  .. ..@ ia       : int [1:9136] 1 3 4 6 8 10 12 14 16 18 ...
  .. ..@ dimension: int [1:2] 9135 114
 $ index          : int [1:9135] 3270 11480 18545 18596 18645 18678 18708 18730 18736 18769 ...
 $ rho            : num [1:1081] -8.07 -11.5 -10.65 0.69 3.7 ...
 $ compprob       : logi TRUE
 $ probA          : num [1:1081] -5.19 -5.25 -5.35 -6.87 -5.08 ...
 $ probB          : num [1:1081] -1.38 -1.153 -1.256 -1.06 0.679 ...
 $ sigma          : num 0
 $ coefs          : num [1:9135, 1:46] 0 0 0 0 0 0 0 0 0 0 ...
 $ na.action      : NULL
 $ fitted         : Factor w/ 47 levels "aging population",..: 34 34 34 34 34 15 15 14 14 34 ...
  ..- attr(*, "names")= chr [1:46793] "1" "2" "3" "4" ...
 $ decision.values: num [1:46793, 1:1081] 1.06 1 1 1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:46793] "1" "2" "3" "4" ...
  .. ..$ : chr [1:1081] "poverty, living stds/europe" "poverty, living stds/international probs" "poverty, living stds/environment" "poverty, living stds/scot const" ...
 - attr(*, "class")= chr "svm"
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
data(mii.w6.model)
## maybe str(mii.w6.model) ; plot(mii.w6.model) ...
}
\keyword{datasets}
