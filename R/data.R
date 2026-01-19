#' Substitution table for text cleaning
#'
#' A lookup table of token substitutions used for issue coding.
#' @format A 1603 x 2 character matrix of substitutions.
"all.subs"

#' Issue coding document-term matrix
#'
#' Document-term matrix used with the issue coding model.
#' @format A `tm` `DocumentTermMatrix`.
"mii.w6.mat"

#' Issue coding SVM model
#'
#' Support vector machine model used for issue coding.
#' @format An `svm` model object.
"mii.w6.model"

#' Occupation crosswalks
#'
#' Crosswalk tables for mapping occupation codes.
#' @format A named list of crosswalk matrices.
"occ.crosswalks"

#' SOC 2010 coding data
#'
#' Text fragments and SOC 2010 codes.
#' @format A data frame with columns `idno`, `text`, and `SOC.2010`.
"soc"

#' US data from 2010
#'
#' List data containing a `sample` data frame and `discrete.targets`.
#' @format A list with elements `sample` and `discrete.targets`.
"us_data_2010"
