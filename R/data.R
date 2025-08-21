#' MICUSP corpus tagged with pseudobibeR features
#'
#' The Michigan Corpus of Upper-Level Student Papers (MICUSP) contains 828
#' student papers. Here each document is tagged with Biber features using the
#' pseudobibeR package. Type-to-token ratio is calculated using the moving
#' average type-to-token ratio (MATTR).
#'
#' @format A data frame containing:
#' \describe{
#' \item{doc_id}{Document ID (from MICUSP)}
#' \item{remaining columns}{Rate of feature use per 1,000 tokens. See `pseudobibeR::biber()` for description of each feature.}
#' }
#' @source Michigan Corpus of Upper-Level Student Papers,
#'   <https://elicorpora.info/main>, tagged with the pseudobibeR package.
"micusp_biber"
