#' MICUSP corpus tagged with pseudobibeR features
#'
#' The Michigan Corpus of Upper-Level Student Papers (MICUSP) contains 828
#' student papers. Here each document is tagged with Biber features using the
#' pseudobibeR package. Type-to-token ratio is calculated using the moving
#' average type-to-token ratio (MATTR).
#'
#' @format A data frame with 828 rows and 68 columns:
#' \describe{
#' \item{doc_id}{Document ID (from MICUSP)}
#' \item{f_01_past_tense to f_67_neg_analytic}{Rate of each linguistic feature per 1,000 tokens. Features follow Douglas Biber's Multi-Dimensional Analysis framework. See pseudobibeR package documentation for detailed feature descriptions.}
#' }
#' @source Michigan Corpus of Upper-Level Student Papers,
#'   <https://elicorpora.info/main>, tagged with the pseudobibeR package.
"micusp_biber"
