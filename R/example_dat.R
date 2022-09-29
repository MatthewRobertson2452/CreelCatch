#' CreelCat data for 100 surveys from 5 states.
#'
#' A dataset containing CreelCat survey data and covariate values for 100 surveys from 5 states.
#'
#' @format A data frame with 100 rows and 19 variables:
#' \describe{
#'   \item{WaterbodyID}{ID of the sampled waterbody}
#'   \item{SurveyID}{ID of the specific creel survey}
#'   \item{Year}{Year when the survey occurred}
#'   \item{State}{State the survey occurred in}
#'   \item{GL}{Binary indicator of whether the surveyed waterbody is a Great Lake (1) or not (0)}
#'   \item{Catch_per_day}{Estimated catch per day from the creel survey}
#'   \item{Effort_per_dat}{Estimated fishing effort per day from the creel survey}
#'   \item{Area}{Area of the surveyed waterbody (ha)}
#'   \item{Population_size}{Mean population size of the counties that overlap the location of the waterbody}
#'   \item{Income}{Mean income of the counties that overlap the location of the waterbody}
#'   \item{Age}{Mean age of the counties that overlap the location of the waterbody}
#'   \item{House_size}{Mean household size of the counties that overlap the location of the waterbody}
#'   \item{Distance}{Distance from the waterbody to the nearest primary road}
#'   \item{all_dev}{Proportion of 100m around the waterbody that is some type of human development}
#'   \item{all_forests}{Proportion of 100m around the waterbody that is some type of forest}
#'   \item{all_wetlands}{Proportion of 100m around the waterbody that is some type of wetland}
#'   \item{all_farms}{Proportion of 100m around the waterbody that is some type of farm}
#'   \item{istate}{ID of the specific creel survey}
#' }
#' @source \url{https://creelcat.shinyapps.io/CreelCat/}
"example_dat"