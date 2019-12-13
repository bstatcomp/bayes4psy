#' Datasets for bayes4psy examples
#' Example datasets for use in \pkg{rstanarm} examples and vignettes.
#' The datasets were extracted from the internal MBLab \url{http://www.mblab.si} repository.
#' MBLab is a research lab at the Faculty of Arts, Department of Psychology, University of Ljubljana, Slovenia.
#'
#' @name bayes4psy-datasets
#' @aliases adaptation_level_small adaptation_level after_images_opponent_process after_images_stimuli after_images_trichromatic after_images flanker stroop_extended stroop_simple
#'
#' @format
#' \describe{
#' \item{\code{adaptation_level_small}}{
#' Small dataset on subjects picking up weights and determining their weights from 1..10.
#'
#' Source: Internal MBLab repository.
#'
#' 50 obs. of 3 variables
#' \itemize{
#' \item \code{sequence} sequence index.
#' \item \code{weight} actual weight of the object.
#' \item \code{response} subject's estimation of weight.
#' }
#' }
#' \item{\code{adaptation_level}}{
#' Data on subjects picking up weights and determining their weights from 1..10.
#'
#' Source: Internal MBLab repository.
#'
#' 2900 obs. of 6 variables
#' \itemize{
#' \item \code{subject} subject index.
#' \item \code{group} group index.
#' \item \code{part} first or second part of the experiment.
#' \item \code{sequence} sequence index.
#' \item \code{weight} actual weight of the object.
#' \item \code{response} subject's estimation of weight.
#' }
#' }
#' #' \item{\code{after_images_opponent_process}}{
#' Colors predicted by the opponent process theory.
#'
#' Source: Internal MBLab repository.
#'
#' 6 obs. of 7 variables
#' \itemize{
#' \item \code{stimuli} name of the color stimuli.
#' \item \code{r} value of the R component in the RGB model.
#' \item \code{g} value of the G component in the RGB model.
#' \item \code{b} value of the B component in the RGB model.
#' \item \code{h} value of the H component in the HSV model.
#' \item \code{s} value of the S component in the HSV model.
#' \item \code{v} value of the V component in the HSV model.
#' }
#' }
#' #' \item{\code{after_images_opponent_stimuli}}{
#' Stimuli used in the after images experiment.
#'
#' Source: Internal MBLab repository.
#'
#' 6 obs. of 7 variables
#' \itemize{
#' \item \code{r_s} value of the R component in the RGB model.
#' \item \code{g_s} value of the G component in the RGB model.
#' \item \code{b_s} value of the B component in the RGB model.
#' \item \code{stimuli} name of the color stimuli.
#' \item \code{h_s} value of the H component in the HSV model.
#' \item \code{s_s} value of the S component in the HSV model.
#' \item \code{v_s} value of the V component in the HSV model.
#' }
#' }
#' #' \item{\code{after_images_trichromatic}}{
#' Colors predicted by the trichromatic theory.
#'
#' Source: Internal MBLab repository.
#'
#' 6 obs. of 7 variables
#' \itemize{
#' \item \code{stimuli} name of the color stimuli.
#' \item \code{r} value of the R component in the RGB model.
#' \item \code{g} value of the G component in the RGB model.
#' \item \code{b} value of the B component in the RGB model.
#' \item \code{h} value of the H component in the HSV model.
#' \item \code{s} value of the S component in the HSV model.
#' \item \code{v} value of the V component in the HSV model.
#' }
#' }
#' #' \item{\code{after_images}}{
#' Data gathered by the after images experiment.
#'
#' Source: Internal MBLab repository.
#'
#' 1311 obs. of 12 variables
#' \itemize{
#' \item \code{subject} subject index.
#' \item \code{rt} reaction time.
#' \item \code{r} value of the R component in the RGB model of subject's response.
#' \item \code{g} value of the G component in the RGB model of subject's response.
#' \item \code{b} value of the B component in the RGB model of subject's response.
#' \item \code{stimuli} name of the color stimuli.
#' \item \code{r_s} value of the R component in the RGB model of the shown stimulus
#' \item \code{g_s} value of the G component in the RGB model of the shown stimulus
#' \item \code{b_s} value of the B component in the RGB model of the shown stimulus
#' \item \code{h_s} value of the H component in the HSV model of the shown stimulus
#' \item \code{s_s} value of the S component in the HSV model of the shown stimulus
#' \item \code{v_s} value of the V component in the HSV model of the shown stimulus
#' }
#' }
#' #' \item{\code{flanker}}{
#' Data gathered by the flanker experiment.
#'
#' Source: Internal MBLab repository.
#'
#' 8256 obs. of 5 variables
#' \itemize{
#' \item \code{subject} subject index.
#' \item \code{group} group index.
#' \item \code{congruencty} type of stimulus.
#' \item \code{result} was subject's reponse correct or wrong?
#' \item \code{rt} reaction time.
#' }
#' }
#' #' \item{\code{stroop_extended}}{
#' All the data gathered by the Stroop experiment.
#'
#' Source: Internal MBLab repository.
#'
#' 41068 obs. of 5 variables
#' \itemize{
#' \item \code{subject} subject ID.
#' \item \code{cond} type of condition.
#' \item \code{rt} reaction time.
#' \item \code{acc} was subject's reponse correct or wrong?
#' \item \code{age} age of subject.
#' }
#' }
#' #' \item{\code{stroop_simple}}{
#' All the data gathered by the Stroop experiment.
#'
#' Source: Internal MBLab repository.
#'
#' 61 obs. of 5 variables
#' \itemize{
#' \item \code{subject} subject ID.
#' \item \code{reading_neutral} average response time for reading neutral stimuli.
#' \item \code{naming_neutral} average response time for naming neutral stimuli.
#' \item \code{reading_incongruent} average response time for reading incongruent stimuli.
#' \item \code{naming_incongruent} average response time for naming incongruent stimuli.
#' }
#' }
#' }
#'
#' @examples
#'
#' # Example of Bayesian bootstraping on 'adaptation_level_small' dataset
#' # linear function of seqence vs. response
#' lm_statistic <- function(data) {
#'   lm(sequence ~ response, data)$coef
#' }
#'
#' # load data
#' data <- adaptation_level_small
#'
#' # bootstrap
#' data_bootstrap <- b_bootstrap(data, lm_statistic, n1=1000, n2=1000)
#'
NULL
