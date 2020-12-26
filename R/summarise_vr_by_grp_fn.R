#' group by and summarise with multiple functions
#'
#' @param dta input dataset
#' @param grp group by vars defined by vars()
#' @param vr variable to be summarised defined by vars()
#' @param fn a named list of functions
#' @param ... any other input listed fn functions
#'
#' @return grouped and summarised dataset
#' @export
#' @importFrom dplyr vars %>% group_by summarise_at
#' @importFrom stats median
#'
#' @examples
#' summarise_vr_by_grp_fn(
#' dta = iris,
#' grp = vars(Species),
#' vr = vars("Sepal.Length", "Petal.Width"),
#' fn = list(avg = mean, median = median)
#' )
#' summarise_vr_by_grp_fn(
#' dta = iris,
#' grp = vars(Species),
#' vr = vars(Sepal.Length, Petal.Width),
#' fn = list(avg = mean, median = median)
#' )
summarise_vr_by_grp_fn = function(dta, grp = vars(), vr = vars(), fn = list(mean = mean, median = median), ...){
  dta %>%
    dplyr::group_by(!!!grp) %>%
    dplyr::summarise_at(.vars = vr, .funs = fn, ...)
}
