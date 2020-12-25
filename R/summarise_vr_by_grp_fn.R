summarise_vr_by_grp_fn = function(dta, grp = vars(), vr = vars(), fn = list(mean = mean, median = median), ...){
  dta %>%
    group_by(!!!grp) %>%
    summarise_at(.vars = vr, .funs = fn, ...)
}
