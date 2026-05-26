fit_meta <- function(data) {
  metafor::rma(
    yi = log_hr,
    sei = se_log_hr,
    data = data,
    method = "REML",
    test = "knha"
  )
}