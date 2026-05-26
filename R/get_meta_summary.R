get_meta_summary <- function(model) {
  tibble::tibble(
    pooled_log_hr = as.numeric(model$b),
    ci_lower_log = model$ci.lb,
    ci_upper_log = model$ci.ub,
    pooled_hr = exp(pooled_log_hr),
    ci_lower = exp(ci_lower_log),
    ci_upper = exp(ci_upper_log),
    p_value = model$pval,
    tau2 = model$tau2,
    tau = sqrt(model$tau2),
    i2 = model$I2,
    h2 = model$H2,
    q = model$QE,
    q_df = model$k - 1,
    q_p_value = model$QEp
  )
}