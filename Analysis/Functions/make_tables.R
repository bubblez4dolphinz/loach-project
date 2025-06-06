## ---------------------------
##
## Script name: make_tables.r
##
## Purpose of script: 
##      To make tables from the glmmTMB objects
##
## ---------------------------
##
## Notes:
##   The user defines the list of models
## ---------------------------


make_gt <- function(model, model_name) {
  tidy(model, conf.int = FALSE) %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, statistic, p.value) %>%
    mutate(
      p_stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      )
    ) %>%
    gt(rowname_col = "term") %>%
    tab_header(title = model_name) %>%
    fmt_number(
      columns  = c("estimate", "std.error", "statistic", "p.value"),
      decimals = 3
    ) %>%
    cols_label(p_stars = "")
}
