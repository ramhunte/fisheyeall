filter_and_mutate_specs <- function(
  df,
  metric,
  specs,
  os,
  size,
  defl_val,
  defl_input
) {
  df |>
    dplyr::filter(
      .data$metric %in% metric,
      .data$variable %in% c(specs, os),
      .data$type %in% size
    ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        .data$metric %in%
          c(
            "Markup",
            "Production price (per lb)",
            "Production value",
            "Purchase price (per lb)",
            "Purchase value"
          ) ~
          .data$value * defl_val / .data$defl,
        TRUE ~ .data$value
      ),
      unit_lab = dplyr::case_when(
        .data$metric == "Production weight" ~
          paste0(.data$variable, ": ", .data$unit, " lbs"),
        FALSE ~ paste0(.data$variable, ": ", .data$unit, " ", defl_input, " $")
      )
    )
}
