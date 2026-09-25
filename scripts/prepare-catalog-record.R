prepare_catalog_record <- function(record, catalog_fields) {

  if (nrow(record) != 1) {
    stop(
      "prepare_catalog_record() requires exactly one record; found ",
      nrow(record),
      "."
    )
  }

  record |>
    dplyr::select(dplyr::all_of(catalog_fields$field)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "field",
      values_to = "value"
    ) |>
    dplyr::left_join(
      catalog_fields,
      by = "field"
    ) |>
    dplyr::filter(
      display,
      display_when_empty | (!is.na(value) & value != "")
    ) |>
    dplyr::arrange(order)
}
