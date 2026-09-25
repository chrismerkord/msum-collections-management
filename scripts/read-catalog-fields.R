read_catalog_fields <- function(
    path = here::here("config", "catalog-fields.csv")
) {

  # Check that the configuration file exists
  if (!file.exists(path)) {
    stop(
      "Catalog field configuration not found:\n",
      path
    )
  }

  # Read configuration
  fields <- readr::read_csv(
    path,
    col_types = readr::cols(
      field = readr::col_character(),
      display = readr::col_logical(),
      display_when_empty = readr::col_logical(),
      label = readr::col_character(),
      order = readr::col_integer()
    ),
    show_col_types = FALSE
  )

  # Check required columns
  required_columns <- c(
    "field",
    "display",
    "display_when_empty",
    "label",
    "order"
  )

  missing_columns <- setdiff(required_columns, names(fields))

  if (length(missing_columns) > 0) {
    stop(
      "catalog-fields.csv is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # Check for duplicate field names
  duplicate_fields <- fields |>
    dplyr::filter(duplicated(field)) |>
    dplyr::pull(field)

  if (length(duplicate_fields) > 0) {
    stop(
      "Duplicate fields in catalog-fields.csv: ",
      paste(unique(duplicate_fields), collapse = ", ")
    )
  }

  # Check for missing configuration values
  incomplete_fields <- fields |>
    dplyr::filter(
      is.na(field) |
        is.na(display) |
        is.na(display_when_empty) |
        is.na(label) |
        is.na(order)
    )

  if (nrow(incomplete_fields) > 0) {
    stop(
      "catalog-fields.csv contains missing configuration values."
    )
  }

  # display_when_empty only makes sense for displayed fields
  invalid_empty_fields <- fields |>
    dplyr::filter(display_when_empty & !display) |>
    dplyr::pull(field)

  if (length(invalid_empty_fields) > 0) {
    stop(
      "Fields cannot have display_when_empty = TRUE when display = FALSE: ",
      paste(invalid_empty_fields, collapse = ", ")
    )
  }

  # Check for duplicate order values
  duplicate_orders <- fields |>
    dplyr::filter(duplicated(order)) |>
    dplyr::pull(order)

  if (length(duplicate_orders) > 0) {
    stop(
      "Duplicate order values in catalog-fields.csv: ",
      paste(unique(duplicate_orders), collapse = ", ")
    )
  }

  # Return fields in catalog display order
  fields |>
    dplyr::arrange(order)
}
