validate_atlas_fields <- function(records, catalog_fields) {

  atlas_fields <- names(records)
  configured_fields <- catalog_fields$field

  new_fields <- setdiff(atlas_fields, configured_fields)
  missing_fields <- setdiff(configured_fields, atlas_fields)

  if (length(new_fields) > 0) {
    warning(
      "Atlas export contains fields not listed in catalog-fields.csv: ",
      paste(new_fields, collapse = ", ")
    )
  }

  if (length(missing_fields) > 0) {
    warning(
      "catalog-fields.csv contains fields not found in the Atlas export: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  invisible(TRUE)
}
