read_atlas_backup <- function(
    path = here::here("data", "raw")
) {

  # Find ZIP files
  zip_files <- list.files(
    path,
    full.names = TRUE
  ) |>
    stringr::str_subset(
      stringr::regex("\\.zip$", ignore_case = TRUE)
    )

  # Require exactly one backup ZIP
  if (length(zip_files) == 0) {
    stop(
      "No ZIP file found in ", path, ".\n",
      "Download the Minnesota Biodiversity Atlas Data Backup ",
      "and place the ZIP file in this directory."
    )
  }

  if (length(zip_files) > 1) {
    stop(
      "More than one ZIP file found in ", path, ".\n",
      "Keep only the Atlas backup you want to use."
    )
  }

  zip_file <- zip_files[[1]]

  # Find occurrences.csv inside the ZIP
  occurrence_files <- unzip(zip_file, list = TRUE) |>
    dplyr::filter(basename(Name) == "occurrences.csv") |>
    dplyr::pull(Name)

  if (length(occurrence_files) == 0) {
    stop(
      "The ZIP file does not contain occurrences.csv:\n",
      zip_file
    )
  }

  if (length(occurrence_files) > 1) {
    stop(
      "The ZIP file contains more than one file named occurrences.csv."
    )
  }

  # Read occurrences.csv directly from the ZIP
  data <- readr::read_csv(
    unz(zip_file, occurrence_files[[1]]),
    col_types = readr::cols(.default = readr::col_character()),
    na = "",
    show_col_types = FALSE
  )

  message(
    "Loaded ", nrow(data), " records from ",
    basename(zip_file)
  )

  data
}
