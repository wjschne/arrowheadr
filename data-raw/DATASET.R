## code to prepare `icons` dataset goes here
icons <- readr::read_csv("data-raw/icons.csv")


usethis::use_data(icons, overwrite = TRUE, internal = TRUE)
arrowheads <- icons |>
  tidyr::nest(.by = "icon") |>
  dplyr::arrange(icon) |>
  dplyr::mutate(data = purrr::map(data, as.matrix)) |>
  tibble::deframe()

usethis::use_data(arrowheads, overwrite = TRUE, internal = FALSE)


