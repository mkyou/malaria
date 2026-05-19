library(readr)
library(dplyr)
library(tibble)
library(jsonlite)

save_model_selection_row <- function(family, species, model_id, fit,
                                     path = 'results/model_selection.csv') {
  fixed_json <- NA_character_
  if (!is.null(fit$summary.fixed) && nrow(fit$summary.fixed) > 0) {
    fe <- fit$summary.fixed
    fe$variable <- rownames(fe)
    rownames(fe) <- NULL
    fixed_json <- as.character(jsonlite::toJSON(
      fe, dataframe = 'rows', digits = 6, na = 'null'
    ))
  }

  row <- tibble::tibble(
    family = family,
    species = species,
    model_id = as.integer(model_id),
    dic = if (!is.null(fit$dic)) fit$dic$dic else NA_real_,
    waic = if (!is.null(fit$waic)) fit$waic$waic else NA_real_,
    log_cpo_sum = if (!is.null(fit$cpo))
      sum(log(fit$cpo$cpo), na.rm = TRUE) else NA_real_,
    fixed_effects = fixed_json
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    existing <- readr::read_csv(path, show_col_types = FALSE)
    if (!'fixed_effects' %in% names(existing)) {
      existing$fixed_effects <- NA_character_
    }
    existing <- dplyr::filter(
      existing,
      !(family == !!family & species == !!species &
          model_id == !!as.integer(model_id))
    )
    rows <- dplyr::bind_rows(existing, row)
  } else {
    rows <- row
  }

  rows <- dplyr::arrange(rows, family, species, model_id)
  readr::write_csv(rows, path)
  invisible(rows)
}
