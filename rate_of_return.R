#' @title Return
#'
#' @description calculate return of investment, included dividend
#'
#' @param init_price price ...
#' @param current
#' @param dividend
#'
#' @export
rate <- function(initation, current, dividend = NA) {

  #' Stop
  stopifnot(
    !rlang::is_missing(initation), !rlang::is_missing(current),
    initation > 0, current > 0
  )

  #' Calculate
  ret <- sum(initation - current, dividend, na.rm = TRUE) / initation

  #' Return
  return(ret)
}




#' @title Skewness
#' @description Calculate skweness of distribution
#' @param data
skew <- function(..., na.rm = FALSE) {
  .comp <- c(...)


  if (na.rm) {
    .comp <- .comp[!is.na(.comp)]
  }
  #' Stop

  if (any(!is.na(.comp), !is.numeric(.comp))) {

    #' Calculate

    skewn <- 1 / length(.comp) * sum((.comp - mean(.comp))^3) / sd(x)^3

    return(skewn)
  } else {
    stop(" Khong duoc co gia tri NA")
  }
}

#### CALCULATE KURTOSIS

kurto <- function(..., na.rm = FALSE) {
  .comp <- c(...)


  if (na.rm) {
    .comp <- .comp[!is.na(.comp)]
  }
  #' Stop

  if (any(!is.na(.comp), !is.numeric(.comp))) {

    #' Calculate

    kurt <- 1 /length(.comp) * sum((.comp - mean(.comp))^4) / sd(x)^4

    return(kurt)
  } else {
    stop(" Khong duoc co gia tri NA")
  }
}

#' @title Create data using vroom
#' @description use vroom, purrr, foreach
#'

#' Create data
 
 
foreach::foreach(ind = 1:1000, .combine = c, .errorhandling = "remove") %do% {
  
  .obj <- 1:sample(1:10, size = 1, replace = TRUE) %>% 
    purrr::map(~ vroom::gen_double(5)) %>%
    purrr::set_names(nm = sample(LETTERS, size = length(.), replace = FALSE)) %>%
    purrr::map_dfc(~ .)
  
  vroom::vroom_write(x = .obj,
                     path = here::here("testdata", glue::glue("file_{ind}.tsv")),
                     delim = "\t",
                     na = "NA",
                     col_names = TRUE,
                     append = FALSE,
                     quote = c("needed", "all", "none"),
                     escape = c("double", "backslash", "none"),
                     bom = FALSE
  )
  
  invisible()
}
 
skew_all <- foreach::foreach(ind = 1:1000,.combine = dplyr::bind_rows) %do% {
  
  .file_name <- glue::glue("file_{ind}.tsv")
  
  .data <- vroom::vroom(file = here::here("testdata", .file_name),
               delim = "\t",
               col_names = TRUE,
               col_types = NULL,
               col_select = NULL,
               id = NULL,
               skip = 0,
               n_max = Inf,
               na = c("", "NA"),
               quote = "\"",
               comment = "",
               trim_ws = TRUE,
               escape_double = TRUE,
               escape_backslash = FALSE,
               locale = default_locale(),
               guess_max = 100,
               altrep = TRUE,
               .name_repair = "unique")
  
  names(.data) %>%
    purrr::map(~ skew(.data[[.]])) %>%
    purrr::set_names(nm = names(.data)) %>%
    purrr::map_dfc(~ ., .id = NULL) %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "column_name",
      values_to = "skewness"
    ) %>%
    dplyr::mutate(file = .file_name)
  
}


foreach::foreach(ind = 1:1000, .combine = c) %do% {
  
  fs::file_delete(here::here("testdata", glue::glue("file_{ind}.tsv")))

    invisible()   
  
}
fs::dir_ls(here::here("testdata"))


dir_create(here::here("testdata"))
