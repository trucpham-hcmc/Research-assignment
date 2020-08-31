
library(dplyr)
library(magrittr)
library(tibble)
library(tidyselect)
library(tidyr)
library(ggplot2)
library(foreach)




#### DOWNLOAF FILES

utils::download.file( url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv",
                      destfile = here::here("data","spotify_song.csv"), 
                      method = "libcurl", 
                      quiet = FALSE, mode = "w",
                      cacheOK = TRUE
  
)

raw <- utils::read.csv(file = here::here("data","spotify_song.csv"), header = TRUE) %>% tibble::as_tibble()

dplyr::glimpse(raw)

#' Features
features <- names(raw)[12:23]

#' A
raw %>%
  dplyr::select(features) %>% 
  tidyr::pivot_longer(cols = tidyselect::everything(), names_to =  "feature", values_to = "value") %>%
  ggplot2::ggplot(mapping = aes(x = value, group = feature)) %+%
  geom_density(aes(color = feature), alpha = 0.5) +
  facet_wrap(~feature, ncol = 3, scales = 'free')

convert_miliseconds <- function(duration, by = "second") {
  
  stopifnot(duration > 0)
  
  ms <- switch (stringr::str_sub(by, 1L, 1L),
    "s" = 1000,
    "m" = 1000 * 60,
    "h" = 1000 * 60^2,
    "d" = 1000 * 60^2 * 24
  )
  
  ms <- duration / ms
  
  return(ms)
  
}
raw %>%
  dplyr::filter(energy >= 0.75, convert_miliseconds(duration_ms, by ="minutes")>5)


get_feature <- function(.data, number) {
  .range <- .data %>%
  dplyr::filter(dplyr::row_number() == !!number) %>%
    dplyr::select(12:22) %>%
    as.list()
  
  return(.range)
  
}

eculean <- function(...) {
  
  #' biến đầu vào thành list 
  #' 
  #' .d: object bị ẩn 
  
  .d <- rlang::dots_list(...,
                         .named = FALSE,
                         .ignore_empty = "none",
                         .preserve_empty = FALSE,
                         .homonyms = "error",
                         .check_assign = FALSE)
  
  .eculean <- 0 
  
  for (i in length(.d)) {
    
    .data <- (.d[[1]][[i]] - .d[[2]][[i]])^2
    .eculean <- sum(.eculean, .data)
    
  }
  
  #'square 

  eculean <- sqrt(.eculean)

  return(eculean)  
}
start <- Sys.time()
ecu1 <- foreach::foreach(i = 2:nrow(raw), .combine = dplyr::bind_rows, .errorhandling = "stop") %do% {
  
  song <- get_feature(raw,1)

  #'subset
  
  .sub <- get_feature(raw,i)
  
  #'Eculean
  
  .ecu <- eculean(song, .sub) %>%
    tibble::enframe(name = NULL, value = "eculean") %>%
    dplyr::mutate(id = i)
  
  cli::cli_alert_success("Danh sach day ne {i}")
  
  #'Return
  
  return(.ecu)
}

end <- Sys.time()
as.numeric(difftime(end, start, units = "auto"))



