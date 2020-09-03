# GLOBAL OPTIONS ====
#'
#' @description  Global Options Settings for R
#' @details
#' General options - Global Shared Envirnoment All Sessions
base::options(tab.width = 2) #' Tab width
base::options(graphics.record = TRUE) #' Graphic Record
base::options(scipen = 999) #' Disable science figure format, kind of 1.23e+1,....
base::options(shiny.autoload.r = FALSE) #' For Shiny not to automaticly update .R file in R/ directory
base::options(digits = 4) #' Numeric Format
base::options(digits.secs = 0) #' Controls the maximum number of digits to print when formatting time values in seconds.
base::options(stringsAsFactors = FALSE) #' Manage String to Factor of R in data.frame

#' SHINY SETTINGS
#' @description Settings Shiny Application for overall application Included various settings: Folder, Loading, etc...
#' @references
#' Settings \{Shiny Options}(https://shiny.rstudio.com/reference/shiny/0.14/shiny-options.html)
#' Shared Resources in Shiny \{Shared Environment}(https://github.com/rstudio/shiny/tree/master/inst/www/shared)
base::normalizePath(path = "~") #' Normalize Path
base::options(shiny.maxRequestSize = 100 * 1024^2) #' Maximum Size Type per each File for Shiny Input

# PACKAGE MANAGE ====
#'
#' @description Character vector of requirement packages, mannual handling
#' @value multiple call libary of packages
#' @example `require_packages()`
#' @export
#' Requirement Packages
require_packages <- c(
  #' Database Access Intergrated Database
  "DBI", "RPostgres", "config", "pool",
  #' Wranging Data
  "tidyverse", "dplyr", "lubridate", "tidyselect", "magrittr", "xlsx",
  #' Working with Excel
  "rlist", "rhandsontable",
  #' Rebranding Base R
  "vctrs",
  "foreach", #' Pararrel
  #' Charactor Control, Text Analysis
  "glue", "stringr", "utf8",
  #' Table
  "tibble", "reactable", "flextable",
  #' Chart Master
  "apexcharter", "zoo",
  #' Relative path
  "here",
  #' In-Line Documentation for R
  "roxygen2",
  #' Modeling Bundle
  "purrr",
  #' Chart Creator
  "ggplot2", "grid",
  #' Color Selector
  "RColorBrewer", "scales",
  #' Function coding
  "rlang", "usethis", "devtools",
  #' Web Builder
  "sass",
  #' JSON Master
  "jsonlite",
  #' Encoding
  "digest",
  #' File Management
  "fs",
  #' Markdown
  "knitr", "rmarkdown",
  #' Table
  "kableExtra",
  #' HTML Bean
  "htmlwidgets", "htmltools",
  #' Shiny Application
  "shiny",
  #' # Shiny Extension
  #' Javascripts Bundle
  "shinyjs",
  #' Input Bundle
  "shinyFeedback", "shinysky",
  #' Themes
  "shinythemes",
  #' Loading Animation Shiny
  "shinybusy", "waiter", "shinytoastr", "shinyhelper",
  #' Shiny Widget
  "shinyWidgets", "shinyalert", "shinyBS", "shinyThings",
  #' Pop Notice
  "shinypop",
  #' Interactive Gantt Chart
  "timevis",
  #' htmlwidget tooltips for R markdown and Shiny
  "tippy",
  #' Guidance, Introduction step by step
  "rintrojs", "cicerone",
  #' Icon Font
  "fontawesome",
  #' Editable table in Shiny
  "basictabler",
  #' Sparkline
  "sparkline",
  #' React
  "reactR"
)

#' Packages
for (.pack in require_packages) {
  
  #' Supress
  base::suppressPackageStartupMessages(library(
    package = .pack,
    character.only = TRUE,
    warn.conflicts = FALSE
  ))
  
  #' Remove
  rm(.pack)
}

#' Remove
rm(require_packages)
