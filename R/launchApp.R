#' Launch App in the default browser
#' @author Mathieu Delsaut, \email{mathieu.delsaut@@univ-reunion.fr}
#' @import shiny rsconnect
#' @examples
#' if (interactive()) {
#'   launchApp()
#' }
#' @export
launchApp <- function() {
  appDir <- system.file("app", package = "WhatStat") # looking for inst/app
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  runApp(appDir, display.mode = "normal")
}
