#' cleanTheme
#'
#' Clean theme for ggploting
#' @param base_size Set the base size for text
#' @keywords clean
#' @export
cleanTheme <- function(base_size = 12){
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    axis.text = element_text(size=15),
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    strip.text = element_text(size=15)
  )
}


#' gg_colour_hue
#'
#' Emulate the defualt ggplot colour hue
#' @param n Number of colours to generate
#' @keywords colour
#' @export
gg_colour_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' RemoveEmail
#'
#' Strip emails addresses from string
#' @param x String
#' @keywords emails
#' @import stringr
#' @export
RemoveEmail <- function(x) {
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}


#' htmlStrip
#'
#' Strip html from string
#' @param x String
#' @keywords html
#' @export
htmlStrip <- function(y) {
  return(gsub("<.*?>", "", y))
}
