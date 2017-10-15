
#' basic analysis report template
#'
#' @param toc
#'
#' @return call basic analysis report chunk opts
#' @export
#'
#' @examples basic_analysis_report_opt()
basic_analysis_report_opt <- function() {
  knitr::opts_chunk$set(fig_width = 6,
                        fig_height = 8,
                        fig.path = "Figures/",
                        cache.path="Cache/",
                        echo=FALSE,
                        warning=FALSE,
                        message=FALSE)

  # call the base html_document function
  # # rmarkdown::html_document(toc = toc,
  #                          fig_width = 6,
  #                          fig_height = 8,
  #                          fig.path = "Figures/",
  #                          cache.path="Cache/",
  #                          echo=FALSE,
  #                          warning=FALSE,
  #                          message=FALSE)

}

