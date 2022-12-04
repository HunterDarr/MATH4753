#' Read in csv
#'
#' @param x Name of csv file
#'
#' @return the read csv
#' @export
#'
#'@importFrom utils head read.csv
#'
#' @examples
#' \dontrun{readInData("SPRUCE.csv")}
readInData <- function(x) {
  dataFromFile = read.csv(x)
  head(dataFromFile)
}
