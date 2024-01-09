#' @title load.selection.tables.audacity
#'
#' @description Loads multiple Audacity selection tables into one data frame.
#'
#' @param path_selection_tables character, the path to the folder containing selection tables. Folder should
#' not contain any other txt files.
#'
#' @return Returns data frame with all selection tables.
#'
#' @examples
#' require(callsync)
#' require(seewave)
#' require(tuneR)
#' files = system.file("extdata", "audacity", package = "callsync")
#' st = load.selection.tables.audacity(path_selection_tables = files)
#'
#' @export
#' @importFrom dplyr "bind_rows"
#' @importFrom utils "read.table"

load.selection.tables.audacity = function(path_selection_tables){
  files = list.files(path_selection_tables, '*txt', full.names = TRUE)
  st_all = lapply(files, function(file){
    st = read.table(file)
    st = st[seq(1, nrow(st)-1, 2),] # remove weird lines
    names(st) = c('start', 'end', 'file')
    return(st)
  }) |> bind_rows()
  st_all$start = as.numeric(st_all$start)
  st_all$end = as.numeric(st_all$end)
  return(st_all)
}
