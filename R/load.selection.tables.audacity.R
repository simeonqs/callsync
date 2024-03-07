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
#' path_git = 'https://raw.githubusercontent.com'
#' path_repo = '/simeonqs/callsync/master/tests/testthat/files'
#' file_1 = '/audacity/chunk_15_ground_truth.txt'
#' url_1 = paste0(path_git, path_repo, file_1)
#' local_dir = paste(tempdir(), 'audacity', sep = '/')
#' local_file_1 = paste(tempdir(), file_1, sep = '/')
#' if(!dir.exists(local_dir)) dir.create(local_dir)
#' if(!file.exists(local_file_1))
#'   download.file(url_1, destfile = local_file_1, mode = 'wb',)
#' st = load.selection.tables.audacity(path_selection_tables = local_dir)
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
