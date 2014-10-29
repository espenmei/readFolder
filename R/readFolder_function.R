#' Read folder
#' 
#' This function reads all files in a folder.
#' @param dir directory to folder.
#' @param pattern wildcard pattern for filenames. Defaults to "*".
#' @param format the format to be returned. Either a list or data.frame. Defaults to data.frame.
#' @param fun function to be applied to each file in the folder.
#' @param ... optional arguments passed to read.table. See ?read.table
#' @examples
#' Calculate the cumulative value for a variable("score") in a new variable("cumscore")
#' cumulativeSum = function(data) {
#'    data[, "cumscore"] = cumsum(data[, "score"])
#'    return(data)
#' }
#' dir = "c:/experiments/experiment1/"
#' data = readFolder(dir, fun = cumulativeSum)

readFolder = function(dir, pattern = "*", format = "data.frame", fun, ...) {

  #Check parameters
  if (!is.character(dir)) stop("dir needs to be of type character")
  if (!file.exists(dir)) stop(paste("dir '",dir,"' does not exist",sep=""))
  if (length(list.files(dir, pattern = glob2rx(pattern))) == 0) stop(paste("no files in dir matching",pattern))
  if (!is.character(format)) stop("format needs to be of type character")
  if (!is.character(pattern)) stop("pattern needs to be of type character")
  
  #Remove trailing file separation
  if (substring(dir,nchar(dir)) == .Platform$file.sep) {
    dir = substring(dir,1,nchar(dir)-1)
  }
  
  # all files in dir
  files = paste(dir, list.files(path = dir, pattern = glob2rx(pattern)), sep = .Platform$file.sep)
  # get all files as list
  dataList = lapply(files, read.table, ...)
  
  # apply fun to all elements of dataList
  if(!missing(fun)) {
    dataList = lapply(dataList, fun) 
  }
  
  # return
  if(format == "data.frame") {
    # rowbind to single data.frame
    return(do.call(rbind, dataList))
  } else if(format == "list"){
    return(dataList)
  }
  
}
