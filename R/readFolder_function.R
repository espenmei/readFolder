#' Read folder
#' 
#' This function reads all files in a folder.
#' @param dir directory to folder.
#' @param sep separator for columns in data files. Defaults to tabulator.
#' @param format the format to be returned. Either a list or data.frame. Defaults to data.frame.
#' @param header does the files contain a header. Defaults to TRUE.
#' @param fun function to be applied to each file in the folder.
#' @examples
#' Calculate the cumulative value for a variable("score") in a new variable("cumscore")
#' cumulativeSum = function(data) {
#'    data[, "cumscore"] = cumsum(dat[, "score"])
#'    return(data)
#' }
#' dir = "c:/experiments/experiment1/"
#' data = readFolder(dir, fun = cumulativeSum)

readFolder = function(dir, sep ="\t", format = "data.frame", header = T, fun) {

  #Check parameters
  if (!is.character(dir)) stop("dir needs to be of type character")
  if (!file.exists(dir)) stop(paste("dir '",dir,"' does not exist",sep=""))
  if (length(list.files(dir)) == 0) stop("no files in dir")
  if (!is.character(sep)) stop("sep needs to be of type character")
  if (!is.character(format)) stop("format needs to be of type character")
  if (!is.logical(header)) stop("header needs to be of type logical")
  
  #Remove trailing file separation
  if (substring(dir,nchar(dir)) == .Platform$file.sep) {
    dir = substring(dir,1,nchar(dir)-1)
  }
  
  # construct filename for all files in dir
  files = paste(dir, list.files(path = dir), sep = .Platform$file.sep) 
  
  # get all files as list
  dataList = lapply(files, read.table, sep = sep, header = header)
  
  # apply fun to all elements of dataList
  if(!missing(fun)) {
    dataList = lapply(dataList, fun) 
  }
  
  # return
  if(format == "data.frame") {
    return(do.call(rbind, dataList))# rowbind all subjects to single frame
  } else if(format == "list"){
    return(dataList)
  }
  
}
