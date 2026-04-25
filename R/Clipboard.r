# Clipboard.r
# Functions that read and write tabular data to and from the clipboard
# Uses the clipr package 
# Created 7/25/2018
# Updated 7/2019 to use the clipr package
# R Jones

#' Read or Write a Table from the Clipboard
#'
#' @description Read or write tabular data from and to the clipboard.  These functions are useful for getting data from
#'   or sending to other programs like Excel. To get data from Excel, highlight the desired  range and select "copy". 
#'   Within the script accept the tabular data like so 'df <- read.clip()'. 
#' NOTE:  These functions are aliases for those in the clipr package (e.g., clipr::read_clip_tbl)
#' 
#'  `read.clip()`  reads a table from the clipboard.
#'  `write.clip(x, ...)` writes a dataframe or tibble to the clipboard.
#'  
#' @param x is the dataframe or tibble to send to the clipboard.
#' @param ... are other arguments passed to `clipr::write_clip()`.
#' @return for `read.clip()` a dataframe is returned.
#' @export
#' @examples
#'  
#'    df <- data.frame(x = 1:10, y = 11:20)
#'    write.clip(df)
#'    
#'    df_clipboard <- read.clip
#'    df_clipboard
read.clip <- function() {
  clipr::read_clip_tbl()
  #read.table("clipboard",sep="\t",header=TRUE,...)
}

#' @rdname read.clip
#' @export
write.clip <- function(x, ...) {
  clipr::write_clip(x, object_type = "table",...)
  #write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
