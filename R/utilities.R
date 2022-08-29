#' Check for the presence of named columns
#'
#' @param which_names Column names to check, e.g. `c("peaks", "genes")`
#' @param input The `names()` of this will be checked.
#' @param input_name Name of the input, e.g. "start_params" just to make it
#' identifiable if an error message is returned.
#' @param function_name Name of function for which this test is carried out.
#' Again, just to make the returning error message a bit more readable. This
#' helps with debugging if you're wrapping many functions within each other.
#'
#' @return Returns an error and stop signal if entries of `which_names`
#'  are missing in the `input`.
#' @examples
#' \dontrun{
#' check_columns(
#'   c("cells", "sample", "condition"),
#'   long_df, "long_df", "plot_profile"
#' )
#' }
#'
#' @export
#'
check_columns <- function(which_names, input, input_name, function_name) {
  check <- which_names %in% names(input)

  if (!all(check)) {
    stop(paste0(
      "The input (", input_name, ") supplied to ", function_name,
      " is missing the following named columns: ",
      paste(which_names[!check], collapse = ", ")
    ))
  }
}

#' Extract attributes from a GTF file
#'
#' @description This is a useful function to extract certain details from the
#' attributes column of a GTF file, e.g. gene names, gene IDs etc.
#' Anything that is indicated in the gtf file within a *`tag`* *`value`* pair
#' can be extracted - the user has to indicate the name of the tag as a string
#' and the corresponding value will be returned.
#'
#' @param gtf_attributes single line of GTF attribute column,
#'   e.g. `genes$attributes[1]`
#' @param att_of_interest string indicating which attribute value should be
#'   extracted,
#'   one of: `c("gene_id", "transcript_id", "")`
#' @details Splits the attribute field of a GTF file line and returns just the
#' value for the attribute of interest.
#' @examples
#' \dontrun{
#' genes <- fread("gencode.v23.basic.lincRNA.gtf")
#' names(genes) <- c(
#'   "chr", "source", "type", "start", "end",
#'   "score", "strand", "phase", "attributes"
#' )
#' # remove the columns from the GTF file you're not going to be interested in
#' genes$score <- NULL
#' genes$phase <- NULL
#' # focus, for example, only on entries of type "gene" (this will significantly
#' # shorten the runtime of the \code{extract_attributes} call)
#' genes <- genes[type == "gene"]
#' # extract the values for the attributes of interest (here: "gene_id")
#' genes$gene_id <- unlist(
#'   lapply(genes$attributes, extract_attributes, "gene_id")
#' )
#' }
#'
#' @import data.table
#'
#' @export
#'
extract_attributes <- function(gtf_attributes, att_of_interest) {
  requireNamespace("data.table")
  att <- base::strsplit(gtf_attributes, "; ")
  att <- base::gsub("\"", "", unlist(att))
  if (!is.null(unlist(strsplit(att[grep(att_of_interest, att)], " ")))) {
    return(unlist(strsplit(att[grep(att_of_interest, att)], " "))[2])
  } else {
    return(NA)
  }
}

#' Get input data.frame for UpSetR
#'
#' @description
#'   `UpSetR` requires a data.frame of zeros and ones, this
#'   function produces this from a list of, e.g.
#'   gene names for which the inter-sections are to be determined.
#'
#' @details Very much the same as `UpSetR::fromList()`, the only difference
#'   being that I am adding rownames to the output.
#'
#' @param input list
#' @return `data.frame` where the column names correspond
#'   to the names of the vectors of the list
#' @examples
#' listInput <- list(
#'   one = c(1, 2, 3, 5, 7, 8, 11, 12, 13),
#'   two = c(1, 2, 4, 5, 10),
#'   three = c(1, 5, 6, 7, 8, 9, 10, 12, 13)
#' )
#' ups_input <- make_upsetr_input(listInput)
#' UpSetR::upset(ups_input, sets = names(ups_input))
#'
#' @export
#'
make_upsetr_input <- function(input) {
  # get a vector of all entries
  universe <- unique(as.character(unlist(input)))
  #
  data <- unlist(lapply(input, function(x) {
    # NA will be introduced for every no-match
    x <- as.vector(match(universe, x))
  }))

  data[is.na(data)] <- as.integer(0) # mark every non-match with a zero
  data[data != 0] <- as.integer(1) # every match gets a one
  # get the correct shape of the data.frame
  data <- data.frame(matrix(data, ncol = length(input), byrow = FALSE))
  data <- data[which(rowSums(data) != 0), ]
  names(data) <- names(input)
  row.names(data) <- make.names(universe)
  return(data)
}

#' Java garbage collection function
#'
#' @description To avoid crashing of `xlsx::write.xlsx()`.
#' Ideally, just make use of `openxlsx` instead of `xlsx`,
#' this removes the Java dependency all together.
#'
#' @export
jgc <- function() {
  base::gc()
  rJava::.jcall("java/lang/System", method = "gc")
}
