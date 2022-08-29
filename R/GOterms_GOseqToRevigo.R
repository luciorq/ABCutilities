#' Turn the GOSEQ result into REVIGO output
#'
#' @param goseq_res result file of a goseq analysis, must contain those columns:
#'  c("over_represented_pvalue","ontology","category").
#' @param analysis_name a label for the analysis,
#'   e.g. "DB_condition", or "cluster1"
#' @param out_path path to where the (temporary) results are to be stored
#' @param python_path Define which python installation should be used to run the
#'   REVIGO script.
#'   Default: "/usr/local/bin/python".
#' @param q_cutoff FDR corrected p-value cutoff. Default: 0.05.
#'
#' @details The REVIGO API will be accessed with the help of a python script
#'   that is part of the ABCutilities package. This function needs python and
#'   sed; make sure the Python path is the correct one. If NULL,
#'   it will use the output of \code{system("which python")},
#'   which may not be approriate. Default: "/usr/local/bin/python".
#'
#' @examples \dontrun{
#' testingrev <- goseq2revigo(GO.wall,
#'   q_cutoff = 0.05,
#'   analysis_name = "test",
#'   out_path = "data/revigo/"
#' )
#' ## create treemap
#' REVIGO_treemap(
#'   revigo.data = testingrev$testBP,
#'   col_palette = "Set3",
#'   title = "testing"
#' )
#' }
#'
#' @seealso [`txtFile2REVIGO()`]
#'
#' @export
#'
goseq2revigo <- function(goseq_res,
                         q_cutoff = 0.05,
                         analysis_name = NULL,
                         out_path = NULL,
                         python_path = "/usr/local/bin/python") {
  check_columns(
    c("over_represented_pvalue", "ontology", "category"), goseq_res,
    "goseq result", "goseq2revigo"
  )

  an <- analysis_name
  if (is.null(analysis_name)) {
    analysis_name <- paste0("REVIGO_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
  }

  rev_list <- list()
  fail_count <- 0
  for (TYPE in c("BP", "CC", "MF")) {
    ## extract GO terms of interest
    enrichedGO <- goseq_res[
      stats::p.adjust(
        goseq_res$over_represented_pvalue,
        method = "BH"
      ) < q_cutoff & goseq_res$ontology == TYPE,
    ][, c("category", "over_represented_pvalue")]

    if (dim(enrichedGO)[1] != 0) {
      if (
        !all(names(enrichedGO) == c("category", "over_represented_pvalue"))
      ) {
        stop(
          "Make sure the goSeq result file contains 'category' ",
          "and 'over_represented_pvalue'"
        )
      }
      ## generate txt file for REVIGO
      outfile <- paste0(
        out_path, "revigo_in_", TYPE, "_", analysis_name, ".txt"
      )
      utils::write.table(
        enrichedGO,
        file = outfile,
        row.names = FALSE,
        sep = "\t",
        col.names = FALSE,
        quote = FALSE
      )
      message(paste("Just saved", outfile))
      ## run REVIGO
      revres <- txtFile2REVIGO(outfile,
        analysis_name = paste(analysis_name, TYPE, sep = "_"),
        out_path = out_path, python_path = python_path
      )
      ## store result
      rev_list[[paste0(an, TYPE)]] <- revres
    } else {
      fail_count <- fail_count + 1
    }
  }

  print(fail_count)
  if (fail_count == 3) {
    warning(
      "None of the GO categories contained sign.",
      "enriched GO terms based on the q-value cut off.",
      "The result will be an empty list."
    )
    return(NULL)
  } else {
    return(rev_list)
  }
}

#' Text file submission to REVIGO
#'
#' @description This function expects a txt file with two columns
#'   (GO term, p-value) that will be submitted to REVIGO.
#'   The result will be a data.frame that can be used to generate a treemap.
#'   If you want to start from a list of genes,
#'   consider [`GOenrichment_GOrilla_REVIGO()`].
#'
#' @param infile path to a txt file with GO terms and p-values
#' @param analysis_name a label for the analysis,
#'   e.g. "DB_condition", or "cluster1"
#' @param out_path path to where the (temporary) results are to be stored
#' @param python_path Define which python installation should be used to run the
#' REVIGO script. Default: "/usr/local/bin/python"
#'
#' @details The REVIGO API will be accessed with the help of a python script
#'   that is part of the ABCutilities package.
#'   This function needs python and sed;
#'   Make sure the Python path is the correct one.
#'   If NULL, it will use the output of `system("which python")`,
#'   which may not be approriate. Default: "/usr/local/bin/python".
#'
#' @return revigo data, whatever it may be.
#'
#' @seealso \code{\link{REVIGO_treemap}}, \code{\link{GOrilla2Revigo}}
#'
#' @examples \dontrun{
#' test <- txtFile2REVIGO(
#'   "data/rna-seq_deseq2_WT_vs_KO_0.05_revigo_in.txt",
#'   analysis_name = "testing"
#' )
#' head(test)
#' ## to generat the REVIGO maps
#' REVIGO_treemap(
#'   revigo.data = test,
#'   col_palette = "Set3",
#'   title = "testing"
#' )
#' }
#'
#' @export
#'
txtFile2REVIGO <- function(infile,
                           analysis_name = NULL,
                           out_path = NULL,
                           python_path = "/usr/local/bin/python") {

  # accessing the python script for the REVIGO API interaction ----------------
  py_script <- system.file(
    "extdata", "download_revigo_improved.py",
    package = "ABCutilities"
  )
  if (py_script == "") {
    stop(
      "Loading of the python script for submitting data to REVIGO failed."
    )
  }

  if (is.null(analysis_name)) {
    analysis_name <- paste0(
      "REVIGO_", format(Sys.time(), "%Y-%m-%d_%H%M%S")
    )
  }

  fname_out_revigo <- paste0(
    out_path, "REVIGO_results_", analysis_name
  )
  fname_out4 <- paste0(fname_out_revigo, ".R")

  fname_out_revigo2 <- paste0(fname_out_revigo, "_treemap_bp.R")
  message(
    paste(
      "Submitting", infile, "to REVIGO, generating",
      gsub(".*/", "", fname_out_revigo2)
    )
  )

  if (is.null(python_path)) {
    python <- system("which python", intern = TRUE)
  } else {
    python <- python_path
  }

  system(
    paste(
      python, py_script, infile, fname_out_revigo
    ),
    wait = TRUE
  )
  system(
    paste(
      "sed -n '/^revigo\\.data.*/,/^stuff.*/p'",
      fname_out_revigo2, "| egrep '^rev|^c' >", fname_out4
    )
  )
  message(print(fname_out4))
  source(fname_out4)

  if (!isTRUE(exists("revigo.data"))) {
    revigo.data <- NULL
  }
  return(revigo.data)
}
