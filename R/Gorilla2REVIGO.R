#' Submit genes to GOrilla and retrieve REVIGO data
#'
#' @details The GORILLA and REVIGO scripts are part of the installation of
#'   ABCutilities
#'   `system.file("extdata", "gorilla_submission.pl", package = "ABCutilities")`
#'   and
#'   `system.file(
#'     "extdata", "download_revigo_improved.py", package = "ABCutilities"
#'   )`.
#'
#' @param organism either one of
#'   "ARABIDOPSIS_THALIANA", "SACCHAROMYCES_CEREVISIAE",
#'   "CAENORHABDITIS_ELEGANS", "DROSOPHILA_MELANOGASTER","DANIO_RERIO",
#'   "HOMO_SAPIENS", "MUS_MUSCULUS", "RATTUS_NORVEGICUS"
#' @param gorilla_runmode either 'mhg' (one ranked target list) or
#'   'hg' (target and background list)
#' @param targets vector of gene symbols for which the GO terms
#'   will be retrieved
#' @param background vector of gene symbols for which the GO terms will be
#'   retrieved and compared to the targets
#' @param outdir path to directory where results should be stored
#' @param analysis_name a label for the analysis, e.g. "DB_condition",
#'   or "cluster1"
#' @param perl_path Define which perl installation should be used to run the
#'   GORILLA script. Set to "/opt/local/bin/perl".
#' @param python_path Define which python installation should be used to run
#'   the REVIGO script. Default: "/usr/local/bin/python"
#'
#' @return A list of 3, one for PROCESS, FUNCTION and COMPONENT.
#'   Each list contains the information that are needed to draw a
#'   REVIGO treemap: the GO terms, their grouping (e.g. "circadian rhythm") and
#'   several REVIGO stats. If a specific GO category did not yield any
#'   enriched results via GOrilla, that category will be
#'   missing from the resulting list here.
#'
#' @examples \dontrun{
#' rev_res_for_treemap <- GOenrichment_GOrilla_REVIGO(
#'   organism = "HOMO_SAPIENS",
#'   targets = "sandbox/forGOrilla_gD.txt",
#'   background = "sandbox/forGOrilla_backgroundPx.txt",
#'   outdir = "sandbox/test3/",
#'   gorilla_runmode = "hg", analysis_name = "blubb"
#' )
#'
#'
#' ## to generate the REVIGO maps
#' REVIGO_treemap(
#'   revigo.data = rev_res_for_treemap$test_PROCESS,
#'   col_palette = "Set3",
#'   title = "testing"
#' )
#' }
#' @export
#'
GOenrichment_GOrilla_REVIGO <- function(organism = NULL,
                                        gorilla_runmode = "hg",
                                        targets = NULL,
                                        background = NULL,
                                        outdir = NULL,
                                        analysis_name = NULL,
                                        perl_path = "/opt/local/bin/perl",
                                        python_path = "/usr/local/bin/python") {
  if (!is.null(outdir) && !grepl("/$", outdir)) {
    outdir <- paste0(outdir, "/")
  }
  ## submit gene list(s) to GOrilla, retrieve HTML
  gores <- submit_to_gorilla(
    organism = organism,
    targets = targets,
    background = background,
    outdir = outdir,
    runmode = gorilla_runmode,
    perl_path = perl_path
  )
  ## grep GO term results from HTML files, submit to REVIGO
  rr <- GOrilla2Revigo(
    GOrilla_link = outdir,
    out_path = outdir,
    analysis_name = analysis_name,
    python_path = python_path
  )
  return(rr)
}

#' Submit to GOrilla
#'
#' @description A wrapper for submitting one (or two) list(s) of g
#'   ene identifiers
#'   to [GOrilla website](http://cbl-gorilla.cs.technion.ac.il/).
#'
#' @details This function relies on a perl script downloaded from biostars
#'   <https://www.biostars.org/p/70064/>.
#'   It requires the following Perl modules:
#'   - `File::Path`
#'   - `WWW::Mechanize`
#'   - `Getopt::Long`
#'
#'   The easiest way to install those modules is to first install `cpanm` and
#'   use that do install the modules above:
#'   1. `cpan App::cpanminus`
#'   2. `cpanm Module::Name`, e.g. `cpanm WWW::Mechanize`
#'
#'   See <http://www.cpan.org/modules/INSTALL.html> for more details.
#'
#' @param organism either one of
#'   "ARABIDOPSIS_THALIANA", "SACCHAROMYCES_CEREVISIAE",
#'   "CAENORHABDITIS_ELEGANS", "DROSOPHILA_MELANOGASTER","DANIO_RERIO",
#'   "HOMO_SAPIENS", "MUS_MUSCULUS", "RATTUS_NORVEGICUS".
#' @param runmode either 'mhg' (one ranked target list) or
#'   'hg' (target and background list).
#' @param targets vector of gene symbols for
#'   which the GO terms will be retrieved.
#' @param background vector of gene symbols for which the
#'   GO terms will be retrieved and compared to the targets.
#' @param outdir path to directory where results should be stored.
#' @param perl_path Define which perl installation should be used.
#'   If set to `NULL` (default), the result of `system("which perl")`
#'   will be used.
#'   Depending on where you installed the specific modules,
#'   you may need to set a manual path here.
#'
#' @seealso [`GOrilla2Revigo()`]
#'
#' @examples \dontrun{
#' submit_to_gorilla(
#'   organism = "HOMO_SAPIENS",
#'   targets = "sandbox/forGOrilla_gA.txt",
#'   background = "sandbox/forGOrilla_backgroundPx.txt",
#'   outdir = "sandbox/test/",
#'   runmode = "hg",
#'   perl_path = "/opt/local/bin/perl"
#' )
#'
#' rev_res <- GOrilla2Revigo(
#'   GOrilla_link = "sandbox/test/",
#'   out_path = "/Users/frd2007/Documents/my_packs/sandbox/",
#'   analysis_name = "bla"
#' )
#'
#' ## to generate the REVIGO maps
#' REVIGO_treemap(
#'   revigo.data = rev_res$bla_PROCESS,
#'   col_palette = "Set3",
#'   title = "testing"
#' )
#' }
#'
#' @export
#'
submit_to_gorilla <- function(organism = NULL,
                              runmode = "hg",
                              targets = NULL,
                              background = NULL,
                              outdir = NULL,
                              perl_path = NULL) {

  ## basic error checks --------------------------------------------------------
  if (is.null(organism)) {
    stop("Define which organism to use.")
  }
  orgs <- c(
    "ARABIDOPSIS_THALIANA",
    "SACCHAROMYCES_CEREVISIAE",
    "CAENORHABDITIS_ELEGANS",
    "DROSOPHILA_MELANOGASTER",
    "DANIO_RERIO",
    "HOMO_SAPIENS",
    "MUS_MUSCULUS",
    "RATTUS_NORVEGICUS"
  )
  if (!(organism %in% orgs)) {
    stop(paste(
      organism,
      "is not eligible. Check the spelling; it should be either one of",
      paste(orgs, collapse = ", ")
    ))
  }
  if (runmode == "hg" && is.null(targets) && is.null(background)) {
    stop("Must supply both target and background list.")
  }
  if (is.null(targets)) {
    stop("Must supply target list.")
  }

  ### check perl path....
  if (is.null(perl_path)) {
    perl <- system("which perl", intern = TRUE)
    message(
      paste(
        "Since you didn't specify a perl path,",
        "this is the one we're going to use:",
        perl
      )
    )
  } else {
    perl <- perl_path
  }

  ### find script ......
  gorilla_script <- system.file(
    "extdata", "gorilla_submission.pl",
    package = "ABCutilities"
  )
  if (gorilla_script == "") {
    stop(
      "Didn't find the perl script for the GOrilla submission,",
      "`gorilla_submission.pl`. Try re-installing ABCutilities."
    )
  }

  ## do the submission to GORILLA ------
  # /usr/bin/env perl gorilla_submission.pl \
  #  --organism HOMO_SAPIENS --runmode hg \
  #  --target forGOrilla_gD.txt \
  #  --background forGOrilla_backgroundPx.txt \
  # --outputdir .
  if (is.null(outdir)) {
    outdir <- "./"
  }
  # if(!is.null(analysis_name)){outdir <- paste0(outdir, analysis_name, "/")}
  if (runmode == "mhg") {
    if (!is.null(background)) {
      warning(
        "You've selected runmode 'mhg',",
        "which is based on a single list of genes,",
        "but you supplied both target and background lists.",
        "Note that the background list will be ignored.",
        "Maybe runmode = 'mhg' is more appropriate?"
      )
    }
    system(paste(
      perl, gorilla_script, "--organism", organism, "--runmode", runmode,
      "--target", targets, "--outputdir", outdir
    ))
  } else if (runmode == "hg") {
    message("Running in two-sample-list-mode")
    system(paste(
      perl, gorilla_script, "--organism", organism, "--runmode", runmode,
      "--target", targets, "--background", background, "--outputdir", outdir
    ))
  } else {
    stop(
      paste("Runmode", runmode, "not recognized."),
      "Should be either 'mhg' or 'hg'."
    )
  }

  ## housekeeping --------------------------------------------------------
  system(paste("rm", paste0(outdir, "top.html")))
  system(paste("rm", paste0(outdir, "GOResults.html")))

  message(paste("Done with GOrilla. HTML results are in", outdir, "."))
}

#' Automated submission of GOrilla results to REVIGO
#'
#' @description This function takes as input the link to an individual GOrilla
#' result, which it uses to download the results, which are then submitted to
#' REVIGO.
#'
#' @details This functions makes all kinds of assumptions that may render it not
#'   as robust as other functions. It needs python and sed; make sure the Python
#'   pass is the correct one. If NULL, it will use the output of
#'   `system("which python")`, which may not be approriate.
#'   Default: "/usr/local/bin/python".
#'
#'   It also assumes that GOrilla has been executed for all GO term classes,
#'   i.e. PROCESS, FUNCTION, and COMPONENT.
#'
#' @param GOrilla_link either the full link to a GOrilla result,
#'   e.g. "http://cbl-gorilla.cs.technion.ac.il/GOrilla/oxso2ua3" or
#'   just the unique identifier ("oxso2ua3").
#'   Can also be the **path** to an already downloaded html file.
#' @param analysis_name a label for the analysis,
#'   e.g. "DB_condition", or "cluster1".
#' @param out_path path to where the (temporary) results are to be stored.
#'
#' @param python_path path to Python executable.
#'
#' @return A list of 3, one for PROCESS, FUNCTION and COMPONENT.
#'   Each list contains the information that are needed
#'   to draw a REVIGO treemap: the GO terms, their
#'   grouping (e.g. "circadian rhythm") and several REVIGO stats.
#'   If no GO terms were enriched, this function will return `NULL`.
#'
#'
#' @examples \dontrun{
#' test <- GOrilla2Revigo("gwlk3omz", "test")
#' # List of 3
#' # test_PROCESS  : chr [1:158, 1:7] "GO:0007623" "GO:0008152" "GO:0009987" ...
#' # test_FUNCTION : chr [1:63, 1:7] "GO:0001228" "GO:0003700" "GO:0001077" ...
#' # test_COMPONENT: chr [1:63, 1:7] "GO:0031974" "GO:0032991" "GO:0043226" ...
#' test2 <- GOrilla2Revigo("GOrilla_results_COMPONENT_test.txt")
#' # List of 3
#' # PROCESS  : chr [1:63, 1:7] "GO:0031974" "GO:0032991" "GO:0043226" ...
#' # FUNCTION : chr [1:63, 1:7] "GO:0031974" "GO:0032991" "GO:0043226" ...
#' # COMPONENT: chr [1:63, 1:7] "GO:0031974" "GO:0032991" "GO:0043226" ...
#'
#' ## to generate the REVIGO maps
#' REVIGO_treemap(
#'   revigo.data = test$test_PROCESS, col_palette = "Set3", title = "testing"
#' )
#' }
#'
#' @seealso [REVIGO_treemap], [submit_to_gorilla]
#'
#' @export
#'
GOrilla2Revigo <- function(GOrilla_link,
                           analysis_name = NULL,
                           out_path = NULL,
                           python_path = "/usr/local/bin/python") {

  ## preparing the link names --------------------------------------------------
  ID <- FALSE
  ## not a text file --> needs some wrangling
  if (!grepl("txt$", GOrilla_link)) {
    ## it's either a website or a path; website will be used as is
    if (grepl("/", GOrilla_link)) {
      ## not a website, i.e. it's a path to previously downloaded HTML results
      if (!grepl("html$", GOrilla_link)) {
        if (!grepl("/$", GOrilla_link)) {
          GOrilla_link <- paste0(GOrilla_link, "/")
        }
      }
      ## it's not a website or path (ot txt file) --> just the ID
    } else {
      GOrilla_link <- paste0(
        "http://cbl-gorilla.cs.technion.ac.il/GOrilla/",
        GOrilla_link,
        "/GOResults"
      )
      ID <- TRUE
    }
  }

  ## define analysis_name ------------------------------------------
  if (is.null(analysis_name)) {
    if (ID == TRUE) {
      analysis_name <- gsub(
        ".*GOrilla\\/([a-z0-9]+)\\/GOResult.*", "\\1", GOrilla_link
      )
    } else {
      analysis_name <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    }
  }

  ## accessing the python script for the REVIGO API interaction ----------------
  py_script <- system.file(
    "extdata", "download_revigo_improved.py",
    package = "ABCutilities"
  )
  if (py_script == "") {
    stop("Loading of the python script for submitting data to REVIGO failed.")
  }
  rev_list <- list()
  message(paste0("retrieving data from: ", GOrilla_link))

  ## do the deed for every type of GO term class--------------------------------
  for (TYPE in c("PROCESS", "FUNCTION", "COMPONENT")) {
    if (!is.null(analysis_name)) {
      fname_out_revigo <- paste0(
        out_path, "REVIGO_results_", TYPE, "_", analysis_name
      )
    } else {
      fname_out_revigo <- paste0(out_path, "REVIGO_results_", TYPE)
    }
    fname_out4 <- paste0(fname_out_revigo, ".R")

    ## extracting relevant parts from GOrilla html............................
    tmp_files <- fx.retrieve_GOrilla_results(
      GOrilla_link,
      TYPE, out_path,
      analysis_name
    )
    if (!is.null(tmp_files$html)) {
      system(paste("egrep '^GO:'", tmp_files$html, ">", tmp_files$txt))
      go_res <- system(paste("head", tmp_files$txt), intern = TRUE)
      if (length(go_res) == 0) {
        warning(
          paste(
            "Apparently, there were no GO terms enriched in",
            tmp_files$html, "."
          )
        )
        next()
      }
    } else {
      warning(
        "You seem to have supplied a txt file instead of a GOrilla link.",
        " The resulting REVIGO files will all contain the same results, ",
        "irrespective of their different names (PROCESS, FUNCTION, COMPONENT).",
        " Maybe you want to consider using the function txt2REVIGO."
      )
    }

    ## submitting GO term list with p-values to REVIGO........................
    fname_out_revigo2 <- paste0(fname_out_revigo, "_treemap_bp.R")
    message(paste(
      "Submitting",
      gsub(".*/", "", tmp_files$txt),
      "to REVIGO, generating", gsub(".*/", "", fname_out_revigo2)
    ))

    if (is.null(python_path)) {
      python <- system("which python", intern = TRUE)
    } else {
      python <- python_path
    }

    system(
      paste(python, py_script, tmp_files$txt, fname_out_revigo),
      wait = TRUE
    )
    system(paste(
      "sed -n '/^revigo\\.data.*/,/^stuff.*/p'",
      fname_out_revigo2, "| egrep '^rev|^c' >", fname_out4
    ))
    message(print(fname_out4))
    source(fname_out4)

    ## clean-up..............................................................
    # TODO: @luciorq What is block doing? I commented the next block
    # + revigo.data variable not declared anywhere
    # if (!is.null(analysis_name)) {
    #   rev_list[[paste(analysis_name, TYPE, sep = "_")]] <- revigo.data
    # }
  }
  return(rev_list)
}


#' Helper function for specifying where to get the GOrilla results from
#'
#' @description This function handles the different types of entries that can be
#' supplied to `GOrilla_link`.
#'
#' @param GOrilla_link GOrilla URL.
#'
#' @param TYPE Type of GOrilla result.
#'
#' @param out_path Path to save generated GOrilla output.
#'
#' @param analysis_name Name to use for analysis run.
#'
#' @export
fx.retrieve_GOrilla_results <- function(GOrilla_link,
                                        TYPE,
                                        out_path,
                                        analysis_name) {

  ## if a link to an online website is provided, use wget to retrieve it
  if (grepl("^http", GOrilla_link)) {
    fname <- paste0(GOrilla_link, TYPE, ".html")
    fname_gorillaHtml <- paste0(
      out_path, "GOrilla_results_", analysis_name, "_", TYPE, ".html"
    )
    system(paste("wget", fname, "-O", fname_gorillaHtml))
    fname_out_gorilla <- paste0(
      out_path, "GOrilla_results_", TYPE, "_", analysis_name, ".txt"
    )
  } else {
    if (grepl("\\.html$", GOrilla_link)) {
      fname_gorillaHtml <- paste0(GOrilla_link)
      fname_out_gorilla <- paste0(
        out_path, "GOrilla_results_", TYPE, "_", analysis_name, ".txt"
      )
    } else {
      if (grepl("txt$", GOrilla_link)) {
        fname_gorillaHtml <- NULL
        fname_out_gorilla <- GOrilla_link
      } else {
        fname_gorillaHtml <- system(
          paste0("ls ", GOrilla_link, "GOResults", TYPE, ".html"),
          intern = TRUE
        ) ## expects the nomenclature from gorilla_submission.pl, i.e. 'GOResultsFUNCTION.html'
        message(paste("Using this html:", fname_gorillaHtml))
        fname_out_gorilla <- paste0(
          out_path, "GOrilla_results_", TYPE, "_", analysis_name, ".txt"
        )
      }
    }
  }

  # TODO: @luciorq Previous commented text should bew removed?
  # if(grepl("txt$", GOrilla_link)){
  #    fname_out_gorilla <- GOrilla_link
  #  }else{
  #    fname_out_gorilla <- paste0(
  #     out_path, "GOrilla_results_", TYPE,"_", analysis_name, ".txt"
  #   )
  #  }

  return(list(txt = fname_out_gorilla, html = fname_gorillaHtml))
}
