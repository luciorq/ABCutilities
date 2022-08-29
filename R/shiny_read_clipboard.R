#---Functions taken from the psych package--------------------------------------
## a number of functions to read data from the clipboard for both Macs and PCs

read.clipboard <- function(header = TRUE, ...) {
  MAC <- Sys.info()[1] == "Darwin"
  if (!MAC) {
    if (header) {
      return(utils::read.table(file("clipboard"), header = TRUE, ...))
    } else {
      return(utils::read.table(file("clipboard"), ...))
    }
  } else {
    if (header) {
      return(utils::read.table(pipe("pbpaste"), header = TRUE, ...))
    } else {
      return(utils::read.table(pipe("pbpaste"), ...))
    }
  }
}

# same as read.clipboard(sep=',')
read.clipboard.csv <- function(header = TRUE,
                               sep = ",",
                               ...) {
  MAC <- Sys.info()[1] == "Darwin"
  if (!MAC) {
    if (header) {
      read.clipboard <- utils::read.table(
        file("clipboard"),
        header = TRUE, sep, ...
      )
    } else {
      read.clipboard <- utils::read.table(
        file("clipboard"),
        sep = sep, ...
      )
    }
  } else {
    if (header) {
      read.clipboard <- utils::read.table(
        pipe("pbpaste"),
        header = TRUE, sep, ...
      )
    } else {
      read.clipboard <- utils::read.table(
        pipe("pbpaste"),
        sep = sep, ...
      )
    }
  }
}

# same as read.clipboard(sep='\t')
read.clipboard.tab <- function(header = TRUE,
                               sep = "\t",
                               ...) {
  MAC <- Sys.info()[1] == "Darwin"
  if (!MAC) {
    if (header) {
      read.clipboard <- utils::read.table(
        file("clipboard"),
        header = TRUE, sep, ...
      )
    } else {
      read.clipboard <- utils::read.table(
        file("clipboard"),
        sep = sep, ...
      )
    }
  } else {
    if (header) {
      read.clipboard <- utils::read.table(
        pipe("pbpaste"),
        header = TRUE, sep, ...
      )
    } else {
      read.clipboard <- utils::read.table(
        pipe("pbpaste"),
        sep = sep, ...
      )
    }
  }
}


# adapted from John Fox's read.moments function
# modified October 31, 2010 to be able to read row names as first column
# corrected September 2, 2011 to be able to read row names as first column but
# without the diagonal
read.clipboard.lower <- function(diag = TRUE,
                                 names = FALSE,
                                 ...) {
  MAC <- Sys.info()[1] == "Darwin" # are we on a Mac using the Darwin system?
  if (!MAC) {
    con <- file("clipboard")
  } else {
    con <- pipe("pbpaste")
  }
  xij <- scan(con, what = "char")
  close(con)
  m <- length(xij)
  d <- if (diag || names) 1 else -1
  n <- floor((sqrt(1 + 8 * m) - d) / 2)
  if (names) {
    name <- xij[cumsum(1:n)]
    xij <- xij[-cumsum(seq(1:n))]
    d <- if (diag) 1 else -1
    n <- floor((sqrt(1 + 8 * (m - n)) - d) / 2)
  }
  xij <- as.numeric(xij)
  X <- diag(n)
  X[upper.tri(X, diag = diag)] <- xij
  diagonal <- diag(X)
  X <- t(X) + X
  diag(X) <- diagonal
  if (!names) name <- paste("V", 1:n, sep = "")
  if (!names) name <- paste("V", 1:n, sep = "")
  if (names && !diag) {
    rownames(X) <- colnames(X) <- c(name, paste("V", n, sep = ""))
  } else {
    rownames(X) <- colnames(X) <- name
  }
  return(X)
}


# fixed April 30, 2016
read.clipboard.upper <- function(diag = TRUE,
                                 names = FALSE,
                                 ...) {
  MAC <- Sys.info()[1] == "Darwin" # are we on a Mac using the Darwin system?
  if (!MAC) {
    con <- file("clipboard")
  } else {
    con <- pipe("pbpaste")
  }
  xij <- scan(con, what = "char")
  close(con)
  m <- length(xij)
  d <- if (diag || names) 1 else -1

  n <- floor((sqrt(1 + 8 * m) - d) / 2) # solve the quadratic for n

  if (names) {
    name <- xij[1:n]
    xij <- xij[-c(1:n)]
  }

  xij <- as.numeric(xij)
  X <- diag(n)
  X[lower.tri(X, diag = diag)] <- xij
  diagonal <- diag(X)
  X <- t(X) + X
  diag(X) <- diagonal
  if (!names) name <- paste("V", 1:n, sep = "")
  rownames(X) <- colnames(X) <- name
  return(X)
}



# added March, 2010 to read fixed width input
read.clipboard.fwf <- function(header = FALSE,
                               widths = rep(1, 10),
                               ...) {
  # are we on a Mac using the Darwin system?
  MAC <- Sys.info()[1] == "Darwin"
  if (!MAC) {
    if (header) {
      read.clipboard <- utils::read.fwf(
        file("clipboard"),
        header = TRUE, widths = widths, ...
      )
    } else {
      read.clipboard <- utils::read.fwf(
        file("clipboard"),
        widths = widths, ...
      )
    }
  } else {
    if (header) {
      read.clipboard <- utils::read.fwf(
        pipe("pbpaste"),
        header = TRUE, widths = widths, ...
      )
    } else {
      read.clipboard <- utils::read.fwf(
        pipe("pbpaste"),
        widths = widths, ...
      )
    }
  }
}

# added May, 2014 to read from https files
read.https <- function(filename,
                       header = TRUE) {
  temp <- tempfile() # create a temporary file
  # copy the https file to temp
  utils::download.file(
    filename,
    destfile = temp,
    method = "curl"
  )
  # now, do the normal read.table command
  result <- utils::read.table(temp, header = header)
  unlink(temp) # get rid of the temporary file
  return(result)
} # give us the result
