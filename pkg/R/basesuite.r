##  rtest : unit and system testing for R
##  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; version 2 of the License.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##  $Id$

## this file describes the generic functions and implements the
## .default and .RTestSuite implementations.  each derived class is
## described in a separate file.

## in the derived classes please try to follow the order: creator,
## private functions (here not present), overriden methods (here:
## methods to override), public utility functions (here: base methods
## that don't need overriding).

baseTestSuite <- function(name, dirs, 
                          rngKind="Marsaglia-Multicarry",
                          rngNormalKind="Kinderman-Ramage", ...) {
  
  if (missing(dirs)) {
    stop("argument 'dirs' is missing without a default.")
  }
  if (missing(name)) {
    warning("argument 'name' is missing. using basename(dirs)[1] instead.")
    name <- basename(dirs)
  }

  ret <- list(name=name,
              dirs=dirs,
              rngKind=rngKind,
              rngNormalKind=rngNormalKind, ...)
  class(ret) <- c('RBaseTestSuite', class(ret))
  invisible(ret)
}

### --------------------------------------------------------------------
### isValidTestSuite
isValidTestSuite <- function(self)
  UseMethod('isValidTestSuite')

isValidTestSuite.default <- function(self) {
  warning(paste("'testSuite' object is not of class 'RUnitTestSuite'."))
  return(FALSE)
}

isValidTestSuite.RBaseTestSuite <- function(self) {
  for(i in seq_along(self))
  {
    if(!is.character(self[[i]]))
    {
      warning(paste("'self' object does not conform to S3 class definition.\n",
                    names(self)[i], " element has to be of type 'character'."))
      return(FALSE)
    }
    if(self[[i]] == "")
    {
      warning(paste("'self' object does not conform to S3 class definition.\n",
                    names(self)[i], " element may not be empty string."))
      return(FALSE)
    }
  }
  
  if (!all(file.exists(self[["dirs"]])))
  {
    warning(paste("specified directory", paste(self[["dirs"]], collapse=", "), "not found."))
    return(FALSE)
  }

  if (length(self[["name"]]) != 1) {
    warning(paste("'name' element may only contain exactly one name."))
    return(FALSE)
  }
  return(TRUE)
}

### --------------------------------------------------------------------
### runSingleValidTestSuite
runSingleValidTestSuite <- function(self)
  UseMethod('runSingleValidTestSuite')

runSingleValidTestSuite.RBaseTestSuite <- function(self) {
  .testLogger$addError("don't know how to run a base test suite.")
}

### --------------------------------------------------------------------
### runTestSuite
runTestSuite <- function(testSuite, ...)
  UseMethod('runTestSuite')

runTestSuite.default <- function(testSuite, ...)
  .testLogger$addError("runTestSuite invoked on a generic object.")

runTestSuite.list <- function(testSuite, ...) {
  ## testSuite is actually a list of suites
  for(i in seq_along(testSuite))
    runTestSuite(testSuite[[i]], ...)
}

runTestSuite.RBaseTestSuite <- function(testSuite, ..., useOwnErrorHandler=TRUE, verbose=getOption("RUnit")$verbose) {
  ##@bdescr
  ## This is the main function of the RUnit framework. It identifies all specified
  ## test files and triggers all required actions. At the end it creates a test
  ## protocol data object. 
  ## IMPORTANT to note, the random number generator is (re-)set to the default
  ## methods specified in defineTestSuite() before each new test case *file* is sourced. 
  ## This guarantees that each new test case set defined together in on file can rely
  ## on the default, even if the random number generator version is being reconfigured in some
  ## previous test case file(s).
  ##@edescr
  ##
  ##@in  testSuite         : [list] list of test suite lists
  ##@in  useOwnErrorHandler : [logical] TRUE (default) : use the RUnit error handler
  ##@in  verbose            : [integer] >= 1: (default) write begin/end comments for each test case, 0: omit begin/end comment 
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  if (!is.logical(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' has to be of type logical.")
  }
  if (length(useOwnErrorHandler) != 1) {
    stop("argument 'useOwnErrorHandler' has to be of length 1.")
  }
  if (is.na(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' may not contain NA.")
  }

  oFile <- getOption("RUnit")$outfile
  if (!is.null(oFile)) {
    if(is.character(oFile)) {
      ##  connection has to be open when handed on to sink
      oFile <- file(oFile, "w")
    } else if(!inherits(oFile, "connection")) {
      stop("'outfile' must be a connection or a character string.")
    }
    sink(file=oFile)
    sink(file=oFile, type="message")
    resetStream <- function() {
      sink(type="message")
      sink()
      flush(oFile)
      close(oFile)
      ##close(oFile)
    }
    on.exit(resetStream())
  }
  ##  record RNGkind and reinstantiate on exit
  rngDefault <- RNGkind()
  on.exit(RNGkind(kind=rngDefault[1], normal.kind=rngDefault[2]), add=TRUE)
  
  oldErrorHandler <- getOption("error")
  ## reinstall error handler
  on.exit(options(error=oldErrorHandler), add=TRUE)
  
  ## initialize TestLogger
  assign(".testLogger", .newTestLogger(useOwnErrorHandler), envir=.GlobalEnv)
  .testLogger$setVerbosity(verbose)

  inputTestSuite <- testSuite
  ## main loop
  for(i in seq_along(inputTestSuite$dirs)) {
    testSuite$name <- paste(inputTestSuite$name, i, sep='.')
    testSuite$dirs <- inputTestSuite$dirs[[i]]
    if(!isValidTestSuite(testSuite)) {
      errMsg <- paste("Invalid test suite", testSuite$name, ". Test run skipped.")
      .testLogger$addError(testSuite$name, errMsg)
    }
    .testLogger$setCurrentTestSuite(testSuite)
    runSingleValidTestSuite(testSuite)
  }

  ret <- .testLogger$getTestData()
  
  return(ret)
}

