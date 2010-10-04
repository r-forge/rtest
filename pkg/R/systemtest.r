##  rtest : unit and system testing for R
##  Copyright (C) 2003-2010 Thomas Koenig, Matthias Burger, Klaus
##  Juenemann, Mario Frasca
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

systemTestSuite <- function(name, dirs, 
                            rngKind="Marsaglia-Multicarry",
                            rngNormalKind="Kinderman-Ramage",
                            test.dir='../tests/',
                            scripts.regex='.+\\.[rR]$',
                            case.in.pattern='%S-%T.in',
                            case.out.pattern='%S-%T.out',
                            name.in.match='read.PI',
                            name.out.match='write.PI',
                            ... ) {
  ##@bdescr
  ##  constructor function for system test suites

  ret <- baseTestSuite(name, dirs, rngKind, rngNormalKind, 
                       test.dir=test.dir, scripts.regex=scripts.regex,
                       case.in.pattern=case.in.pattern, case.out.pattern=case.out.pattern,
                       name.in.match=name.in.match, name.out.match=name.out.match, ...)
  class(ret) <- c("RSystemTestSuite", class(ret))
  invisible(ret)
}

extractMatching <- function(lines, perl.regex, preselect=NULL) {
  if(!is.null(preselect))
    lines <- lines[grep(preselect, lines, perl=TRUE)]
  found <- regexpr(perl.regex, lines, perl=TRUE)
  match.length <- attributes(found)$match.length[found != -1]
  start.pos <- found[found != -1]
  return(substr(lines[found != -1], start.pos, start.pos + match.length - 1))
}

patternToRegex <- function(case.in.pattern, basename.script, testCaseName=NULL, perl=FALSE) {
  pattern <- sub('%S', basename.script, case.in.pattern, fixed=TRUE)
  parts <- strsplit(pattern, "%T", fixed=TRUE)[[1]]
  if(!is.null(testCaseName))
    return(paste(parts[1], testCaseName, parts[2], sep=""))
  if(isTRUE(perl))
    return(paste("(?<=", parts[1], ").*?(?=", parts[2], ")", sep=""))
  else
    return(paste(parts[1], ".*?", parts[2], sep=""))
}

isValidTestSuite.RSystemTestSuite <- function(self) {
  requiredNames <-  c("name", "dirs", "test.dir", "scripts.regex", "case.in.pattern",
                      "case.out.pattern", "name.in.match","name.out.match",
                      "rngKind", "rngNormalKind")
  if(!all(requiredNames %in% names(self)))
  {
    warning("'self' object does not conform to S3 class definition. Not all list elements present.")
    return(FALSE)
  }

  if(!NextMethod(.Generic))
    return(FALSE)
  
  if(!(length(self[['dirs']]) == 1)) {
    warning(paste("length of 'dirs' element must be 1. --so sorry--."))
    return(FALSE)
  }
  
  lengths <- c(1, length(self$dirs))
  if(!(length(self[['test.dir']]) %in% lengths)) {
    warning(paste("length of 'test.dir' element must be 1 or match 'dirs'."))
    return(FALSE)
  }
  if(!all(file.exists(paste(self$dirs, self$test.dir, sep='/')))) {
    warning(paste("not all test directories exist."))
    return(FALSE)
  }
  if(!(length(self[['scripts.regex']]) %in% lengths)) {
    warning(paste("length of 'scripts.regex' element must be 1 or match 'dirs'."))
    return(FALSE)
  }
  if(!(length(self[['case.in.pattern']]) %in% lengths)) {
    warning(paste("length of 'case.in.pattern' element must be 1 or match 'dirs'."))
    return(FALSE)
  }
  if(!(length(self[['case.out.pattern']]) %in% lengths)) {
    warning(paste("length of 'case.out.pattern' element must be 1 or match 'dirs'."))
    return(FALSE)
  }
  return(TRUE)
}

runSingleValidTestSuite.RSystemTestSuite <- function(self) {
  ## for each script (this means: let's build the vector, and loop)
  scripts <- list.files(self$dirs,
                        pattern=self$scripts.regex,
                        full.names=TRUE)
  for(script in scripts) {
    .testLogger$setCurrentSourceFile(script)
    ## get the expected input and output file names
    lines <- readLines(script)
    script <- basename(script)
    name.in <- extractMatching(lines, "(?<=[\'\"]).*?(?=[\'\"])", preselect=self$name.in.match)
    name.out <- extractMatching(lines, "(?<=[\'\"]).*?(?=[\'\"])", preselect=self$name.out.match)

    if(length(name.in) != 1) {
      .testLogger$addError(testFuncName=script, paste("can't find the input name."))
      next
    }

    if(length(name.out) != 1) {
      .testLogger$addError(testFuncName=script, paste("can't find the output name."))
      next
    }

    ## get the basename of the script, without extension
    parts <- strsplit(script, '.', fixed=TRUE)[[1]]
    basename.script <- paste(parts[-length(parts)], collapse='.')

    ## replace the basename in the case.in.pattern and construct the
    ## corresponding regular expression, for use in list.files
    case.in.regex <- patternToRegex(self$case.in.pattern, basename.script)
    testFiles <- list.files(paste(self$dirs, self$test.dir, sep='/'),
                            pattern=case.in.regex,
                            full.names=TRUE)
    if(length(testFiles) == 0) {
      .testLogger$addDeactivated(testFuncName=script)
    } else
    for(case.in.name in testFiles) {
      ## preparations
      case.in.matcher <- patternToRegex(self$case.in.pattern, basename.script, perl=TRUE)
      testName <- extractMatching(case.in.name, case.in.matcher)
      case.out.name <- patternToRegex(self$case.out.pattern, basename.script, testName)
      case.out.name <- paste(self$dirs, self$test.dir, case.out.name, sep='/')

      ## sanity checks
      if(!file.exists(case.in.name)) {
        .testLogger$addError(paste("input for test case", testName, "not found.  skipping."))
        next
      }
      if(!file.exists(case.out.name)) {
        .testLogger$addError(paste("output for test case", testName, "not found.  skipping."))
        next
      }

      ## copy stuff to expected place
      file.copy(case.in.name, file.path(self$dirs, name.in), overwrite=TRUE)
      
      ## execute script in temporary environment
      sandbox <- new.env()
      pwd <- getwd()
      setwd(self$dirs)
      timing <- try(system.time(sys.source(script, sandbox)))
      setwd(pwd)

      ## check it did not crash
      if(inherits(timing, 'try-error')) {
        .testLogger$addError(testFuncName=paste(script, testName, sep=':'),
                             errorMsg=geterrmessage())
      } else {
        ## check output (name.out compared to case.out.name)
        target <- readLines(case.out.name)
        current <- readLines(file.path(self$dirs, name.out))
        if(any(current != target)) {
          diff <- which(current != target)
          msg <- paste(length(diff), " difference(s), namely at lines ", paste(diff, collapse=", "), ". ", sep="")
          .testLogger$addFailure(testFuncName=testName, failureMsg=msg)
        } else {
          .testLogger$addSuccess(testFuncName=testName, secs=round(timing[3], 2))
        }
      }
    }
  }
}
