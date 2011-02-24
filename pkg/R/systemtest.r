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
                            ... ) {
  ##@bdescr
  ##  constructor function for system test suites

  ret <- baseTestSuite(name, dirs, rngKind, rngNormalKind, 
                       test.dir=test.dir, scripts.regex=scripts.regex,
                       ...)
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
  requiredNames <-  c("name", "dirs", "test.dir", "scripts.regex",
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
  return(TRUE)
}



runSingleValidTestSuite.RSystemTestSuite <- function(self) {
  ## for each script (this means: let's build the vector, and loop)
  scripts <- list.files(self$dirs,
                        pattern=self$scripts.regex,
                        full.names=TRUE)
  for(script in scripts) {

    ## get the name of the directory containing 'run' containing script
    container <- basename(dirname(dirname(script)))

    ## get the basename of the script, without extension
    script <- basename(script)
    parts <- strsplit(script, '.', fixed=TRUE)[[1]]
    parts <- parts[-length(parts)]
    basename.script <- paste(parts, collapse='.')

    .testLogger$setCurrentSourceFile(paste(container, basename.script, sep='.'))

    testDirs <- list.files(paste(self$dirs, self$test.dir, basename.script, sep='/'),
                           full.names=TRUE)
    testDirs <- testDirs[file.info(testDirs)$isdir]
    
    if(length(testDirs) == 0) {
      .testLogger$addDeactivated(testFuncName=script)
    } else
    for(testDir in testDirs) {
      ## preparations
      testName <- basename(testDir)
      testFiles <- list.files(testDir, recursive=TRUE)

      ## sanity checks
      if(length(testFiles) == 0) {
        .testLogger$addDeactivated(testName)
        next
      }

      ## copy stuff to expected place
      unlink(paste(self$dirs, '..', "sandbox", sep='/'), recursive=TRUE)
      file.copy(from=path.expand(testDir), to=path.expand(paste(self$dirs, '..', sep='/')), recursive=TRUE)
      file.rename(from=paste(self$dirs, '..', testName, sep='/'),
                  to=paste(self$dirs, '..', 'sandbox', sep='/'))
      
      ## execute script in temporary environment
      sandbox <- new.env()
      pwd <- getwd()
      setwd(self$dirs)
      setwd('../sandbox')
      timing <- try(system.time(sys.source(paste(pwd, self$dirs, script, sep='/'), sandbox)))
      setwd(pwd)

      ## check it did not crash
      if(inherits(timing, 'try-error')) {
        .testLogger$addError(testFuncName=testName, errorMsg=geterrmessage())
      } else {
        for(targetName in list.files(testDir, pattern="--expected--*", recursive=TRUE)) {
          ## check output (name.out compared to case.out.name)
          target <- readLines(paste(testDir, targetName, sep='/'))
          current <- readLines(paste(self$dirs, '..', 'sandbox', targetName, sep='/'))
          if(any(current != target)) {
            diff <- which(current != target)
            msg <- paste(length(diff), " difference(s), namely at lines ", paste(diff, collapse=", "), ". ", sep="")
            .testLogger$addFailure(testFuncName=testName, failureMsg=msg)
          } else {
            .testLogger$addSuccess(testFuncName=testName, secs=round(timing[3], 2))
          }
        }
      }
      unlink(paste(self$dirs, '..', "sandbox", sep='/'), recursive=TRUE)
    }
  }
}
