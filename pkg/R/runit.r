##  RUnit : A unit test framework for the R programming language
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

unitTestSuite <- function(name, dirs, 
                          rngKind="Marsaglia-Multicarry",
                          rngNormalKind="Kinderman-Ramage",
                          testFileRegexp="^runit.+\\.[rR]$",
                          testFuncRegexp="^test.+", ...) {
  ##@bdescr
  ##  Convenience functions to handle test suites
  ##@edescr
  ##
  ##@in  name           : [character] test suite title used in protocol
  ##@in  dirs           : [character] vector of paths to search for test case files
  ##@in  testFileRegexp : [character] regular expression string to match file names
  ##@in  testFuncRegexp : [character] (vector) regular expression string(s) to match test case functions within all test case files
  ##@in  rngKind        : [character] name of the RNG version, see RNGversion()
  ##@in  rngNormalKind  : [character] name of the RNG version for the rnorm, see RNGversion()
  ##@ret                : [RUnitTestSuite] S3 class (list) object, ready for test runner
  ##
  ##@codestatus : testing

  ret <- baseTestSuite(name, dirs, rngKind, rngNormalKind,
                       testFileRegexp=testFileRegexp, testFuncRegexp=testFuncRegexp,
                       ...)
  class(ret) <- c("RUnitTestSuite", class(ret))
  return(invisible(ret))
}

##
## name as of before the transition to generic
##
defineTestSuite <- function(name, dirs, 
                            testFileRegexp="^runit.+\\.[rR]$",
                            testFuncRegexp="^test.+",
                            rngKind="Marsaglia-Multicarry",
                            rngNormalKind="Kinderman-Ramage" )
  unitTestSuite(name, dirs, rngKind, rngNormalKind, testFileRegexp, testFuncRegexp)

.setUp <- function() {
  ##@bdescr
  ##  Internal Function.
  ##  Default function to be executed once for each test case before the test case gets executed.
  ##  This function can be adopted to specific package requirements for a given project.
  ##  Need to replace this default with a new function definition.
  ##  Function cannot take arguments and does not have a return value.
  ##@edescr
  ##
  ##@codestatus : internal
  
  return(invisible())
}

.tearDown <- function() {
  ##@bdescr
  ##  Internal Function.
  ##  Default function to be executed once for each test case after the test case got executed.
  ##  This function can be adopted to specific package requirements for a given project.
  ##  Need to replace this default with a new function definition.
  ##  Function cannot take arguments and does not have a return value.
  ##@edescr
  ##
  ##@codestatus : internal
  
  return(invisible())
}

.executeTestCase <- function(self, funcName, envir, setUpFunc, tearDownFunc) {
  ##@bdescr
  ##  Internal Function.
  ##  Execute individual test case, record logs and change state of global TestLogger object.
  ##@edescr
  ##
  ##@in  funcName     : [character] name of test case function
  ##@in  envir        : [environment]
  ##@in  setUpFunc    : [function]
  ##@in  tearDownFunc : [function]
  ##@ret              : [NULL]
  ##
  ##@codestatus : internal
  
  ##  write to stdout for logging

  func <- get(funcName, envir=envir)
  ## anything else than a function is ignored.
  if(mode(func) != "function") {
    return(invisible())
  }

  if (.testLogger$getVerbosity() > 0) {
    cat("\n\nExecuting test function", funcName, " ... ")
  }
  
  ## safe execution of setup function
  res <- try(setUpFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .setUp before", funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".setUp (before ", funcName, ")", sep=""),
                         errorMsg=message)
    return(invisible())
  }

  ## reset book keeping variables in .testLogger
  .testLogger$cleanup()
  ## ordinary test function execution:
  timing <- try(system.time(func()))
  if (inherits(timing, "try-error")) {
    if(.testLogger$isFailure()) {
      .testLogger$addFailure(testFuncName=funcName,
                             failureMsg=geterrmessage())
    }
    else if(.testLogger$isDeactivated()) {
      .testLogger$addDeactivated(testFuncName=funcName)
    }
    else {
      .testLogger$addError(testFuncName=funcName,
                           errorMsg=geterrmessage())
    }
  }
  else {
    .testLogger$addSuccess(testFuncName=funcName, secs=round(timing[3], 2))
  }

  ##  add number of check function calls within test case
  .testLogger$addCheckNum(testFuncName=funcName)
  
  ## safe execution of tearDown function
  res <- try(tearDownFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .tearDown after", funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".tearDown (after ", funcName, ")", sep=""),
                         errorMsg=message)
    return(invisible())
  }

  if (.testLogger$getVerbosity() > 0) {
    cat(" done successfully.\n\n")
  }
  return(invisible())
}

.sourceTestFile <- function(self, absTestFileName) {
  ##@bdescr
  ## This function sources a file, finds all the test functions in it, executes them
  ## and reports the results to the TestLogger.
  ## No return value, called for its side effects on TestLogger object
  ##@edescr
  ##
  ##@in  absTestFileName : [character] absolute path name of the file to test
  ##@in  testFuncRegexp  : [character] a regular expression identifying the names of test functions
  ##@ret                 : [NULL]
  ##
  ##@codestatus : internal

  .testLogger$setCurrentSourceFile(absTestFileName)
  if (!file.exists(absTestFileName)) {
    msgText <- paste("Test case file ", absTestFileName, " not found.")
    .testLogger$addError(testFuncName=absTestFileName, errorMsg=msgText)
    return(invisible())
  }

  sandbox <- new.env(parent=.GlobalEnv)
  ##  will be destroyed after function closure is left
  
  ##  catch syntax errors in test case file
  res <- try(sys.source(absTestFileName, envir=sandbox, keep.source=TRUE))
  if (inherits(res, "try-error")) {
    message <- paste("Error while sourcing ", absTestFileName, ":", geterrmessage())
    .testLogger$addError(testFuncName=absTestFileName, errorMsg=message)
    return(invisible())
  }
  ##  test file provides definition of .setUp/.tearDown
  if (exists(".setUp", envir=sandbox, inherits=FALSE)) {
    .setUp <- get(".setUp", envir=sandbox)
  }
  if (exists(".tearDown", envir=sandbox, inherits=FALSE)) {
    .tearDown <- get(".tearDown", envir=sandbox)
  }
  testFunctions <- ls(pattern=self$testFuncRegexp, envir=sandbox)
  for (funcName in testFunctions) {
    .executeTestCase(self, funcName, envir=sandbox, setUpFunc=.setUp, tearDownFunc=.tearDown)
  }
  
}

.setStandardRandomNumberGenerator <- function(self)
  UseMethod('.setStandardRandomNumberGenerator')

.setStandardRandomNumberGenerator.RUnitTestSuite <- function(self) {
  ## set a standard random number generator.
  RNGkind(kind=self$rngKind, normal.kind=self$rngNormalKind)
}

isValidTestSuite.RUnitTestSuite <- function(self) {
  ##@bdescr
  ##  Helper function
  ##  checks 'RUnitTestSuite' class object features
  ##@edescr
  ##
  ##@in   self : [RUnitTestSuite] S3 class (list) object, input object for test runner
  ##@ret            : [logical] TRUE if self is valid
  ##
  ##@codestatus : testing
  
  ##  check required elements, irrespective of order, allow for additional elements
  requiredNames <-  c("name", "dirs", "testFileRegexp", "testFuncRegexp",
                      "rngKind", "rngNormalKind")
  if(!all(requiredNames %in% names(self)))
  {
    warning("'self' object does not conform to S3 class definition. Not all list elements present.")
    return(FALSE)
  }

  if(!NextMethod(.Generic))
    return(FALSE)

  ##  RNGkind has an internal list of valid names which cannot be accessed
  ##  programmatically. Furthermore, users can define their own RNG and select that one
  ##  so we have to leave it to RNGkind() to check if the arguments are valid.
  if (length(self[["rngKind"]]) != 1) {
    warning(paste("'rngKind' element may only contain exactly one name."))
    return(FALSE)
  }
  if (length(self[["rngNormalKind"]]) != 1) {
    warning(paste("'rngNormalKind' element may only contain exactly one name."))
    return(FALSE)
  }
  return(TRUE)
}


runSingleValidTestSuite.RUnitTestSuite <- function(self) {
  testFiles <- list.files(self$dirs,
                          pattern = self$testFileRegexp,
                          full.names=TRUE)
  for(testFile in testFiles) {
    .setStandardRandomNumberGenerator(self)
    .sourceTestFile(self, testFile)
  }
}


runTestFile <- function(absFileName, useOwnErrorHandler=TRUE, 
                        testFuncRegexp="^test.+",
                        rngKind="Marsaglia-Multicarry",
                        rngNormalKind="Kinderman-Ramage",
                        verbose=getOption("RUnit")$verbose) {
  ##@bdescr
  ##  Convenience function.
  ##@edescr
  ##
  ##@in  absFileName        : [character] complete file name of test cases code file
  ##@in  useOwnErrorHandler : [logical] if TRUE RUnits error handler will be used
  ##@in  testFuncRegexp     : [character]
  ##@in  rngKind            : [character] name of the RNG, see RNGkind for avialbale options
  ##@in  rngNormalKind      : [character] name of the RNG for rnorm, see RNGkind for avialbale options
  ##@in  verbose            : [integer] >= 1: (default) write begin/end comments for each test case, 0: ommit begin/end comment (passed on to function runTestSuite)
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  ##  all error checking and handling is delegated to function runTestSuite
  
  fn <- basename(absFileName)
  nn <- strsplit(fn, "\\.")[[1]][1]
  dn <- dirname(absFileName)
  ts <- defineTestSuite(name=nn, dirs=dn, 
                        testFileRegexp=paste("^", fn, "$", sep=""),
                        testFuncRegexp=testFuncRegexp,
                        rngKind=rngKind,
                        rngNormalKind=rngNormalKind)
                        
  return(runTestSuite(ts, useOwnErrorHandler=useOwnErrorHandler,
                      verbose=verbose))
}
