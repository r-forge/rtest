%%  rtest : unit and system testing for R
%%  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; version 2 of the License.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

%%  $Id$

\encoding{latin1}
\name{systemTestSuite}
\alias{systemTestSuite}
\alias{isValidTestSuite.RSystemTestSuite}
\alias{runSingleValidTestSuite.RSystemTestSuite}
\title{Definition and execution of RSystem test suites.}

\usage{
systemTestSuite(name, dirs, rngKind, rngNormalKind, test.dir,
 scripts.regex, ...)
\method{isValidTestSuite}{RSystemTestSuite}(self)
}

\arguments{
  \item{name}{The name of the test suite.}
  \item{dirs}{Vector of absolute directory names where to look for test
    files.}
  \item{rngKind}{name of an available RNG  (see \code{\link[base:Random]{RNGkind}} 
    for possible options).}
  \item{rngNormalKind}{name of a valid rnorm RNG version (see 
    \code{\link[base:Random]{RNGkind}} for possible options).}
  \item{test.dir}{the directory that holds the test cases.}
  \item{scripts.regex}{character: the name of the scripts to test.}
  \item{self}{A single object, of class RUnitTestSuite or similar.}
  \item{...}{passed to included functions or for later extensions.}
}

\description{
  \code{systemTestSuite} is a helper function to define a test
  suite. See below for a precise definition of a test suite.

  \code{isValidTestSuite} checks if an object defines a valid test suite.
}

\details{
  The basic idea of the RUnit test framework is to declare a certain set
  of functions to be test functions and report the results of their
  execution. The test functions must not take any parameter nor return
  anything such that their execution can be automatised.

  The specification which functions are taken as test functions is
  contained in an object of class  \code{RUnitTestSuite} which is a list
  with the following elements.
  \describe{
  \item{name}{A simple character string. The name of a test suite is mainly used 
    to create a well structure test protocol.}
  \item{dirs}{A character vector containing the absolute names of all
    directories where to look for test files.}
  \item{testFileRegexp}{A regular expression specifying the test
    files. All files in the test directories whose names match this
    regular expression are taken as test files. Order of file names will
    be alphabetical but depending on the used locale.}
  \item{testFuncRegexp}{A regular expression specifying the test
    functions. All functions defined in the test files whose names match
    this regular expression are used as test functions. Order of test
    functions will be alphabetical.}
}

  After the RUnit framework has sequentially executed all test suites it returns all
  data collected during the test run as an object of class
  \code{RUnitTestData}. This is a (deeply nested)
  list with one list element for each executed test suite. Each of these
  executed test suite lists contains the following elements:
  \describe{
  \item{nTestFunc}{The number of test functions executed in the test
    suite.}
  \item{nErr}{The number of errors that occurred during the execution.}
  \item{nFail}{The number of failures that occurred during the execution.}
  \item{dirs}{The test directories of the test suite.}
  \item{testFileRegexp}{The regular expression for identifying the test
    files of the test suite.}
  \item{testFuncRegexp}{The regular expression for identifying the test
    functions of the test suite.}
  \item{sourceFileResults}{A list containing the results for each
    separate test file of the test suite.}
}

  The \code{sourceFileResults} list just mentioned contains one element
  for each specified test function in the source file. This element is a list with
  the following entries:
  \describe{
  \item{kind}{Character string with one of \code{success}, \code{error} or 
    \code{failure} describing the outcome of the test function.}
  \item{msg}{the error message in case of an error or failure and
    \code{NULL} for a successfully executed test function.}
  \item{time}{The duration (measured in seconds) of the successful
    execution of a test function and \code{NULL} in the case of an error
    or failure.}
  \item{traceBack}{The full trace back as a character vector in the case of an 
    error and \code{NULL} otherwise.}
}

  To further control test case execution it is possible to define two
  parameterless function \code{.setUp} and \code{\link{.tearDown}} in each
  test file. \code{.setUp()} is executed directly before and
  \code{.tearDown()} directly after each test function execution.

  Quite often, it is useful to base test cases on random numbers. To
  make this procedure reproducible, the function \code{runTestSuite}
  sets the random number generator to the default setting
  \code{RNGkind(kind="Marsaglia-Multicarry",
    normal.kind="Kinderman-Ramage")} before sourcing each test file
  (note that this default has been chosen due to historical reasons and
  differs from the current R default). This default can be overwritten
  by  configuring the random number generator at the beginning of a test
  file. This setting, however, is valid only inside its own source file
  and gets overwritten when the next test file is sourced.
}


\value{
  \code{runTestSuite} and \code{runTestFile} both return an object of
  class RUnitTestData.

  \code{defineTestSuite} returns an object of class  \code{RUnitTestSuite}.
}

\author{
Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{&} Matthias Burger}

\seealso{
  \code{\link{checkTrue}} and friends for writing test cases.
  \code{\link{printTextProtocol}} and \code{\link{printHTMLProtocol}}
  for printing the test protocol. See \link{RUnit-options} for global
  options controlling log out.
}


\examples{

## run some test suite
myTestSuite <- defineTestSuite("RUnit Example",
                               system.file("examples", package = "rtest"),
                               testFileRegexp = "correctTestCase.r")
testResult <- runTestSuite(myTestSuite)

##  same but without the logger being involved
## source(file.path(system.file("examples", package = "rtest"), 
##                  "correctTestCase.r"))
## test.correctTestCase()


## prints detailed text protocol
## to standard out:
printTextProtocol(testResult, showDetails = TRUE)

##  use current default RNGs
myTestSuite1 <- defineTestSuite("RUnit Example",
                               system.file("examples", package = "rtest"),
                               testFileRegexp = "correctTestCase.r",
                               rngKind = "Mersenne-Twister",
                               rngNormalKind = "Inversion")

testResult1 <- runTestSuite(myTestSuite)


##  for single test files, e.g. outside a package context
testResult2 <- runTestFile(file.path(system.file("examples", 
                                                 package = "rtest"),
                                     "correctTestCase.r"))
printTextProtocol(testResult2, showDetails = TRUE)
}

\keyword{programming}
\concept{RUnit}
