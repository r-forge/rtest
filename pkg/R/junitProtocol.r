##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann, Mario Frasca
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

##  $Id: htmlProtocol.r 4 2010-09-22 09:41:59Z mariotomo $


printJUnitProtocol <- function(testData,
                               fileName = "") {

  ##@bdescr
  ##  Report generator
  ##  Extracts the log information stored in the 'RUnitTestData' test run object
  ##  and generates a XML output that is compatible with Hudson.
  ##@edescr
  ##
  ##@in  testData            : [RUnitTestData] S3 class object
  ##@in  fileName            : [character]
  ##@ret                     : [logical] TRUE if execution completed w/o error
  ##
  ##@codestatus : testing

  ## --------------------------------
  ##  CHECK OF INPUT DATA
  ## --------------------------------
  if (!is(testData, "RUnitTestData"))
  {
    stop("Argument 'testData' must be of class 'RUnitTestData'.")
  }

  if (!is.character(fileName))
  {
    stop("Argument 'fileName' has to be of type character.")
  }
  if (length(fileName) != 1)
  {
    stop("Argument 'fileName' must contain exactly one element.")
  }

  if(require("XML") == FALSE)
    return(invisible(FALSE))

  ## create the xml document
  rootNode <- xmlNode('testsuites')
  
  for(tsName in names(testData)) {
    errInfo <- testData[[tsName]]

    ## add n-th testsuite
    testSuiteNode <- xmlNode('testsuite',
                             attrs = c(
                               'name' = tsName,
                               'tests' = errInfo$nTestFunc,
                               'errors' = errInfo$nErr,
                               'failures' = errInfo$nFail,
                               'skip' = errInfo$nDeactivated))
    addChildren(rootNode, kids=list(testSuiteNode))

    res <- errInfo$sourceFileResults
    for(testFileName in names(res)) {
      testFuncNames <- names(res[[testFileName]])

      for(testFuncName in testFuncNames) {
        testFuncInfo <- res[[testFileName]][[testFuncName]]

        testCaseNode <- xmlNode('testcase',
                                attrs=c(
                                  'classname'=testFileName,
                                  'name'=testFuncName,
                                  'time'=testFuncInfo$time))
        addChildren(testSuiteNode, kids=list(testCaseNode))

        if(testFuncInfo$kind != 'success') {
          text <- paste('![CDATA[', testFuncInfo$traceBack, ']]', sep='')
          failureNode <- xmlNode('failure', text,
                                 attrs=c(
                                   'type'=testFuncInfo$kind,
                                   'message'=testFuncInfo$msg))
          addChildren(testCaseNode, kids=list(failureNode))
        }
      }
      
    }
  }

  saveXML(rootNode, fileName)
  
  return(invisible(TRUE))
}
