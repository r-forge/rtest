library("rtest")

options(warn=1)

testSuite <- defineTestSuite(name="rtest",
                            dirs=".",
                            testFileRegexp="runit.*\\.r$",
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(testSuite, verbose=0L)
printTextProtocol(testData, showDetails=FALSE)

