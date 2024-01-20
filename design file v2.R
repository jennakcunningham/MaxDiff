# github access token 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
dat<-gen.factorial(3,3,varNames=c("A","B","C"))
desD<-optFederov(~quad(.),dat,nTrials=14,eval=TRUE) # Choose an optimum 14 trail design.
optBlock(~quad(.),desD$design,c(7,7))
optBlock(~quad(.),dat,c(7,7))
BIB<-optBlock(~.,withinData=factor(1:7),blocksizes=rep(3,7))
crossprod(table(BIB$rows,c(rep(1:7, rep(3,7)))))

MaxDiffDesign(number.alternatives, number.questions, alternatives.per.question,
              n.versions = 1, n.repeats = 1000, seed = 1223)

install.packages("remotes", repos='http://cran.us.r-project.org')
remotes::install_github("erikerhardt/flipMaxDiff")
install.packages("MaxDiff", dependencies = TRUE, repos = "http://cran.rstudio.com")
