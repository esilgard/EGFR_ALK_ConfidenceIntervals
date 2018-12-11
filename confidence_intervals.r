# written by Aasthaa Bansal and Qin Sun 

# internal validation datasets
setwd("C:/Users/qsun/Desktop/NLP Study/InternalValidationOutput/InternalValidationOutput/ReportedOrNot")
report <- TRUE
result <- method <- insufficient <- FALSE

setwd("C:/Users/qsun/Desktop/NLP Study/InternalValidationOutput/InternalValidationOutput/Result")
result <- TRUE
report <- method <- insufficient <- FALSE

setwd("C:/Users/qsun/Desktop/NLP Study/InternalValidationOutput/InternalValidationOutput/Method")
method <- TRUE
report <- result <- insufficient <- FALSE

setwd("C:/Users/qsun/Desktop/NLP Study/InternalValidationOutput/InternalValidationOutput/InsufficientOrUnknown")
insufficient <- TRUE
report <- result <- method <- FALSE

# external validation datasets
setwd("C:/Users/qsun/Desktop/NLP Study/ExternalValidationOutput/ExternalValidationOutput/ReportedOrNot")
report <- TRUE
result <- method <- insufficient <- FALSE

setwd("C:/Users/qsun/Desktop/NLP Study/ExternalValidationOutput/ExternalValidationOutput/Result")
result <- TRUE
report <- method <- insufficient <- FALSE

setwd("C:/Users/qsun/Desktop/NLP Study/ExternalValidationOutput/ExternalValidationOutput/Method")
method <- TRUE
report <- result <- insufficient <- FALSE

setwd("C:/Users/qsun/Desktop/NLP Study/ExternalValidationOutput/ExternalValidationOutput/InsufficientOrUnknown")
insufficient <- TRUE
report <- result <- method <- FALSE


# read in report-level txt files
data <- NULL
for(i in 1:5) {
   currdata <- read.table(paste(i,".txt",sep=""), header=T, sep="\t", 
      col.names=c("CaseId", "ReportId", "TestInstance", "InstanceId", "SystemOutput", "GoldStandardLabel"))
   data <- rbind(data, currdata)
}

# get estimates and 95% CIs
n <- 1000

tests <- c("EGFR", "ALK")
for(t in 1:length(tests)) {
   print(tests[t])	
	
   currTest <- subset(data, TestInstance==tests[t])
   dim(currTest)
   currTest[1:5,]
   
   currTest$GoldStandardLabel <- as.character(currTest$GoldStandardLabel)
   currTest$SystemOutput <- as.character(currTest$SystemOutput)
   
   if(report) {
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="Reported")] <- "TRUE"
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="NotReported"|currTest$GoldStandardLabel=="Unknown"|currTest$GoldStandardLabel=="Insufficient"|currTest$GoldStandardLabel=="Negative")] <- "FALSE"
      currTest$SystemOutput[which(currTest$SystemOutput=="Reported")] <- "TRUE"
      currTest$SystemOutput[which(currTest$SystemOutput=="NotReported")] <- "FALSE"
   }
   else if(result) {
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="Positive")] <- "TRUE"
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="Negative")] <- "FALSE"
      currTest$SystemOutput[which(currTest$SystemOutput=="Positive")] <- "TRUE"
      currTest$SystemOutput[which(currTest$SystemOutput=="Negative")] <- "FALSE"
   }
   else if(method) {
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="MutationalAnalysis" | currTest$GoldStandardLabel=="FISH")] <- "TRUE"
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="OTHER")] <- "FALSE"
      currTest$SystemOutput[which(currTest$SystemOutput =="MutationalAnalysis" | currTest$SystemOutput=="FISH")] <- "TRUE"
      currTest$SystemOutput[which(currTest$SystemOutput=="OTHER")] <- "FALSE"
   }
   else if(insufficient) {
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="Insufficient")] <- "TRUE"
      currTest$GoldStandardLabel[which(currTest$GoldStandardLabel=="Unknown")] <- "FALSE"
      currTest$SystemOutput[which(currTest$SystemOutput=="Insufficient")] <- "TRUE"
      currTest$SystemOutput[which(currTest$SystemOutput=="Unknown")] <- "FALSE"
   }
      	
   cases <- subset(currTest, GoldStandardLabel=="TRUE")
   ctrls <- subset(currTest, GoldStandardLabel=="FALSE")
   Se <- mean(cases$SystemOutput=="TRUE")
   Sp <- mean(ctrls$SystemOutput=="FALSE")
   
   rep <- subset(currTest, SystemOutput =="TRUE")
   notRep <- subset(currTest, SystemOutput =="FALSE")
   ppv <- mean(rep$GoldStandardLabel =="TRUE")
   npv <- mean(notRep$GoldStandardLabel=="FALSE")
   
   subjs <- unique(currTest$CaseId)
   nSubj <- length(subjs)  
   
   SeStar <- SpStar <- ppvStar <- npvStar <- vector(length=n)
       
   for(j in 1:n) {
   	  currSubjs <- sample(subjs, nSubj, replace=T)
   	     
   	  currData <- currTest[which(!is.na(match(currTest$CaseId, currSubjs))),]
   	     
      cases <- subset(currData, GoldStandardLabel=="TRUE")
      ctrls <- subset(currData, GoldStandardLabel=="FALSE")
      SeStar[j] <- mean(cases$SystemOutput=="TRUE")
      SpStar[j] <- mean(ctrls$SystemOutput=="FALSE")
      
      rep <- subset(currData, SystemOutput =="TRUE")
      notRep <- subset(currData, SystemOutput =="FALSE")
      ppvStar[j] <- mean(rep$GoldStandardLabel =="TRUE")
      npvStar[j] <- mean(notRep$GoldStandardLabel=="FALSE")
   }
   
   est <- round(Se, 3)
   ci <- round(quantile(SeStar, probs=c(0.025,0.975)),3)
   print(paste(est, " (", ci[1], ",", ci[2], ")", sep=""), quote=F)
   
   est <- round(Sp, 3)
   ci <- round(quantile(SpStar, probs=c(0.025,0.975)),3)
   print(paste(est, " (", ci[1], ",", ci[2], ")", sep=""), quote=F)
   
   est <- round(ppv, 3)
   ci <- round(quantile(ppvStar, probs=c(0.025,0.975)),3)
   print(paste(est, " (", ci[1], ",", ci[2], ")", sep=""), quote=F)
   
   est <- round(npv, 3)
   ci <- round(quantile(npvStar, probs=c(0.025,0.975)),3)
   print(paste(est, " (", ci[1], ",", ci[2], ")", sep=""), quote=F)
}
