setClass("HARMONIZEALLELES",
	representation = representation(
						strEqcCommand		=	"character",
						colInA1				=	"character",
						colInA2				=	"character",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInA1				=	"",
						colInA2				=	"",
						strTag				=	"HA"
						)
	#contains = c("EcfReader")
)

setGeneric("setHARMONIZEALLELES", function(object) standardGeneric("setHARMONIZEALLELES"))
setMethod("setHARMONIZEALLELES", signature = (object = "HARMONIZEALLELES"), function(object) {
	
	aEqcSlotNamesIn = c("colInA1", "colInA2", "strTag")
	#aEcfSlotNamesIn = c("arcdEditCol", "acolEdit")

	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		}
	}
	return(object)
})

#############################################################################################################################
validHARMONIZEALLELES <- function(objHA) {

	return(TRUE)
}

HARMONIZEALLELES.GWADATA.valid <- function(objHA, objGWA){ 

	if(!(objHA@colInA1 %in% objGWA@aHeader))
		stop(paste("EASY ERROR:HARMONIZEALLELES\n Column colInA1\n",objHA@colInA1," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
		
	if(!(objHA@colInA2 %in% objGWA@aHeader))
		stop(paste("EASY ERROR:HARMONIZEALLELES\n Column colInA2\n",objHA@colInA2," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
}

#############################################################################################################################
HARMONIZEALLELES.run <- function(objHA, objGWA, objREPORT, isValidScript) {
	
	colInA1 	<- objHA@colInA1
	colInA2 	<- objHA@colInA2
	
	strReportPrefix <- ifelse(objHA@strTag!="",paste(objHA@strTag,".",sep=""),"")
	strWriteSuffix <- ifelse(objHA@strTag!="",paste(".", objHA@strTag,sep=""),"")
	
	### A1=NA & A2=NA
	isBothMissing <- is.na(objGWA@tblGWA[[colInA1]]) & is.na(objGWA@tblGWA[[colInA2]])
	if(any(isBothMissing)) objGWA <- GWADATA.removerows(objGWA, which(isBothMissing))
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.numDrop_BothAllelesMissing",sep=""),length(which(isBothMissing)))
	rm(isBothMissing)
	
	objGWA@tblGWA[[colInA1]] <- toupper(objGWA@tblGWA[[colInA1]])
	objGWA@tblGWA[[colInA2]] <- toupper(objGWA@tblGWA[[colInA2]])
	
	A1in <- objGWA@tblGWA[[colInA1]]
	A2in <- objGWA@tblGWA[[colInA2]]
	
	##################
	## Recode missing single allele or '<DEL>' or '-' to 'D' and set the other to 'I'
	num_Recoded_DEL <- 0
	
	isRecode <- is.na(A1in)|A1in=='<DEL>'|A1in=='-'
	objGWA@tblGWA[[colInA1]][isRecode] <- "D"
	objGWA@tblGWA[[colInA2]][isRecode] <- "I"
	num_Recoded_DEL <- num_Recoded_DEL + length(which(isRecode))
	
	isRecode <- is.na(A2in)|A2in=='<DEL>'|A2in=='-'
	objGWA@tblGWA[[colInA1]][isRecode] <- "I"
	objGWA@tblGWA[[colInA2]][isRecode] <- "D"
	num_Recoded_DEL <- num_Recoded_DEL + length(which(isRecode))
	
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.num_Recoded_DEL",sep=""),num_Recoded_DEL)	
	
	##################
	## Recode MACH R/I -> D/I and R/D -> I/D
	num_Recoded_MACH_R <- 0
	
	isRecode <- A1in=="R" & A2in=="I"
	objGWA@tblGWA[[colInA1]][isRecode] <- "D"
	num_Recoded_MACH_R <- num_Recoded_MACH_R + length(which(isRecode))
	
	isRecode <- A1in=="R" & A2in=="D"
	objGWA@tblGWA[[colInA1]][isRecode] <- "I"
	num_Recoded_MACH_R <- num_Recoded_MACH_R + length(which(isRecode))
	
	isRecode <- A2in=="R" & A1in=="I"
	objGWA@tblGWA[[colInA2]][isRecode] <- "D"
	num_Recoded_MACH_R <- num_Recoded_MACH_R + length(which(isRecode))
	
	isRecode <- A2in=="R" & A1in=="D"
	objGWA@tblGWA[[colInA2]][isRecode] <- "I"
	num_Recoded_MACH_R <- num_Recoded_MACH_R + length(which(isRecode))
	
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.num_Recoded_MACH_R",sep=""),num_Recoded_MACH_R)
	
	##################
	## Recode MACH sth/I -> D/I and sth/D -> I/D
	num_Recoded_AI2DI <- 0
	
	isRecode <- A1in=="I" & A2in!="D"
	objGWA@tblGWA[[colInA2]][isRecode] <- "D"
	num_Recoded_AI2DI <- num_Recoded_AI2DI + length(which(isRecode))
	
	isRecode <- A1in!="D" & A2in=="I"
	objGWA@tblGWA[[colInA1]][isRecode] <- "D"
	num_Recoded_AI2DI <- num_Recoded_AI2DI + length(which(isRecode))
	
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.num_Recoded_AI2DI",sep=""),num_Recoded_AI2DI)
	## 
	num_Recoded_AD2ID <- 0
	
	isRecode <- A1in=="D" & A2in!="I"
	objGWA@tblGWA[[colInA2]][isRecode] <- "I"
	num_Recoded_AD2ID <- num_Recoded_AD2ID + length(which(isRecode))
	
	isRecode <- A1in!="I" & A2in=="D"
	objGWA@tblGWA[[colInA1]][isRecode] <- "I"
	num_Recoded_AD2ID <- num_Recoded_AD2ID + length(which(isRecode))
	
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.num_Recoded_AD2ID",sep=""),num_Recoded_AD2ID)
	
	
	##################
	## Recode Sequence coding to D/I
	
	## reload alleles
	A1in <- objGWA@tblGWA[[colInA1]]
	A2in <- objGWA@tblGWA[[colInA2]]
	
	num_Recoded_SEQ <- 0
	isRecode <- nchar(A1in)>nchar(A2in)
	
	if(any(isRecode)) {
	
		idxRecode = which(isRecode)
		isSeqA1 = unlist(lapply(strsplit(A1in[idxRecode],""),function(x) all(x%in%c("A","C","G","T","N"))))
		isSeqA2 = unlist(lapply(strsplit(A2in[idxRecode],""),function(x) all(x%in%c("A","C","G","T","N"))))
		isRecode[idxRecode[which(!(isSeqA1&isSeqA2))]] <- FALSE
		
		objGWA@tblGWA[[colInA1]][isRecode] <- "I"
		objGWA@tblGWA[[colInA2]][isRecode] <- "D"
		num_Recoded_SEQ <- num_Recoded_SEQ + length(which(isRecode))		
	}
	
	isRecode <- nchar(A1in)<nchar(A2in)

	if(any(isRecode)) {
	
		idxRecode = which(isRecode)
		isSeqA1 = unlist(lapply(strsplit(A1in[idxRecode],""),function(x) all(x%in%c("A","C","G","T","N"))))
		isSeqA2 = unlist(lapply(strsplit(A2in[idxRecode],""),function(x) all(x%in%c("A","C","G","T","N"))))
		isRecode[idxRecode[which(!(isSeqA1&isSeqA2))]] <- FALSE
		
		objGWA@tblGWA[[colInA1]][isRecode] <- "D"
		objGWA@tblGWA[[colInA2]][isRecode] <- "I"
		num_Recoded_SEQ <- num_Recoded_SEQ + length(which(isRecode))	
	}
	
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.num_Recoded_SEQ",sep=""),num_Recoded_SEQ)
	
	##################
	##################
	
	isInvalid <- !(objGWA@tblGWA[[colInA1]]%in%c("A","C","G","T","I","D")&objGWA@tblGWA[[colInA2]]%in%c("A","C","G","T","I","D")&objGWA@tblGWA[[colInA1]]!=objGWA@tblGWA[[colInA2]])
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"HA.numDrop_InvalidAlleles",sep=""),length(which(isInvalid)))
	if(any(isInvalid) & isValidScript) {
		objGWA.invalid <- GWADATA.getrows(objGWA, which(isInvalid))
		GWADATA.write(objGWA.invalid, strSuffix = paste(strWriteSuffix,".HA.numDrop_InvalidAlleles",sep=""))
		objGWA <- GWADATA.removerows(objGWA, which(isInvalid))
	}
	
	rm(A1in)
	rm(A2in)
	rm(isRecode)
	rm(isInvalid)
	
	lsOut <- list(objGWA, objREPORT)
	
	return(lsOut)
}

HARMONIZEALLELES <- function(strEqcCommand){ 
	## Wrapper for class definition
	HARMONIZEALLELESout <- setHARMONIZEALLELES(new("HARMONIZEALLELES", strEqcCommand = strEqcCommand))
	validHARMONIZEALLELES(HARMONIZEALLELESout)
	return(HARMONIZEALLELESout)	
}
