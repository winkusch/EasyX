setClass("ADJUSTALLELES",
	representation = representation(
						strEqcCommand		=	"character",
						colInStrand			=	"character",
						colInA1				=	"character",
						colInA2				=	"character",
						colInFreq			=	"character",
						acolInFreq			=	"character",
						colInBeta			=	"character",
						acolInBeta			=	"character",
						acolInOr			=	"character",
						colRefStrand		=	"character",
						colRefA1			=	"character",
						colRefA2			=	"character",
						blnMetalUseStrand	=	"logical",
						blnAAMerge			=	"logical",
						blnWriteMismatch	=	"logical",
						blnWriteInvalid		=	"logical",
						blnWriteRefInvalid	=	"logical",
						blnRemoveMismatch	=	"logical",
						blnRemoveInvalid	=	"logical",
						blnRemoveRefInvalid	=	"logical",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInStrand			=	"",
						colInA1				=	"",
						colInA2				=	"",
						colInFreq			=	"",
						acolInFreq			=	"",
						colInBeta			=	"",
						acolInBeta			=	"",
						acolInOr			=	"",
						colRefStrand		=	"",
						colRefA1			=	"",
						colRefA2			=	"",
						blnMetalUseStrand	=	FALSE,
						blnAAMerge			=	FALSE,
						blnWriteMismatch	=	TRUE,
						blnWriteInvalid		=	TRUE,
						blnWriteRefInvalid	=	FALSE,
						blnRemoveMismatch	=	FALSE,
						blnRemoveInvalid	=	FALSE,
						blnRemoveRefInvalid	=	FALSE,
						strTag				=	"AA"
						)
)

setGeneric("setADJUSTALLELES", function(object) standardGeneric("setADJUSTALLELES"))
setMethod("setADJUSTALLELES", signature = (object = "ADJUSTALLELES"), function(object) {
	
	aEqcSlotNamesIn = c("colInStrand", "colInA1", "colInA2", "colInFreq", "colInBeta", 
						"colRefStrand", "colRefA1", "colRefA2", 
						"blnMetalUseStrand", 
						"acolInBeta","acolInOr",
						"acolInFreq",
						"blnWriteMismatch",
						"blnRemoveMismatch",
						"blnWriteInvalid",
						"blnRemoveInvalid",
						"blnWriteRefInvalid",
						"blnRemoveRefInvalid",
						"strTag"
						) 
						
	### Last 4 are inherited from class GWADATA and can be used with ADJUSTALLELES for reference file!
					
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
ADJUSTALLELES.valid <- function(objAA) {
	
	if(objAA@colInA1 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELES\n colInA1 is not being specified. \n PLease set colInA1.", sep=""))
	if(objAA@colInA2 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELES\n colInA2 is not being specified. \n PLease set colInA2.", sep=""))
	if(objAA@colRefA1 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELES\n colRefA1 is not being specified. \n PLease set colRefA1.", sep=""))
	if(objAA@colRefA2 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELES\n colRefA2 is not being specified. \n PLease set colRefA2.", sep=""))
	if(objAA@colInFreq == ""&all(objAA@acolInFreq==""))
		warning(paste(" EASY WARNING:ADJUSTALLELES\n No --colInFreq or --acolInFreq defined. Alleles are adjusted to ref without flipping the frequency !!!!!", sep=""))
	if(objAA@colInBeta==""&all(objAA@acolInBeta=="")&all(objAA@acolInOr==""))
		warning(paste(" EASY WARNING:ADJUSTALLELES\n No --colInBeta, --acolInBeta or --acolInOr defined. Alleles are adjusted to ref without flipping the effect or odds ratio !!!!!", sep=""))

	return(TRUE)
		
}

ADJUSTALLELES.GWADATA.valid <- function(objAA, objGWA) {
	
	if(!(objAA@colInStrand %in% objGWA@aHeader) & (objAA@colInStrand != ""))
		warning(paste(" EASY WARNING:ADJUSTALLELES\n Defined column colInStrand \n",objAA@colInStrand, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n Strand will be set to + for all SNPs.", sep=""))
		
	if(!(objAA@colInA1 %in% objGWA@aHeader) & (objAA@colInA1 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colInA1 \n",objAA@colInA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAA@colInA2 %in% objGWA@aHeader) & (objAA@colInA2 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colInA2 \n",objAA@colInA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAA@colInFreq %in% objGWA@aHeader) & (objAA@colInFreq != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colInFreq \n",objAA@colInFreq, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAA@colInBeta %in% objGWA@aHeader) & (objAA@colInBeta != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colInBeta \n",objAA@colInBeta, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAA@colRefStrand %in% objGWA@aHeader) & (objAA@colRefStrand != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colRefStrand \n",objAA@colRefStrand, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAA@colRefA1 %in% objGWA@aHeader) & (objAA@colRefA1 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colRefA1 \n",objAA@colRefA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAA@colRefA2 %in% objGWA@aHeader) & (objAA@colRefA2 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined column colRefA2 \n",objAA@colRefA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(any(!(objAA@acolInBeta %in% objGWA@aHeader)) & (objAA@acolInBeta[1] != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined acolInBeta columns \n",paste(objAA@acolInBeta[which(!(objAA@acolInBeta %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(objAA@colInBeta != "" & objAA@acolInBeta[1] != "" )
		stop(paste(" EASY ERROR:ADJUSTALLELES\n PLease either specify --colInBeta OR --acolInBeta! You cannot use both versions.", sep=""))
	
	if(any(!(objAA@acolInOr %in% objGWA@aHeader)) & (objAA@acolInOr[1] != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined acolInOr columns \n",paste(objAA@acolInOr[which(!(objAA@acolInOr %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	if(any(!(objAA@acolInFreq %in% objGWA@aHeader)) & (objAA@acolInFreq[1] != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELES\n Defined acolInFreq columns \n",paste(objAA@acolInFreq[which(!(objAA@acolInFreq %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(objAA@colInFreq != "" & objAA@acolInFreq[1] != "" )
		stop(paste(" EASY ERROR:ADJUSTALLELES\n PLease either specify --colInFreq OR --acolInFreq! You cannot use both versions.", sep=""))
		
}
ADJUSTALLELES.FlipStrand <- function(tblIn, idxA1=NA, idxA2=NA, idxStrand=NA) {
	
	tblOut <- tblIn
	
	if(!is.na(idxStrand)) {
		tblOut[[idxStrand]] <-  ifelse(tblIn[[idxStrand]] == "+", "-", "+")
	}
	
	if(!is.na(idxA1)) {
		isSwitch <- tblIn[[idxA1]] == "A"
		if(any(isSwitch)) tblOut[[idxA1]][isSwitch] <- "T"
		isSwitch <- tblIn[[idxA1]] == "T"
		if(any(isSwitch)) tblOut[[idxA1]][isSwitch] <- "A"
		isSwitch <- tblIn[[idxA1]] == "C"
		if(any(isSwitch)) tblOut[[idxA1]][isSwitch] <- "G"
		isSwitch <- tblIn[[idxA1]] == "G"
		if(any(isSwitch)) tblOut[[idxA1]][isSwitch] <- "C"
	}
	if(!is.na(idxA2)) {
		isSwitch <- tblIn[[idxA2]] == "A"
		if(any(isSwitch)) tblOut[[idxA2]][isSwitch] <- "T"
		isSwitch <- tblIn[[idxA2]] == "T"
		if(any(isSwitch)) tblOut[[idxA2]][isSwitch] <- "A"
		isSwitch <- tblIn[[idxA2]] == "C"
		if(any(isSwitch)) tblOut[[idxA2]][isSwitch] <- "G"
		isSwitch <- tblIn[[idxA2]] == "G"
		if(any(isSwitch)) tblOut[[idxA2]][isSwitch] <- "C"
	}
	## "I"/"D" no change
	
	return(tblOut)
}
ADJUSTALLELES.return <- function(objGWA, objREPORT, objAA, tblOut, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand, tblRefInvalid) {
	
	strSuffix <- ifelse(objAA@strTag!="",paste(".", objAA@strTag,sep=""),"")
	
	if(!isAVRefStrand) {
		if(ncol(tblOut)>0) tblOut <- tblOut[,-ncol(tblOut),with=FALSE]
		if(ncol(tblMisMatch)>0) tblMisMatch <- tblMisMatch[,-ncol(tblMisMatch),with=FALSE]
		if(ncol(tblInvalid)>0) tblInvalid <- tblInvalid[,-ncol(tblInvalid),with=FALSE]
		if(ncol(tblRefInvalid)>0) tblRefInvalid <- tblRefInvalid[,-ncol(tblRefInvalid),with=FALSE]
	}
	if(!isAVInStrand)  {
		if(ncol(tblOut)>0) tblOut <- tblOut[,-ncol(tblOut),with=FALSE]
		if(ncol(tblMisMatch)>0) tblMisMatch <- tblMisMatch[,-ncol(tblMisMatch),with=FALSE]
		if(ncol(tblInvalid)>0) tblInvalid <- tblInvalid[,-ncol(tblInvalid),with=FALSE]
		if(ncol(tblRefInvalid)>0) tblRefInvalid <- tblRefInvalid[,-ncol(tblRefInvalid),with=FALSE]
	}

	if(!objAA@blnRemoveMismatch) {
		## re-add mismatch
		tblOut <- rbind(tblOut, tblMisMatch)
	}
	if(!objAA@blnRemoveInvalid) {
		## re-add invalid
		tblOut <- rbind(tblOut, tblInvalid)
	}
	if(!objAA@blnRemoveRefInvalid) {
		## re-add invalid
		tblOut <- rbind(tblOut, tblRefInvalid)
	}
	#objGWA@tblGWA 		<- data.frame()
	objGWA@tblGWA 		<- data.table()
	objGWA.adj 			<- objGWA
	objGWA.adj@tblGWA 	<- tblOut
	objGWA.adj 			<- GWADATA.reset(objGWA.adj)
	
	if(isValidScript) {
		if(objAA@blnWriteMismatch & dim(tblMisMatch)[1] > 0) {
			objGWA.miss 		<- objGWA
			objGWA.miss@tblGWA 	<- tblMisMatch
			objGWA.miss 		<- GWADATA.reset(objGWA.miss)
			GWADATA.write(objGWA.miss, strSuffix = paste(strSuffix,".mismatch",sep=""))
			rm(objGWA.miss)
		}
		if(objAA@blnWriteInvalid & dim(tblInvalid)[1] > 0) {
			objGWA.invalid 			<- objGWA
			objGWA.invalid@tblGWA 	<- tblInvalid
			objGWA.invalid 			<- GWADATA.reset(objGWA.invalid)
			GWADATA.write(objGWA.invalid, strSuffix = paste(strSuffix,".invalid",sep=""))
			rm(objGWA.invalid)
		}
		if(objAA@blnWriteRefInvalid & dim(tblRefInvalid)[1] > 0) {
			objGWA.refinvalid 			<- objGWA
			objGWA.refinvalid@tblGWA 	<- tblRefInvalid
			objGWA.refinvalid 			<- GWADATA.reset(objGWA.refinvalid)
			GWADATA.write(objGWA.refinvalid, strSuffix = paste(strSuffix,".refinvalid",sep=""))
			rm(objGWA.refinvalid)
		}
	}
	
	return(list(objGWA.adj,objREPORT))
}
#############################################################################################################################
ADJUSTALLELES.run <- function(objAA, objGWA, objREPORT, isValidScript) {
						
	# adjust SNPs in 2nd table according to 1st 
	# leave 1st table
	# only change intersect with different alleles
	##aColRef = c("Strand.ref", "A1.ref", "A2.ref")
	##aColIn = c("Strand.in", "A1.in", "A2.in","EAF.in", "BETA.in")
	strReportPrefix <- ifelse(objAA@strTag!="",paste(objAA@strTag,".",sep=""),"")
	#strReportPrefix <- paste(objAA@strTag,".",sep="")
	
	#### Init report variables
	# objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefixDrop,"AlleleInNotValid",sep=""),0)
	# objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefixDrop,"AlleleRefNotValid",sep=""),0)
	# objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefixDrop,"StrandInNotValid",sep=""),0)
	# objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefixDrop,"StrandRefNotValid",sep=""),0)	
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"Checked",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"StrandChange",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleMatch",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleChange",sep=""),0)
	
	if(objAA@blnMetalUseStrand) {
		objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"n4AlleleMatch",sep=""),0)
		objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"n4AlleleChange",sep=""),0)
	}
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleMismatch",sep=""),0)
	# objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleOrStrandInvalid",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleInMissing",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleInInvalid",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"StrandInInvalid",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleRefMissing",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"AlleleRefInvalid",sep=""),0)
	objREPORT <- REPORT.addval(objREPORT,paste(strReportPrefix,"StrandRefInvalid",sep=""),0)	
	
	####
	
	tblIn <- objGWA@tblGWA
	#objGWA@tblGWA <- data.frame()
	objGWA@tblGWA <- data.table()
	
	#################################################################################
	#### Get indices and availability of columns

	idxRefStrand 	= which(names(tblIn)==objAA@colRefStrand)
	idxRefA1 		= which(names(tblIn)==objAA@colRefA1)
	idxRefA2 		= which(names(tblIn)==objAA@colRefA2)
	
	isAVRefStrand = ifelse(length(idxRefStrand) == 1, TRUE, FALSE) 
	if(!isAVRefStrand) {
		#names(tblIn)[names(tblIn) == "Strand.ref"] <- "Strand.ref.old"
		#setnames(tblIn,"Strand.ref","Strand.ref.old")
		tblIn$Strand.ref = rep("+", dim(tblIn)[1]) 
		idxRefStrand = ncol(tblIn)
	}
	
	idxInStrand 	= which(names(tblIn)==objAA@colInStrand)
	idxInA1 		= which(names(tblIn)==objAA@colInA1)
	idxInA2 		= which(names(tblIn)==objAA@colInA2)
	idxInFreq		= which(names(tblIn)==objAA@colInFreq)
	aidxInFreq		= which(names(tblIn)%in%objAA@acolInFreq)
	idxInBeta 		= which(names(tblIn)==objAA@colInBeta)
	aidxInBeta 		= which(names(tblIn)%in%objAA@acolInBeta)
	aidxInOr 		= which(names(tblIn)%in%objAA@acolInOr)
	
	isAVInStrand 	= ifelse(length(idxInStrand) == 1, TRUE, FALSE) 
	if(!isAVInStrand) {
		#names(tblIn)[names(tblIn) == "Strand.in"] <- "Strand.in.old"
		#setnames(tblIn,"Strand.in","Strand.in.old")
		tblIn$Strand.in = rep("+", dim(tblIn)[1]) 
		idxInStrand = ncol(tblIn)
	}
	isAVInFreq 	= ifelse(length(idxInFreq) == 1, TRUE, FALSE)
	isAVInFreqs	= ifelse(length(aidxInFreq) > 0, TRUE, FALSE)
	isAVInBeta 	= ifelse(length(idxInBeta) == 1, TRUE, FALSE)
	isAVInBetas	= ifelse(length(aidxInBeta) > 0, TRUE, FALSE)
	isAVInOrs	= ifelse(length(aidxInOr) > 0, TRUE, FALSE)
		
	#################################################################################
	#### Coerce columns
	tblIn[[idxRefStrand]]	<- as.character(tblIn[[idxRefStrand]])
	tblIn[[idxRefA1]] 		<- as.character(tblIn[[idxRefA1]])
	tblIn[[idxRefA2]] 		<- as.character(tblIn[[idxRefA2]])
	tblIn[[idxInStrand]] 	<- as.character(tblIn[[idxInStrand]])
	tblIn[[idxInA1]] 		<- as.character(tblIn[[idxInA1]])
	tblIn[[idxInA2]] 		<- as.character(tblIn[[idxInA2]])
	if(isAVInFreq) tblIn[[idxInFreq]] <- as.numeric(tblIn[[idxInFreq]])
	if(isAVInFreqs) {
		for(idxFreqTmp in aidxInFreq) 
			tblIn[[idxFreqTmp]] <- as.numeric(tblIn[[idxFreqTmp]])
	}
	if(isAVInBeta) tblIn[[idxInBeta]] <- as.numeric(tblIn[[idxInBeta]])
	if(isAVInBetas) {
		for(idxBetaTmp in aidxInBeta) 
			tblIn[[idxBetaTmp]] <- as.numeric(tblIn[[idxBetaTmp]])
	}
	if(isAVInOrs) {
		for(idxOrTmp in aidxInOr) 
			tblIn[[idxOrTmp]] <- as.numeric(tblIn[[idxOrTmp]])
	}
	
	# class(tblIn[,idxRefStrand]) <- "character"
	# class(tblIn[,idxRefA1]) 	<- "character"
	# class(tblIn[,idxRefA2]) 	<- "character"
	# class(tblIn[,idxInStrand]) 	<- "character"
	# class(tblIn[,idxInA1]) 		<- "character"
	# class(tblIn[,idxInA2]) 		<- "character"
	# class(tblIn[,idxInFreq]) 	<- "numeric"
	# class(tblIn[,idxInBeta]) 	<- "numeric"
	
	tblIn[[idxRefA1]] 	<- toupper(tblIn[[idxRefA1]])
	tblIn[[idxRefA2]] 	<- toupper(tblIn[[idxRefA2]])
	tblIn[[idxInA1]] 	<- toupper(tblIn[[idxInA1]])
	tblIn[[idxInA2]] 	<- toupper(tblIn[[idxInA2]])
	
	#################################################################################
	#### Output data-frames
	# tblOut 			<- data.frame()
	# tblMisMatch 	<- data.frame()
	# tblInvalid 		<- data.frame()
	# tblRefInvalid 	<- data.frame()
	tblOut 			<- data.table()
	tblMisMatch 	<- data.table()
	tblInvalid 		<- data.table()
	tblRefInvalid 	<- data.table()
	
	#################################################################################
	#### CHECK ALLELE and STRAND coding of input and reference
	######################
	### Input columns
	## Remove NA In Alleles
	iRemoveInAlleleNA = which(is.na(tblIn[[idxInA1]]) | is.na(tblIn[[idxInA2]]))
	if(length(iRemoveInAlleleNA)>0) {
		### This is a mismatch
		### Exclude from output
		#tblMisMatch <- rbind(tblMisMatch, tblIn[iRemoveInAlleleNA,])
		tblInvalid 	<- rbind(tblInvalid, tblIn[iRemoveInAlleleNA,])
		tblIn 		<- tblIn[-iRemoveInAlleleNA,]
		objREPORT 	<- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleInMissing",sep=""),length(iRemoveInAlleleNA))
	}
	rm(iRemoveInAlleleNA)
	## Remove Alleles that dont match acgt
	# iRemoveInAlleleNotValid = which(!(tblIn[,idxInA1] %in% c("A","C","G","T")) | !(tblIn[,idxInA2] %in% c("A","C","G","T")))
	iRemoveInAlleleNotValid = which(!(tblIn[[idxInA1]] %in% c("A","C","G","T","I","D")) | !(tblIn[[idxInA2]] %in% c("A","C","G","T","I","D")))
	if(length(iRemoveInAlleleNotValid)>0) {
		### This is a mismatch
		### Exclude from output
		#tblMisMatch <- rbind(tblMisMatch, tblIn[iRemoveInAlleleNotValid,])
		tblInvalid 	<- rbind(tblInvalid, tblIn[iRemoveInAlleleNotValid,])
		tblIn 		<- tblIn[-iRemoveInAlleleNotValid,]
		objREPORT 	<- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleInInvalid",sep=""),length(iRemoveInAlleleNotValid))
	}
	rm(iRemoveInAlleleNotValid)
	## Recode missing strand to +

	iRecodeInStrandMiss = which(is.na(tblIn[[idxInStrand]]))
	if(length(iRecodeInStrandMiss)>0) {
		#tblIn[iRecodeInStrandMiss,idxInStrand] <- "+"
		tblIn[[idxInStrand]][iRecodeInStrandMiss] <- "+"
	}
	rm(iRecodeInStrandMiss)
	## Remove Strand that dont match +,-

	iRemoveInStrandNotValid = which(!(tblIn[[idxInStrand]] %in% c("+","-")))
	if(length(iRemoveInStrandNotValid)>0) {
		### This is a mismatch
		### Exclude from output
		#tblMisMatch <- rbind(tblMisMatch, tblIn[iRemoveInStrandNotValid,])
		tblInvalid 	<- rbind(tblInvalid, tblIn[iRemoveInStrandNotValid,])
		tblIn 		<- tblIn[-iRemoveInStrandNotValid,]
		objREPORT 	<- REPORT.setval(objREPORT,paste(strReportPrefix,"StrandInInvalid",sep=""),length(iRemoveInStrandNotValid))
	}
	rm(iRemoveInStrandNotValid)
	#objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleInNotValid",sep=""),(length(iRemoveInAlleleNotValid)+length(iRemoveInAlleleNA)))
	#objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"StrandInNotValid",sep=""),length(iRemoveInStrandNotValid))
	#if(dim(tblIn)[1]==0) return(ADJUSTALLELES.return(objGWA, objREPORT, objAA, tblIn, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand))
	if(dim(tblIn)[1]==0) return(ADJUSTALLELES.return(objGWA, objREPORT, objAA, tblIn, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand, tblRefInvalid))

	######################
	### Reference columns
	## Remove NA In Alleles
	iRemoveRefAlleleNA = which(is.na(tblIn[[idxRefA1]]) | is.na(tblIn[[idxRefA2]]))
	if(length(iRemoveRefAlleleNA)>0) {
		### This is a mismatch
		### Exclude from output
		#tblMisMatch <- rbind(tblMisMatch, tblIn[iRemoveRefAlleleNA,])
		#tblInvalid <- rbind(tblInvalid, tblIn[iRemoveRefAlleleNA,])
		tblRefInvalid <- rbind(tblRefInvalid, tblIn[iRemoveRefAlleleNA,])
		tblIn <- tblIn[-iRemoveRefAlleleNA,]
		objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleRefMissing",sep=""),length(iRemoveRefAlleleNA))
	}
	rm(iRemoveRefAlleleNA)
	## Remove Alleles that dont match acgt
	iRemoveRefAlleleNotValid = which(!(tblIn[[idxRefA1]] %in% c("A","C","G","T","I","D")) | !(tblIn[[idxRefA2]] %in% c("A","C","G","T","I","D")))
	if(length(iRemoveRefAlleleNotValid)>0) {
		### This is a mismatch
		### Exclude from output
		#tblMisMatch <- rbind(tblMisMatch, tblIn[iRemoveRefAlleleNotValid,])
		#tblInvalid <- rbind(tblInvalid, tblIn[iRemoveRefAlleleNotValid,])
		tblRefInvalid <- rbind(tblRefInvalid, tblIn[iRemoveRefAlleleNotValid,])
		tblIn <- tblIn[-iRemoveRefAlleleNotValid,]
		objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleRefInvalid",sep=""),length(iRemoveRefAlleleNotValid))
	}
	rm(iRemoveRefAlleleNotValid)
	## Recode missing strand to +
	iRecodeRefStrandMiss = which(is.na(tblIn[[idxRefStrand]]))
	if(length(iRecodeRefStrandMiss)>0) {
		#tblIn[iRecodeRefStrandMiss,idxRefStrand] <- "+"
		tblIn[[idxRefStrand]][iRecodeRefStrandMiss] <- "+"
	}
	## Remove Strand that dont match +,-
	iRemoveRefStrandNotValid = which(!(tblIn[[idxRefStrand]] %in% c("+","-")))
	if(length(iRemoveRefStrandNotValid)>0) {
		### This is a mismatch
		### Exclude from output
		#tblMisMatch <- rbind(tblMisMatch, tblIn[iRemoveRefStrandNotValid,])
		#tblInvalid <- rbind(tblInvalid, tblIn[iRemoveRefStrandNotValid,])
		tblRefInvalid <- rbind(tblRefInvalid, tblIn[iRemoveRefStrandNotValid,])
		tblIn <- tblIn[-iRemoveRefStrandNotValid,]
		objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"StrandRefInvalid",sep=""),length(iRemoveRefStrandNotValid))
	}
	rm(iRemoveRefStrandNotValid)
	
	objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"Checked",sep=""),dim(tblIn)[1])
	
	# if(dim(tblIn)[1]==0) return(ADJUSTALLELES.return(tblIn, tblMisMatch, objGWA, objREPORT, tblInvalid))
	#if(dim(tblIn)[1]==0) return(ADJUSTALLELES.return(objGWA, objREPORT, objAA, tblIn, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand))
	if(dim(tblIn)[1]==0) return(ADJUSTALLELES.return(objGWA, objREPORT, objAA, tblIn, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand, tblRefInvalid))
	
	#################################################################################
	#### START ADJUSTING
	##########################
	#### 1. Adjust Strand
	iStrandChange = which(tblIn[[idxRefStrand]] != tblIn[[idxInStrand]])
	if(length(iStrandChange)>0) {
		#### Recode strand
		tblIn[iStrandChange, ] <- ADJUSTALLELES.FlipStrand(tblIn[iStrandChange, ], idxInA1, idxInA2, idxInStrand)
	}
	StrandChange = length(iStrandChange)
	rm(iStrandChange)
	objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"StrandChange",sep=""),StrandChange)
	#print(paste("StrandChange =",StrandChange))
	
	##########################
	#### 2. Get those rows that are already correct! (Same alleles, same direction)
	iAlleleMatch = which((tblIn[[idxInA1]] == tblIn[[idxRefA1]]) & (tblIn[[idxInA2]] == tblIn[[idxRefA2]]))
	if(length(iAlleleMatch)>0)  {
		### Transfer to output
		### exclude it for further checks
		tblOut <- rbind(tblOut, tblIn[iAlleleMatch,])
		tblIn <- tblIn[-iAlleleMatch,]
	}
	AlleleMatch = length(iAlleleMatch)
	rm(iAlleleMatch)
	objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleMatch",sep=""),AlleleMatch)
	#print(paste("AlleleMatch =",AlleleMatch))
	##########################
	#### 3. Get those rows that are already correct but with dir switched! (Same alleles, different direction)
	iAlleleChange = which((tblIn[[idxInA1]] == tblIn[[idxRefA2]]) & (tblIn[[idxInA2]] == tblIn[[idxRefA1]]))
	if(length(iAlleleChange)>0) {
		## switch dir
		tblIn[[idxInA1]][iAlleleChange] <- tblIn[[idxRefA1]][iAlleleChange]
		tblIn[[idxInA2]][iAlleleChange] <- tblIn[[idxRefA2]][iAlleleChange]
		if(isAVInFreq) tblIn[[idxInFreq]][iAlleleChange] = 1 - tblIn[[idxInFreq]][iAlleleChange]
		if(isAVInFreqs) {
			for(idxFreqTmp in aidxInFreq) 
				tblIn[[idxFreqTmp]][iAlleleChange] = 1 - tblIn[[idxFreqTmp]][iAlleleChange]
		}	
		if(isAVInBeta) tblIn[[idxInBeta]][iAlleleChange] = - tblIn[[idxInBeta]][iAlleleChange]
		if(isAVInBetas) {
			for(idxBetaTmp in aidxInBeta) 
				tblIn[[idxBetaTmp]][iAlleleChange] = - tblIn[[idxBetaTmp]][iAlleleChange]
		}
		if(isAVInOrs) {
			for(idxOrTmp in aidxInOr) 
				tblIn[[idxOrTmp]][iAlleleChange] = 1/tblIn[[idxOrTmp]][iAlleleChange]
		}
	
		### Transfer to output
		### exclude it for further checks
		tblOut <- rbind(tblOut, tblIn[iAlleleChange,])
		tblIn <- tblIn[-iAlleleChange,]
	}
	AlleleChange = length(iAlleleChange)
	rm(iAlleleChange)
	objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleChange",sep=""),AlleleChange)
	#print(paste("AlleleChange =",AlleleChange))
	
	##########################
	#### 4. Handle mismatches
	
	if(dim(tblIn)[1] > 0) {
		
		if(objAA@blnMetalUseStrand) {
			## AT TA +/- is already been handled above according to STRAND!
			## Here one just needs to solve AC / TG
			## 1. Recode strand + recheck WITHOUT RECODING STRAND COLUMN!
			
			#tblIn[,idxInStrand] = ifelse(tblIn[,idxInStrand] == "+" ,"-", "+")

			tblIn <- ADJUSTALLELES.FlipStrand(tblIn, idxInA1, idxInA2) ## No Strand Idx!
			
			i4AlleleMatch = which((tblIn[[idxInA1]] == tblIn[[idxRefA1]]) & (tblIn[[idxInA2]] == tblIn[[idxRefA2]]))
			if(length(i4AlleleMatch)>0)  {
				### Transfer to output (RECODE to ORIGIN!)
				### exclude it for further checks
				tblIn[i4AlleleMatch, ] <- ADJUSTALLELES.FlipStrand(tblIn[i4AlleleMatch, ], idxInA1, idxInA2)
				tblOut <- rbind(tblOut, tblIn[i4AlleleMatch, ])
				tblIn <- tblIn[-i4AlleleMatch,]
			}
			n4AlleleMatch = length(i4AlleleMatch)
			rm(i4AlleleMatch)
			objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"n4AlleleMatch",sep=""),n4AlleleMatch)
			#print(paste("n4AlleleMatch =",n4AlleleMatch))
			
			##########################
			#### 3. Get those rows that are already correct but with dir switched! (Same alleles, different direction)
			i4AlleleChange = which((tblIn[[idxInA1]] == tblIn[[idxRefA2]]) & (tblIn[[idxInA2]] == tblIn[[idxRefA1]]))
			if(length(i4AlleleChange)>0) {
				## switch dir
				tblIn[[idxInA1]][i4AlleleChange] <- tblIn[[idxRefA1]][i4AlleleChange]
				tblIn[[idxInA2]][i4AlleleChange] <- tblIn[[idxRefA2]][i4AlleleChange]
				
				if(isAVInFreq) tblIn[[idxInFreq]][i4AlleleChange] = 1 - tblIn[[idxInFreq]][i4AlleleChange]
				if(isAVInFreqs) {
					for(idxFreqTmp in aidxInFreq) 
						tblIn[[idxFreqTmp]][i4AlleleChange] = 1 - tblIn[[idxFreqTmp]][i4AlleleChange]
				}	
				if(isAVInBeta) tblIn[[idxInBeta]][i4AlleleChange] = - tblIn[[idxInBeta]][i4AlleleChange]
				if(isAVInBetas) {
					for(idxBetaTmp in aidxInBeta) 
						tblIn[[idxBetaTmp]][i4AlleleChange] = - tblIn[[idxBetaTmp]][i4AlleleChange]
				}
				if(isAVInOrs) {
					for(idxOrTmp in aidxInOr) 
						tblIn[[idxOrTmp]][i4AlleleChange] = 1/tblIn[[idxOrTmp]][i4AlleleChange]
				}
				### Transfer to output
				### exclude it for further checks
				tblIn[i4AlleleChange, ] <- ADJUSTALLELES.FlipStrand(tblIn[i4AlleleChange, ], idxInA1, idxInA2)
				tblOut <- rbind(tblOut, tblIn[i4AlleleChange,])
				tblIn <- tblIn[-i4AlleleChange,]
			}
			n4AlleleChange = length(i4AlleleChange)
			rm(i4AlleleChange)
			objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"n4AlleleChange",sep=""),n4AlleleChange)
			#print(paste("n4AlleleChange =",n4AlleleChange))
			#### The rest is a mismatch! Recode strand to origin.
			tblIn <- ADJUSTALLELES.FlipStrand(tblIn, idxInA1, idxInA2)
			tblMisMatch <- rbind(tblMisMatch, tblIn)
			
		} else {
			#### Handle all as mismatches
			tblMisMatch <- rbind(tblMisMatch, tblIn)
		}
	}

	# AlleleMismatch = dim(tblIn)[1]
	# objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleMismatch",sep=""),AlleleMismatch)
	
	# iRemoveInAlleleNA iRemoveInAlleleNotValid iRemoveInStrandNotValid
	# iRemoveRefAlleleNA iRemoveRefAlleleNotValid iRemoveRefStrandNotValid
	
	AlleleMismatch = dim(tblMisMatch)[1]
	objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleMismatch",sep=""),AlleleMismatch)
	# Invalid = dim(tblInvalid)[1]
	# objREPORT <- REPORT.setval(objREPORT,paste(strReportPrefix,"AlleleOrStrandInvalid",sep=""),Invalid)
	
	#return(ADJUSTALLELES.return(objGWA, objREPORT, objAA, tblOut, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand))
	return(ADJUSTALLELES.return(objGWA, objREPORT, objAA, tblOut, tblMisMatch, tblInvalid, isValidScript, isAVRefStrand, isAVInStrand, tblRefInvalid))
	
	# ##### Create output object
	# objGWA@tblGWA 		<- data.frame()
	# objGWA.adj 			<- objGWA
	# objGWA.adj@tblGWA 	<- tblOut
	# objGWA.adj@aHeader 	<- names(tblOut)

	# for(i in 1:ncol(tblOut)) objGWA.adj@aClasses[i] <- class(tblOut[,i])

	# ##### Write mismatch table
	# #if(numMismatch>0 & !blnSupressOutput) {
	# objGWA.miss 			<- objGWA
	# objGWA.miss@tblGWA	 	<- tblMisMatch
	# objGWA.invalid 			<- objGWA
	# objGWA.invalid@tblGWA 	<- tblInvalid
	
	
	#	GWADATA.write(objGWA.miss, strSuffix = ".mismatch")
	#}

	#return(list(objGWA.adj,objGWA.miss,objREPORT,objGWA.invalid))
	
	
	
}
#############################################################################################################################
ADJUSTALLELES <- function(strEqcCommand){ 
	## Wrapper for class definition
	ADJUSTALLELESout <- setADJUSTALLELES(new("ADJUSTALLELES", strEqcCommand = strEqcCommand))
	ADJUSTALLELES.valid(ADJUSTALLELESout)
	return(ADJUSTALLELESout)

}
