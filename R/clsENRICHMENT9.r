setClass("ENRICHMENT",
	representation = representation(
						strEqcCommand		=	"character",
						rcdCriterion		=	"character",
						rcdBinomCount		=	"character",
						strBinomAlternative	=	"character",
						numBinomPval		=	"numeric",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						rcdCriterion		=	"",
						rcdBinomCount		=	"",
						strBinomAlternative	=	"",
						numBinomPval		=	0.5,
						strTag				=	""
						)
	#contains = c("EcfReader")
)
setGeneric("setENRICHMENT", function(object) standardGeneric("setENRICHMENT"))
setMethod("setENRICHMENT", signature = (object = "ENRICHMENT"), function(object) {
	
	aEqcSlotNamesIn = c("rcdCriterion",
						"rcdBinomCount",
						"strBinomAlternative",
						"numBinomPval",
						"strTag"
						)
	## strBinomCount must be
	## Beta>0	-> 
	##	strAlternative="greater": binom.test(#Beta>0,#SNPs,p=0.5,alternative="greater") "Enrichment of positive effects"
	##	strAlternative="two.sided": binom.test(#Beta>0,#SNPs,p=0.5,alternative="two.sided") "Enrichment of pos/neg effects"
	##	strAlternative="less": binom.test(#Beta>0,#SNPs,p=0.5,alternative="less") "Enrichment of negative effects"
	   ## IDENTICAL: Beta<0 -> binom.test(#Beta<0,#SNPs,p=0.5,alternative="greater")
	## P<0.05	-> binom.test(#P<0.05,#SNPs,p=0.05,alternative="greater")
	## Beta>0&P<0.05 -> binom.test(# Beta>0&P<0.05,#SNPs,p=0.05*0.5,alternative="greater")
	
						
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			# if(tmpSlot == "arcdExclude") tmpSlotVal[is.na(tmpSlotVal)]=""
			if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		}
	}
	
	return(object)
})

#############################################################################################################################
validENRICHMENT <- function(objER) {
	
	if(objER@numBinomPval<=0|objER@numBinomPval>=1)
		stop(paste("EASY ERROR:ENRICHMENT\n Wrong numBinomPval defined, Please set numBinomPval in (0,1) \n !!!", sep=""))	
		
	if(!(objER@strBinomAlternative%in%c("two.sided","less","greater")))
		stop(paste("EASY ERROR:ENRICHMENT\n Wrong strBinomAlternative defined, Please use 'two.sided' (default),'less' or 'greater' \n !!!", sep=""))	

	if(objER@rcdBinomCount=="")
		stop(paste("EASY ERROR:ENRICHMENT\n rcdBinomCount undefined, Please set --rcdBinomCount \n !!!", sep=""))	
	
	return(TRUE)
}

ENRICHMENT.GWADATA.valid <- function(objER, objGWA) {
	
	# if(grepl("Beta",objER@strMode)) {
		# if(objER@colBeta == "") {
			# stop(paste("EASY ERROR:ENRICHMENT\n Column colBeta undefined. PLease set --colBeta \n !!!" ,sep=""))		
		# }
		# isMatch = objER@colBeta %in% objGWA@aHeader
		# if(!isMatch) {
			# stop(paste("EASY ERROR:ENRICHMENT\n Column \n",objER@colBeta,"\n does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		# }
	# }
	
	# if(grepl("Pval",objER@strMode)) {
		# if(objER@colPval == "") {
			# stop(paste("EASY ERROR:ENRICHMENT\n Column colPval undefined. PLease set --colPval \n !!!" ,sep=""))		
		# }
		# isMatch = objER@colPval %in% objGWA@aHeader
		# if(!isMatch) {
			# stop(paste("EASY ERROR:ENRICHMENT\n Column \n",objER@colPval,"\n does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		# }
	# }
	
	return(TRUE)
	
}

ENRICHMENT.run <- function(objER, objGWA, objREPORT) {
						
	rcdCriterion 		<- objER@rcdCriterion
	rcdBinomCount		<- objER@rcdBinomCount
	strBinomAlternative <- objER@strBinomAlternative
	numBinomPval		<- objER@numBinomPval
	strTag				<- objER@strTag
		
	if(rcdCriterion != "") {
		objRCD 	<- RCD(rcdCriterion)
		out 	<- RCD.eval(objRCD, objGWA)
		numCrit = length(which(out))
		objGWA.crit <- GWADATA.getrows(objGWA, which(out))
	} else {
		numCrit <- nrow(objGWA@tblGWA)
		objGWA.crit <- objGWA
	}
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	
	if(numCrit > 0) {
	
		objRcdCount <- RCD(rcdBinomCount)
		ablnFullfilled	<- RCD.eval(objRcdCount, objGWA.crit)
		numTrials = length(which(!is.na(ablnFullfilled)))
		numSuccess = length(which(ablnFullfilled))
		pBinOut = binom.test(numSuccess,numTrials,p=numBinomPval,alternative=strBinomAlternative)$p.value
		
	} else {
		numTrials <- numSuccess <- pBinOut <- NA
	}
	
	BinomParams_P_Alt = paste(numBinomPval, strBinomAlternative,sep="_")
	tblEnrichmentReport <- data.frame(rcdCriterion, numCrit, numTrials, rcdBinomCount, numSuccess, BinomParams_P_Alt, pBinOut, stringsAsFactors=FALSE)
	
	return(tblEnrichmentReport)
	
}

ENRICHMENT <- function(strEqcCommand){ 
	## Wrapper for class definition
	ENRICHMENTout <- setENRICHMENT(new("ENRICHMENT", strEqcCommand = strEqcCommand))
	validENRICHMENT(ENRICHMENTout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(ENRICHMENTout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

