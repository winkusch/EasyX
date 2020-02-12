setClass("GETVAR",
	representation = representation(
						strEqcCommand			=	"character",
						colInMarker				=	"character",
						strMarker				=	"character",
						blnTry2Combine			= 	"logical",
						strAlleleRef			=	"character",
						colInA1					=	"character",
						colInA2					=	"character",
						acolInFreq				=	"character",
						acolInBeta				=	"character",
						acolInOr				=	"character",
						blnUseA1only			=	"logical"
						),
	prototype = prototype(
						strEqcCommand			=	"",
						colInMarker				=	"",
						strMarker				=	"",
						blnTry2Combine			= 	TRUE,
						strAlleleRef			=	"",
						colInA1					=	"",
						colInA2					=	"",
						acolInFreq				=	"",
						acolInBeta				=	"",
						acolInOr				=	"",
						blnUseA1only			=	FALSE
						)
	#contains = c("EcfReader")
)

setGeneric("setGETVAR", function(object) standardGeneric("setGETVAR"))
setMethod("setGETVAR", signature = (object = "GETVAR"), function(object) {
	
	aEqcSlotNamesIn = c("colInMarker", "strMarker", "blnTry2Combine","strAlleleRef","colInA1","colInA2","acolInFreq","acolInBeta","acolInOr","blnUseA1only")

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
validGETVAR <- function(objGETVAR) {
	
	if(objGETVAR@colInMarker == "") 
		stop(paste(" EASY ERROR:GETVAR\n No colInMarker defined. \n Please set colInMarker or remove GETVAR function.", sep=""))
	
	if(objGETVAR@strMarker == "") 
		stop(paste(" EASY ERROR:GETVAR\n No strMarker defined. \n Please set strMarker or remove GETVAR function.", sep=""))
	
}


GETVAR.GWADATA.valid <- function(objGETVAR, objGWA) {
	
	if(!(objGETVAR@colInA1 %in% objGWA@aHeader) & (objGETVAR@colInA1 != ""))
		stop(paste(" EASY ERROR:GETVAR\n Defined column colInA1 \n",objGETVAR@colInA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objGETVAR@colInA2 %in% objGWA@aHeader) & (objGETVAR@colInA2 != ""))
		stop(paste(" EASY ERROR:GETVAR\n Defined column colInA2 \n",objGETVAR@colInA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	if((!is.na(objGETVAR@colInA1))&is.na(objGETVAR@colInA2)) stop(paste(" EASY ERROR:GETVAR\n Column colInA1 \n",objGETVAR@colInA1, "\n is defined but colInA2 is missing. Please add --colInA2 or set --blnUseA1only 1", sep=""))
	if((!is.na(objGETVAR@colInA2))&is.na(objGETVAR@colInA1)) stop(paste(" EASY ERROR:GETVAR\n Column colInA2 \n",objGETVAR@colInA2, "\n is defined but colInA1 is missing. Please add --colInA1 and optionally set --blnUseA1only 1", sep=""))
	
	if((!is.na(objGETVAR@strAlleleRef)) & is.na(objGETVAR@colInA1)) stop(paste(" EASY ERROR:GETVAR\n Column --colInA1 is missing. Please add --colInA1 and --colInA2 or add --colInA1 and set --blnUseA1only 1", sep=""))
		
	if(any(!(objGETVAR@acolInFreq %in% objGWA@aHeader)) & (objGETVAR@acolInFreq[1] != ""))
		stop(paste(" EASY ERROR:GETVAR\n Defined acolInFreq columns \n",paste(objGETVAR@acolInFreq[which(!(objGETVAR@acolInFreq %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(any(!(objGETVAR@acolInBeta %in% objGWA@aHeader)) & (objGETVAR@acolInBeta[1] != ""))
		stop(paste(" EASY ERROR:GETVAR\n Defined acolInBeta columns \n",paste(objGETVAR@acolInBeta[which(!(objGETVAR@acolInBeta %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(any(!(objGETVAR@acolInOr %in% objGWA@aHeader)) & (objGETVAR@acolInOr[1] != ""))
		stop(paste(" EASY ERROR:GETVAR\n Defined acolInOr columns \n",paste(objGETVAR@acolInOr[which(!(objGETVAR@acolInOr %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
		
	return(TRUE)
}

#############################################################################################################################
GETVAR.run <- function(objGV, objGWA, objREPORT) {
	
	colInMarker <- objGV@colInMarker
	strMarker 	<- objGV@strMarker
	
	strAlleleRef <- objGV@strAlleleRef
	colInA1	<- objGV@colInA1
	colInA2 <- objGV@colInA2
	acolInFreq <- objGV@acolInFreq
	acolInBeta <- objGV@acolInBeta
	acolInOr <- objGV@acolInOr
	blnUseA1only <- objGV@blnUseA1only
	
	blnAdjustAlleles2Ref <- strAlleleRef != ""
	
	objRCD 	<- RCD(paste(objGV@colInMarker,"==",objGV@strMarker,sep=""))
	out 	<- RCD.eval(objRCD, objGWA)
	numVarFound = length(which(out))
	
	objGWA.var <- GWADATA.getrows(objGWA, which(out))
	
	if(nrow(objGWA.var@tblGWA)>1) {
		stop(paste(" EASY ERROR:GETVAR\n Duplicates found in \n",objGWA@fileIn,"\n PLease clean up.", sep=""))
	}
	
	if(nrow(objGWA.var@tblGWA)==1) {
		if(blnAdjustAlleles2Ref) {
		
				if(toupper(strAlleleRef) == toupper(objGWA.var@tblGWA[[colInA1]])) {
					## all ok
					objGWA.var = GWADATA.cbind(objGWA.var, TRUE, "GETVAR.alleleadjusted")			
					
					
				} else if(blnUseA1only | toupper(strAlleleRef) == toupper(objGWA.var@tblGWA[[colInA2]])) {
					## switch allele
					a1 = objGWA.var@tblGWA[[colInA1]]
					a2 = objGWA.var@tblGWA[[colInA2]]
					objGWA.var@tblGWA[[colInA2]] = a1
					objGWA.var@tblGWA[[colInA1]] = a2
					if(acolInFreq[1]!="") {
						for(colInFreq in acolInFreq) objGWA.var@tblGWA[[colInFreq]] <- 1-as.numeric(objGWA.var@tblGWA[[colInFreq]])
					}
					if(acolInBeta[1]!="") {
						for(colInBeta in acolInBeta) objGWA.var@tblGWA[[colInBeta]] <- -as.numeric(objGWA.var@tblGWA[[colInBeta]])
					}
					if(acolInOr[1]!="") {
						for(colInOr in acolInOr) objGWA.var@tblGWA[[colInOr]] <- 1/as.numeric(objGWA.var@tblGWA[[colInOr]])
					}
					objGWA.var = GWADATA.cbind(objGWA.var, TRUE, "GETVAR.alleleadjusted")
					
				} else {
					objGWA.var = GWADATA.cbind(objGWA.var, FALSE, "GETVAR.alleleadjusted")
				}
		} 
	}
		
	#if(!blnSupressOutput) GWADATA.write(objGWA.iCrit, strSuffix = ".GETVAR")
	
	strMarker = gsub("\"","",strMarker)
	
	objREPORT <- REPORT.addval(objREPORT,paste("GETVAR.",strMarker,sep=""),numVarFound)

	return(list(objGWA.var,objREPORT))
}

GETVAR <- function(strEqcCommand){ 
	## Wrapper for class definition
	GETVARout <- setGETVAR(new("GETVAR", strEqcCommand = strEqcCommand))
	validGETVAR(GETVARout)
	#GETVARout.valid <- validGETVAR(GETVARout)
	return(GETVARout)
}

