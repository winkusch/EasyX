setClass("CREATECPAID",
	representation = representation(
						strEqcCommand		=	"character",
						colInA1				=	"character",
						colInA2				=	"character",
						colInChr			=	"character",
						colInPos			=	"character",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInA1				=	"",
						colInA2				=	"",
						colInChr			=	"",
						colInPos			=	"",
						strTag				=	""
						)
)

CREATECPAID.set <- function(strEqcCommand, objCPT) {

	aEqcSlotNamesIn = c("colInA1","colInA2","colInChr","colInPos","strTag")
	
	## astrPatterns
	## *_CHR_POS
	## chrCHR:POS
	
	### Last 4 are inherited from class GWADATA and can be used with CREATECPAID for reference file!
	
	objEqcReader <- EqcReader(strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(objCPT, tmpSlot) <- tmpSlotVal
		}
	}
	
	return(objCPT)
}

# setGeneric("setCREATECPAID", function(object) standardGeneric("setCREATECPAID"))
# setMethod("setCREATECPAID", signature = (object = "CREATECPAID"), function(object, objGWADATA.default) {
	
	# object@blnUseFastRead <- objGWADATA.default@blnUseFastRead
	
	# aEqcSlotNamesIn = c("colInMarker","colInA1","colInA2","fileMap","colMapMarker","colMapChr","colMapPos","strTag","colInChr","colInPos","blnUseInMarker","blnUseFastRead")
	
	# ## astrPatterns
	# ## *_CHR_POS
	# ## chrCHR:POS
	
	# ### Last 4 are inherited from class GWADATA and can be used with CREATECPAID for reference file!
	
	# objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	# if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		# for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			# tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			# tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			# if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		# }
	# }
	
	# return(object)
# })

#############################################################################################################################

CREATECPAID.GWADATA.valid <- function(objCPT, objGWA) {
	
	isAv <- objCPT@colInA1 %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPAID\n Defined column colInA1 \n",objCPT@colInA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isAv <- objCPT@colInA2 %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPAID\n Defined column colInA2 \n",objCPT@colInA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isAv <- objCPT@colInChr %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPAID\n Defined column colInChr \n",objCPT@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct --colInChr OR remove --colInchr and --colInPos.", sep=""))

	isAv <- objCPT@colInPos %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPAID\n Defined column colInPos \n",objCPT@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct --colInPos OR remove --colInchr and --colInPos.", sep=""))

}

#############################################################################################################################
CREATECPAID.run <- function(objCPT, objGWA, isValidScript) {
	
	colInChr 	<- objCPT@colInChr
	colInPos 	<- objCPT@colInPos
	colInA1 	<- objCPT@colInA1
	colInA2 	<- objCPT@colInA2
	strTag 		<- objCPT@strTag
	
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
		
	cpaid <- rep(NA,nrow(objGWA@tblGWA))
	
	a1 <- as.character(objGWA@tblGWA[[colInA1]])
	a2 <- as.character(objGWA@tblGWA[[colInA2]])
	chr <- as.character(objGWA@tblGWA[[colInChr]])
	pos <- as.character(as.integer(objGWA@tblGWA[[colInPos]]))
	
	a1[is.na(a1)] <- "NA"
	a2[is.na(a2)] <- "NA"
	isA1First = a1 < a2
	cpaid <- ifelse(isA1First, paste(chr,":",pos,":",a1,"_",a2,sep=""), paste(chr,":",pos,":",a2,"_",a1,sep=""))
	
	objGWA <- GWADATA.cbind(objGWA, cpaid, "cpaid")
	
	return(objGWA)
}
#############################################################################################################################
CREATECPAID <- function(strEqcCommand){ 
	## Wrapper for class definition
	CREATECPAIDout <- new("CREATECPAID")
	CREATECPAIDout <- CREATECPAID.set(strEqcCommand, CREATECPAIDout)
	
	return(CREATECPAIDout)

}
