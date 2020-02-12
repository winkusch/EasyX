setClass("SORT",
	representation = representation(
						strEqcCommand		=	"character",
						acolSort			=	"character",
						ablnDescending		=	"logical"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						acolSort			=	"",
						ablnDescending		=	FALSE
						)
	#contains = c("EcfReader")
)

setGeneric("setSORT", function(object) standardGeneric("setSORT"))
setMethod("setSORT", signature = (object = "SORT"), function(object) {
	
	aEqcSlotNamesIn = c("acolSort", "ablnDescending")
	#aEcfSlotNamesIn = c("arcdSORT", "acolOut")

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
validSORT <- function(objSORT) {

	if(objSORT@acolSort[1] == "") 
		stop(paste(" EASY ERROR:SORT\n No column names acolSort defined. Please set acolSort.", sep=""))
	
	return(TRUE)
}

SORT.GWADATA.valid <- function(objSORT, objGWA) {
	
	aisMatch = objSORT@acolSort %in% objGWA@aHeader
	if(!all(aisMatch))
		stop(paste("EASY ERROR:SORT\n Column \n",paste(objSORT@acolSort[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		
	return(TRUE)
	
}

#############################################################################################################################
SORT.run <- function(objSORT, objGWA) {

	acolSort 		<- objSORT@acolSort
	ablnDescending 	<- objSORT@ablnDescending
	
	objGWA.sorted <- GWADATA.sort(objGWA, acolSort, ablnDescending)
	
	return(objGWA.sorted)
}

SORT <- function(strEqcCommand){ 
	## Wrapper for class definition
	SORTout <- setSORT(new("SORT", strEqcCommand = strEqcCommand))
	validSORT(SORTout)
	#SORTout.valid <- validSORT(SORTout)
	return(SORTout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

# setValidity("SORT", function(object){
	# print("SORT-CHECK")
	
	
	
	# print(TRUE)
	# return(TRUE)
# })

