setClass("MERGEEASYIN",
	representation = representation(
						strEqcCommand				=	"character",
						colInMarker					=	"character",
						fileOutShortName			=	"character",
						blnMergeAll					=	"logical"
						),
	prototype = prototype(
						strEqcCommand				=	"",
						colInMarker					=	"",
						fileOutShortName			=	"",
						blnMergeAll					=	FALSE
						)

)

setGeneric("setMERGEEASYIN", function(object) standardGeneric("setMERGEEASYIN"))
setMethod("setMERGEEASYIN", signature = (object = "MERGEEASYIN"), function(object) {
	
	#aEqcSlotNamesIn = c("colInMarker", "fileOutShortName", "blnMergeAll") 
	aEqcSlotNamesIn = c("colInMarker", "blnMergeAll") 
	
	### Last 4 are inherited from class GWADATA and can be used with MERGEEASYIN for reference file!
						

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
validMERGEEASYIN <- function(objME) {
	
	if(objME@colInMarker == "")
		stop(paste(" EASY ERROR:MERGEEASYIN\n No input Marker column defined. \n Please define colInMarker that will be used for merging the EASYIN data-sets.", sep=""))
	
	return(TRUE)
}

MERGEEASYIN.GWADATA.valid <- function(objME, objGWA) {
	
	isAv <- objME@colInMarker %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:MERGEEASYIN\n Defined column colInMarker \n",objME@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))

}
#############################################################################################################################
MERGEEASYIN.start <- function(objME, objGWADATA.default, objGWA) {

	objGWA.merged <- objGWA
	### Get aHeader, aClasses, fileInTag, fileInTrait, fileInStrat from objGWA
	
	icolMarker <- match(objME@colInMarker, objGWA@aHeader)
	
	aHeaderTmp <- objGWA.merged@aHeader
	aHeaderTmp[-icolMarker] <- paste(aHeaderTmp[-icolMarker],objGWA.merged@fileInTag ,sep=".")
	objGWA.merged@tblGWA <- setNames(objGWA.merged@tblGWA,aHeaderTmp)
	objGWA.merged@aHeader <- aHeaderTmp
	
	## Reset GWADATA values
	objGWA.merged@fileInType		<- "GWADATA"
	objGWA.merged@pathOut 			<- objGWADATA.default@pathOut
	objGWA.merged@strMissing 	<- "NA"
	objGWA.merged@strSeparator 	<- "WHITESPACE"
	objGWA.merged@acolIn 		<- ""
	objGWA.merged@acolInClasses <- ""
	objGWA.merged@aHeaderRead 	<- ""
	objGWA.merged@aClassesRead 	<- ""
	objGWA.merged@blnMergedEasyin <- TRUE
	
	objGWA.merged@fileIn 			<- objME@fileOutShortName
	
	fileInShortName <- objME@fileOutShortName
	
	if(objGWA@fileTranslator!="") {
		## replace fileInShortName by appropriate value from fileTranslator
		
		tblTrans = read.table(objGWA@fileTranslator,sep="\t",header=T,stringsAsFactors=F,colClasses="character")
		
		afsn = unlist(strsplit(fileInShortName,".",fixed=T))
		
		for(isn in 1:length(afsn)) {
			isFsnMatch = afsn[isn]==tblTrans$PhenotypeCode
			if(any(isFsnMatch)) {
				afsn[isn] = tblTrans$PhenotypeDescription[which(isFsnMatch)]
			}
		}
		fileInShortName = paste(afsn,collapse=".")
	}
	
	objGWA.merged@fileInShortName <- fileInShortName
	
	return(objGWA.merged)
}
#############################################################################################################################
MERGEEASYIN.run <- function(objME, objGWA.merged, objGWA) {

	#objGWA.merged <- objGWA
	
	icolMarker <- match(objME@colInMarker, objGWA@aHeader)
	#objGWA@aHeader[-icolMarker] <- names(objGWA@tblGWA)[-icolMarker] <- paste(objGWA@aHeader[-icolMarker],objGWA@fileInStrat ,sep=".")
	
	aHeaderTmp 	<- objGWA@aHeader
	aHeaderTmp[-icolMarker] <- paste(aHeaderTmp[-icolMarker],objGWA@fileInTag ,sep=".")
	objGWA@tblGWA  <- setNames(objGWA@tblGWA,aHeaderTmp)
	objGWA@aHeader <- aHeaderTmp
	
	objGWA.merged <- GWADATA.merge(objGWA.merged, objGWA, 
						strSuffix.In = "", 
						strSuffix.Add = "", 
						blnAll.In = objME@blnMergeAll, 
						blnAll.Add = objME@blnMergeAll, 
						strBy.In = objME@colInMarker, 
						strBy.Add = objME@colInMarker
					) 
	#objGWA.merged@blnMergedEasyin 	<- TRUE
	objGWA.merged@fileInTag 		<- paste(objGWA.merged@fileInTag,objGWA@fileInTag,sep="_")
	objGWA.merged@fileInStrat 		<- paste(objGWA.merged@fileInStrat,objGWA@fileInStrat,sep="_")
	#objGWA.merged@fileInShortName 	<- paste(objGWA.merged@fileInShortName,objGWA@fileInStrat,sep="_")
	#objGWA.merged@fileInShortName <- paste(objGWA.merged@fileInShortName,objGWA@fileInShortName,sep=".")
		
	return(objGWA.merged)
}
#############################################################################################################################
MERGEEASYIN <- function(strEqcCommand, fileOutShortName){ 
	## Wrapper for class definition
	MERGEEASYINout <- setMERGEEASYIN(new("MERGEEASYIN", strEqcCommand = strEqcCommand))
	MERGEEASYINout@fileOutShortName = paste(fileOutShortName,".merged",sep="")
	
	validMERGEEASYIN(MERGEEASYINout)
	return(MERGEEASYINout)

}
