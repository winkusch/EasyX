setClass("MERGE",
	representation = representation(
						strEqcCommand				=	"character",
						colInMarker					=	"character",
						strInSuffix					=	"character",
						blnInAll					=	"logical",
						fileRef						=	"character",
						colRefMarker				=	"character",
						strRefSuffix				=	"character",
						blnRefAll					=	"logical",
						fileRefTag					=	"character",
						blnWriteNotInRef			=	"logical",
						blnWriteNotInIn				=	"logical",
						strTag						=	"character",
						blnCreateCpaid				=	"logical",
						colRefChr					=	"character",
						colRefPos					=	"character",
						colRefA1					=	"character",
						colRefA2					=	"character",
						blnSort2Ref					=	"logical"
						),
	prototype = prototype(
						strEqcCommand				=	"",
						colInMarker					=	"",
						strInSuffix					=	"",
						blnInAll					=	TRUE,
						fileRef						=	"",
						colRefMarker				=	"",
						strRefSuffix				=	"",
						blnRefAll					=	FALSE,
						fileRefTag					=	"1",
						blnWriteNotInRef			=	FALSE,
						blnWriteNotInIn				=	FALSE,
						strTag						=	"",
						blnCreateCpaid				=	FALSE,
						colRefChr					=	"",
						colRefPos					=	"",
						colRefA1					=	"",
						colRefA2					=	"",
						blnSort2Ref					=	FALSE
						),
	contains = c("GWADATA")
)

MERGE.set <- function(strEqcCommand, objMERGE, objGWA.default) {

	objMERGE@strMissing <- objGWA.default@strMissing
	objMERGE@strSeparator <- objGWA.default@strSeparator
	objMERGE@blnUseFastRead <- objGWA.default@blnUseFastRead
	
	aEqcSlotNamesIn = c("colInMarker", "strInSuffix", "blnInAll", "fileRef", "colRefMarker", "strRefSuffix", "blnRefAll","fileRefTag","blnWriteNotInRef","blnWriteNotInIn","strTag",
						"strMissing", "strSeparator", "acolIn", "acolInClasses","acolNewName","blnUseFastRead","numRowSkip", "blnCreateCpaid", "colRefChr", "colRefPos", "colRefA1", "colRefA2", "blnSort2Ref") 
	
	### Last 4 are inherited from class GWADATA and can be used with MERGE for reference file!
						
	objEqcReader <- EqcReader(strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(objMERGE, tmpSlot) <- tmpSlotVal
		}
	}
	
	#object@pathOut <- getwd()
	objMERGE@pathOut <- "NA"
	
	return(objMERGE)
}

#############################################################################################################################
MERGE.init <- function(objMERGE) {
	
	if(objMERGE@fileRef == "")
		stop(paste(" EASY ERROR:MERGE\n No reference file fileRef defined. \n Please set fileRef that will be merged to alll GWA data-sets.", sep=""))
	
	if(!file.exists(objMERGE@fileRef))
		stop(paste("EASY ERROR:MERGE\n Reference file \n ",objMERGE@fileRef,"\n that should be merged to input, does not exist!!!\n", sep=""))
	
	if(objMERGE@blnCreateCpaid) {
		if(objMERGE@colRefChr == "")
			stop(paste(" EASY ERROR:MERGE\n No reference Chr column defined for fileRef. \n Please define colRefChr that will be used for merging the data-set.", sep=""))
		if(objMERGE@colRefPos == "")
			stop(paste(" EASY ERROR:MERGE\n No reference Pos column defined for fileRef. \n Please define colRefPos that will be used for merging the data-set.", sep=""))
		if(objMERGE@colRefA1 == "")
			stop(paste(" EASY ERROR:MERGE\n No reference A1 column defined for fileRef. \n Please define colRefA1 that will be used for merging the data-set.", sep=""))
		if(objMERGE@colRefA2 == "")
			stop(paste(" EASY ERROR:MERGE\n No reference A2 column defined for fileRef. \n Please define colRefA2 that will be used for merging the data-set.", sep=""))
	} else {
		if(objMERGE@colRefMarker == "")
			stop(paste(" EASY ERROR:MERGE\n No reference Marker column defined for fileRef. \n Please define colRefMarker that will be used for merging the data-set.", sep=""))
	}
	
	if(objMERGE@colInMarker == "")
		stop(paste(" EASY ERROR:MERGE\n No input Marker column defined. \n Please define colInMarker that will be used for merging the data-set.", sep=""))
	
	objMERGE@fileIn <- objMERGE@fileRef
	
	objMERGE@pathOut = "NA"
	
	# if(objMERGE@strTag == "") 
		# stop(paste(" EASY ERROR:MERGE\n Requested parameter 'strTag' missing. \n Please ensure that unique 'strTag' values are set for all your MERGE,ADJUSTALLELES and AFCHECK commands.", sep=""))
	
	objMERGE.init <- GWADATA.init(objMERGE)
	
	if(objMERGE@blnCreateCpaid) {
		if(!(objMERGE.init@colRefChr%in%objMERGE.init@aHeader) & !(objMERGE.init@colRefChr%in%objMERGE.init@acolNewName)) 
			stop(paste(" EASY ERROR:MERGE\n Reference Chr column colRefChr \n",objMERGE@colRefChr," \n cannot be found in fileRef \n", objMERGE@fileRef," \n Please correct colRefChr.", sep=""))
		if(!(objMERGE.init@colRefPos%in%objMERGE.init@aHeader) & !(objMERGE.init@colRefPos%in%objMERGE.init@acolNewName)) 
			stop(paste(" EASY ERROR:MERGE\n Reference Pos column colRefPos \n",objMERGE@colRefPos," \n cannot be found in fileRef \n", objMERGE@fileRef," \n Please correct colRefPos.", sep=""))
		if(!(objMERGE.init@colRefA1%in%objMERGE.init@aHeader) & !(objMERGE.init@colRefA1%in%objMERGE.init@acolNewName)) 
					stop(paste(" EASY ERROR:MERGE\n Reference A1 column colRefA1 \n",objMERGE@colRefA1," \n cannot be found in fileRef \n", objMERGE@fileRef," \n Please correct colRefA1.", sep=""))
		if(!(objMERGE.init@colRefA2%in%objMERGE.init@aHeader) & !(objMERGE.init@colRefA2%in%objMERGE.init@acolNewName)) 
					stop(paste(" EASY ERROR:MERGE\n Reference A2 column colRefA2 \n",objMERGE@colRefA2," \n cannot be found in fileRef \n", objMERGE@fileRef," \n Please correct colRefA2.", sep=""))			
	} else {
		if(!(objMERGE.init@colRefMarker%in%objMERGE.init@aHeader) & !(objMERGE.init@colRefMarker%in%objMERGE.init@acolNewName)) 
			stop(paste(" EASY ERROR:MERGE\n Reference Marker column colRefMarker \n",objMERGE@colRefMarker," \n cannot be found in fileRef \n", objMERGE@fileRef," \n Please correct colRefMarker.", sep=""))
	}
	
	if(objMERGE@blnSort2Ref) {
		objMERGE@blnInAll <- FALSE
		objMERGE@blnRefAll <- TRUE
	}
	
	return(objMERGE.init)
}

MERGE.GWADATA.valid <- function(objMERGE, objGWA) {
	
	isAv <- objMERGE@colInMarker %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:MERGE\n Defined column colInMarker \n",objMERGE@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))

	### Check duplicate colnames in merged data set

						
	aHeadIn <- objGWA@aHeader
	iMarkerIn = match(objMERGE@colInMarker, aHeadIn)
	aHeadIn[-iMarkerIn] <- paste(aHeadIn[-iMarkerIn],objMERGE@strInSuffix,sep="")
	
	if(!objMERGE@blnCreateCpaid) {
		aHeadRef <- objMERGE@aHeader
		iMarkerRef = match(objMERGE@colRefMarker, aHeadRef)
		aHeadRef[-iMarkerRef] <- paste(aHeadRef[-iMarkerRef],objMERGE@strRefSuffix,sep="")
		
		aHeadIntersect = intersect(aHeadIn[-iMarkerIn],aHeadRef[-iMarkerRef])
		if(length(aHeadIntersect)>0)
			stop(paste(" EASY ERROR:MERGE\n Column \n",paste(aHeadIntersect,collapse="\n"), 
						"\n will be duplicated after merging data to \n",objGWA@fileIn,
						"\n PLease specify correct suffixes strInSuffix and strRefSuffix or rename column in one file.", sep="")
				)
	}
}

#############################################################################################################################
MERGE.run <- function(objMERGE, objGWA, objREPORT, isValidScript) {
	
	blnSort2Ref <- objMERGE@blnSort2Ref
	
	if(objMERGE@blnCreateCpaid) {
		
		cpaid <- rep(NA,nrow(objMERGE@tblGWA))
	
		a1ref <- as.character(objMERGE@tblGWA[[objMERGE@colRefA1]])
		a2ref <- as.character(objMERGE@tblGWA[[objMERGE@colRefA2]])
		chrref <- as.character(objMERGE@tblGWA[[objMERGE@colRefChr]])
		posref <- as.character(as.integer(objMERGE@tblGWA[[objMERGE@colRefPos]]))
		
		a1ref[is.na(a1ref)] <- "NA"
		a2ref[is.na(a2ref)] <- "NA"
		isA1First = a1ref < a2ref
		
		cpaid <- ifelse(isA1First, paste(chrref,":",posref,":",a1ref,"_",a2ref,sep=""), paste(chrref,":",posref,":",a2ref,"_",a1ref,sep=""))
		
		objMERGE <- GWADATA.cbind(objMERGE, cpaid, "cpaid")
		
		objMERGE@colRefMarker <- "cpaid"
		
		rm(a1ref)
		rm(a2ref)
		rm(chrref)
		rm(posref)
		rm(isA1First)
		
	}
	
	colInMarker 	<- objMERGE@colInMarker
	colRefMarker 	<- objMERGE@colRefMarker
	
	strPrefix <- ifelse(objMERGE@strTag!="", paste(objMERGE@strTag,".",sep=""), "")
	
	objGWA.NotInRef <- GWADATA.copy(objGWA)
	objMERGE.NotInIn <- GWADATA.copy(objMERGE)
	objMERGE.NotInIn@pathOut <- objGWA@pathOut
	objMERGE.NotInIn@fileInShortName <- rev(strsplit(objMERGE.NotInIn@fileRef,"/",fixed=T)[[1]])[1]
			
	#aMarkerIn <- GWADATA.getcol(objGWA,objMERGE@colInMarker)
	#aMarkerRef <- GWADATA.getcol(objMERGE,objMERGE@colRefMarker)
	
	isNotInRef 	<- !objGWA@tblGWA[[colInMarker]]%in%objMERGE@tblGWA[[colRefMarker]]
	isNotInIn 	<- !objMERGE@tblGWA[[colRefMarker]]%in%objGWA@tblGWA[[colInMarker]]
	
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"NotInRef",sep=""),length(which(isNotInRef)))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"NotInIn",sep=""),length(which(isNotInIn)))
	
	if(isValidScript) {
		if(objMERGE@blnWriteNotInRef & any(isNotInRef)) {
			objGWA.NotInRef <- GWADATA.getrows(objGWA,which(isNotInRef))
			GWADATA.write(objGWA.NotInRef, strSuffix = paste(".",strPrefix,"notinref",sep=""))
			rm(objGWA.NotInRef)
		}
		if(objMERGE@blnWriteNotInIn & any(isNotInIn)) {
			objMERGE.NotInIn <- GWADATA.getrows(objMERGE,which(isNotInIn))		
			if(objMERGE.NotInIn@pathOut == "NA") objMERGE.NotInIn@pathOut <- objGWA@pathOut
			GWADATA.write(objMERGE.NotInIn, strSuffix = paste(".",strPrefix,"notinin",sep=""))
			rm(objMERGE.NotInIn)
		}
	}
	objGWA <- GWADATA.merge(objGWA, objMERGE, 
				strSuffix.In = objMERGE@strInSuffix, 
				strSuffix.Add = objMERGE@strRefSuffix, 
				blnAll.In = objMERGE@blnInAll, 
				blnAll.Add = objMERGE@blnRefAll, 
				strBy.In = objMERGE@colInMarker, 
				strBy.Add = objMERGE@colRefMarker) 
	
	if(blnSort2Ref) {
		
		idxResort = match(objMERGE@tblGWA[[objMERGE@colRefMarker]],objGWA@tblGWA[[objMERGE@colInMarker]])
		objGWA@tblGWA = objGWA@tblGWA[idxResort,]
		
	}
	
	
	return(list(objGWA,objREPORT))
}
#############################################################################################################################
MERGE <- function(strEqcCommand,objGWA.default){ 
	## Wrapper for class definition
	# MERGEout <- setMERGE(new("MERGE", strEqcCommand = strEqcCommand))
	
	MERGEout <- new("MERGE")
	MERGEout <- MERGE.set(strEqcCommand, MERGEout,objGWA.default)
	MERGEout <- MERGE.init(MERGEout)
	
	return(MERGEout)

}
