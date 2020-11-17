setClass("RENAMEMARKER",
	representation = representation(
						strEqcCommand			=	"character",
						colInMarker				=	"character",
						fileRename				=	"character",
						strTag					=	"character",
						colRenameOldMarker		=	"character",
						colRenameNewMarker		=	"character",
						blnWriteRenamed			=	"logical",
						blnWriteNomatches		=	"logical",
						blnRetainNoMatches		=	"logical",
						blnSaveOldMarker		=	"logical",
						tblRename				=	"data.frame"
						),
	prototype = prototype(
						strEqcCommand			=	"",
						colInMarker				=	"",
						fileRename				=	"",
						strTag					=	"",
						colRenameOldMarker		=	"",
						colRenameNewMarker		=	"",
						blnWriteRenamed			=	FALSE,
						blnWriteNomatches		=	FALSE,
						blnRetainNoMatches		=	TRUE,
						blnSaveOldMarker		=	TRUE,
						tblRename				=	data.frame()
						)
	#contains = c("GWADATA")
)


setGeneric("setRENAMEMARKER", function(object) standardGeneric("setRENAMEMARKER"))
setMethod("setRENAMEMARKER", signature = (object = "RENAMEMARKER"), function(object) {
	
	aEqcSlotNamesIn = c("colInMarker", "fileRename", "strTag","colRenameOldMarker", "colRenameNewMarker","blnWriteRenamed","blnWriteNomatches","blnRetainNoMatches","blnSaveOldMarker")
	
	### Last 4 are inherited from class GWADATA and can be used with RENAMEMARKER for reference file!
	
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
RENAMEMARKER.valid <- function(objRM) {
	
	if(objRM@fileRename == "")
		stop(paste(" EASY ERROR:RENAMEMARKER\n No renaming file fileRename defined. \n Please set fileRename that contains old and new MarkerNames.", sep=""))
	
	if(!file.exists(objRM@fileRename))
		stop(paste("EASY ERROR:RENAMEMARKER\n Renaming file \n ",objRM@fileRename,"\n does not exist!!!\n", sep=""))
	
	if(objRM@colRenameOldMarker == "")
		stop(paste(" EASY ERROR:RENAMEMARKER\n No column for old MarkerNames defined for fileRename. \n Please set colRenameOldMarker !!!", sep=""))
	
	if(objRM@colRenameNewMarker == "")
		stop(paste(" EASY ERROR:RENAMEMARKER\n No column for new MarkerNames defined for fileRename. \n Please set colRenameNewMarker !!!", sep=""))
			
	if(objRM@colInMarker == "")
		stop(paste(" EASY ERROR:RENAMEMARKER\n No input Marker column defined. \n Please set colInMarker that will be used for renaming SNP names.", sep=""))
	

}

RENAMEMARKER.GWADATA.valid <- function(objRM, objGWA) {
	
	isAv <- objRM@colInMarker %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:RENAMEMARKER\n Defined column colInMarker \n",objRM@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	if(!objRM@blnRetainNoMatches & !objRM@blnSaveOldMarker)
		stop(paste(" EASY ERROR:RENAMEMARKER\n You cannot set both 'blnRetainNoMatches' AND 'blnSaveOldMarker' to FALSE as that might create missings in the marker column.", sep=""))
	
	
}

RENAMEMARKER.read <- function(objRM, blnReadAll) {
	
	fR <- objRM@fileRename
	
	if(blnReadAll) {
		cat(paste("\n + Reading ",fR, "... \n"))
		#matR = scan(fR, what="character")
		tblR = read.table(fR, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = c("character","character"))
	} else {
		tblR = read.table(fR, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = c("character","character"), nrows = 10)
		
		is2Col = ncol(tblR)==2
		if(!is2Col) 
			stop(paste(" EASY ERROR:RENAMEMARKER\n File fileRename \n", fR," \n must contain exactly TWO columns separated by TAB.", sep=""))	
		
		# matR = scan(fR, what="character",nmax=1,sep="\n")
		# ## test if file consists of two columns
		# matR <- gsub("\t"," ",matR)
		# while(grepl("  ",matR)) matR <- gsub("  "," ",matR)
		# isOk = length(strsplit(matR, " ")[[1]])==2
		# if(!isOk) 
			# stop(paste(" EASY ERROR:RENAMEMARKER\n File fileRename \n", fR," \n must contain exactly TWO columns separated by TAB, COMMA or SPACE.", sep=""))	
		
		# matR = scan(fR, what="character", nmax=22)
	}
	### first row is header
	
	# matR = matrix(matR, nrow=length(matR)/2, ncol=2, byrow=TRUE)
	
	# if(dim(matR)[1]==0)
		# stop(paste("EASY ERROR:GWADATA\n There are no rows available in \n",fR,"\n The file is empty!!!\n", sep=""))
	
	# if(!(objRM@colRenameNewMarker%in%matR[1,])) 
		# stop(paste(" EASY ERROR:RENAMEMARKER\n New Marker column colRenameNewMarker \n",objRM@colRenameNewMarker," \n cannot be found in fileRename \n", fR," \n Please correct colRenameNewMarker.", sep=""))	
		
	# if(!(objRM@colRenameOldMarker%in%matR[1,])) 
		# stop(paste(" EASY ERROR:RENAMEMARKER\n New Marker column colRenameOldMarker \n",objRM@colRenameOldMarker," \n cannot be found in fileRename \n", fR," \n Please correct colRenameOldMarker.", sep=""))	
		
	# objRM@matRename <- matR
	
	if(dim(tblR)[1]==0)
		stop(paste("EASY ERROR:GWADATA\n There are no rows available in \n",fR,"\n The file is empty!!!\n", sep=""))
	
	if(!(objRM@colRenameNewMarker%in%names(tblR))) 
		stop(paste(" EASY ERROR:RENAMEMARKER\n New Marker column colRenameNewMarker \n",objRM@colRenameNewMarker," \n cannot be found in fileRename \n", fR," \n Please correct colRenameNewMarker.", sep=""))	
		
	if(!(objRM@colRenameOldMarker%in%names(tblR))) 
		stop(paste(" EASY ERROR:RENAMEMARKER\n New Marker column colRenameOldMarker \n",objRM@colRenameOldMarker," \n cannot be found in fileRename \n", fR," \n Please correct colRenameOldMarker.", sep=""))	
		
	objRM@tblRename <- tblR
	
	return(objRM)
}
#############################################################################################################################
RENAMEMARKER.run <- function(objRM, objGWA, objREPORT, isValidScript) {
						
	#### Replaces MarkerNames
	colInMarker 	<- objRM@colInMarker
	colMarkerOld 	<- objRM@colRenameOldMarker
	colMarkerNew 	<- objRM@colRenameNewMarker
	#matRename 		<- objRM@matRename
	tblRename 		<- objRM@tblRename
	strTag 			<- objRM@strTag
	blnWriteRenamed 	<- objRM@blnWriteRenamed
	blnWriteNomatches 	<- objRM@blnWriteNomatches
	blnRetainNoMatches 	<-	objRM@blnRetainNoMatches
	blnSaveOldMarker 	<-	objRM@blnSaveOldMarker
	
	strTag <- ifelse(strTag!="", paste(strTag,".",sep=""), "")
	
	iMarker 	= which(names(objGWA@tblGWA) == colInMarker)
	iMarkerOld 	= which(names(tblRename) == colMarkerOld)
	iMarkerNew 	= which(names(tblRename) == colMarkerNew)

	iMatch <- match(objGWA@tblGWA[[iMarker]],tblRename[[iMarkerOld]])
	iRename <- which(!is.na(iMatch))
	isAlreadyNewName <- objGWA@tblGWA[[iMarker]]%in%tblRename[[iMarkerNew]]

	## 8.5
	if(blnSaveOldMarker) {
		objGWA <- GWADATA.cbind(objGWA, objGWA@tblGWA[[iMarker]], paste(colInMarker,".old",sep=""), blnOverwrite=TRUE)
	}

	if(blnRetainNoMatches) {
		if(length(iRename)>0)
			objGWA@tblGWA[[iMarker]][iRename] <-  tblRename[[iMarkerNew]][iMatch[iRename]]
	} else {
		isNoMatch <- is.na(iMatch) & !isAlreadyNewName
		# iRename
		if(length(iRename)>0)
			objGWA@tblGWA[[iMarker]][iRename] <-  tblRename[[iMarkerNew]][iMatch[iRename]]
		objGWA@tblGWA[[iMarker]][which(isNoMatch)] <- NA
	}
	objGWA <- GWADATA.renamecol(objGWA, colInMarker, colMarkerNew)
	
	if(blnWriteRenamed & any(!is.na(iMatch)) & isValidScript) {
		objGWA.renamed <- GWADATA.copy(objGWA)
		objGWA.renamed@tblGWA <- objGWA@tblGWA[which(!is.na(iRename)),]
		GWADATA.write(objGWA.renamed, strSuffix = paste(".",strTag,"renamed.marker",sep=""))
		rm(objGWA.renamed)
	}
	if(blnWriteNomatches & any(is.na(iMatch) & !isAlreadyNewName) & isValidScript ) {
		objGWA.nomatch <- GWADATA.copy(objGWA)
		objGWA.nomatch@tblGWA <- objGWA@tblGWA[which(is.na(iMatch) & !isAlreadyNewName),]
		GWADATA.write(objGWA.nomatch, strSuffix = paste(".",strTag,"nomatch.marker",sep=""))
		rm(objGWA.nomatch)
	}
	
	numRenamedMatch <- length(which(!is.na(iMatch)|isAlreadyNewName))
	
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numRenamedMatch",sep=""),numRenamedMatch)
			
	return(list(objGWA,objREPORT))
}
#############################################################################################################################
RENAMEMARKER <- function(strEqcCommand){ 
	## Wrapper for class definition
	RENAMEMARKERout <- setRENAMEMARKER(new("RENAMEMARKER", strEqcCommand = strEqcCommand))
	return(RENAMEMARKERout)

}
