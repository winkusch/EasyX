setClass("EASYMERGE",
	representation = representation(
						### EASYIN PARAMS
						fileIn						=	"character",
						strMissing					= 	"character",
						strSeparator				= 	"character",
						acolIn						=	"character",
						acolInClasses				=	"character",
						acolNewName					=	"character",
						### Set within class
						aHeader						= 	"character",
						aClasses					= 	"character",
						aHeaderRead					= 	"character",
						aClassesRead				= 	"character",
						tblGWA						=	"data.table",
						colMerge					=	"character",
						strSuffix					=	"character",
						iMergeID					=	"numeric",
						blnUseFastRead				=	"logical",
						blnRemoveDuplicates			=	"logical",
						blnHeaderCommented			=	"logical",
						numRowSkip					=	"numeric",
						astrMissing					= 	"character",
						astrSetNumCol				=	"character",
						fileTranslator				=	"character",
						fileInShortName				=	"character"
						),
	prototype = prototype(
						### EASYIN PARAMS
						fileIn						=	"",
						strMissing					= 	"",
						strSeparator				= 	"WHITESPACE",
						acolIn						=	"",
						acolInClasses				=	"",
						acolNewName					=	"",
						#### Set withinh class
						aHeader						= 	"",
						aClasses					= 	"",
						aHeaderRead					= 	"",
						aClassesRead				= 	"",
						tblGWA						=	data.table(),
						colMerge					=	"",
						strSuffix					=	"",
						iMergeID					=	0,
						blnUseFastRead				=	TRUE,
						blnRemoveDuplicates			=	TRUE,
						blnHeaderCommented			=	FALSE,
						numRowSkip					=	0,
						astrMissing					= 	c("NA",".","NaN","na","nan"),
						astrSetNumCol				=	"",
						fileTranslator				=	"",
						fileInShortName				=	""
						)
)
EASYMERGE.define <- function(objEM, strConfigCommand) {

	aEqcSlotNamesIn = c("strMissing",
						"strSeparator", 
						"acolIn", 
						"acolInClasses", 
						"acolNewName",
						"pathOut",
						"fileInType",
						"blnGarbageCleaning", 
						"blnUseFastRead",
						"blnLogMemory",
						"blnLogTime",
						"blnOverwriteResults",
						"numRowSkip",
						"blnHeader",
						"fileTranslator",
						"fileInShortName"
						)

	objEqcReader <- EqcReader(strConfigCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal)) & tmpSlot%in%slotNames(objEM)) slot(objEM, tmpSlot) <- tmpSlotVal
		}
	}
	
	return(objEM)
}
EASYMERGE.easymerge <- function(objEM, strConfigCommand, icount.GWADATA) {

	aEqcSlotNamesIn = c("fileIn", 
						"strMissing", 
						"strSeparator", 
						"acolIn", 
						"acolInClasses", 
						"acolNewName", 
						"colMerge", 
						"strSuffix",
						"blnUseFastRead",
						"blnRemoveDuplicates",
						"numRowSkip",
						"blnHeader",
						"astrSetNumCol",
						"fileTranslator",
						"fileInShortName"
						)
	
	objEqcReader <- EqcReader(strConfigCommand,aEqcSlotNamesIn)
	
	for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
		tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
		tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
		
		if(all(!is.na(tmpSlotVal))) slot(objEM, tmpSlot) <- tmpSlotVal
	}
	
	objEM@iMergeID <- icount.GWADATA
	
	return(objEM)
}

EASYMERGE.init <- function(object) {
		

	if(object@strMissing != "") {
		object@astrMissing <- unique(c(object@astrMissing, object@strMissing))
	}
		
	####################################################################
	##### Reset file separator
	if(object@strSeparator == "TAB") 		object@strSeparator <- "\t"
	if(object@strSeparator == "WHITESPACE") object@strSeparator <- ""
	if(object@strSeparator == "SPACE") 		object@strSeparator <- " "
	if(object@strSeparator == "COMMA") 		object@strSeparator <- ","
	
	##### Check File Separator
	if(all(object@strSeparator != c("\t", "", " ", ",")))
		stop("EASY ERROR:\n Wrong File separator defined.\n Please use TAB, WHITESPACE, SPACE or COMMA!")

	####################################################################
	##### Check availablity of GWA file
	if(!file.exists(object@fileIn)) {
		stop(paste("EASY ERROR:\n File \n",object@fileIn,"\n does not exist!!!\n", sep=""))
	}
	
	
	####################################################################
	##### Set fileInShortName to FileName, if not defined
	if(object@fileInShortName == "") object@fileInShortName <- strsplit(object@fileIn,"/")[[1]][length(strsplit(object@fileIn,"/")[[1]])]
	
	if(object@fileTranslator!="") {
		## replace fileInShortName by appropriate value from fileTranslator
		
		tblTrans = read.table(object@fileTranslator,sep="\t",header=T,stringsAsFactors=F,colClasses="character")
		
		afsn = unlist(strsplit(object@fileInShortName,".",fixed=T))
		
		for(isn in 1:length(afsn)) {
			isFsnMatch = afsn[isn]==tblTrans$PhenotypeCode
			if(any(isFsnMatch)) {
				afsn[isn] = tblTrans$PhenotypeDescription[which(isFsnMatch)]
			}
		}
		object@fileInShortName = paste(afsn,collapse=".")
	}
	
	
	numRowSkip <- object@numRowSkip
	
	####################################################################
	##### Set object@aHeader

	strHeaderTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE)
	
	## v7:
	blnHeaderCommented = strsplit(strHeaderTmp,"")[[1]][1] == "#"
	if(blnHeaderCommented) {
		#strHeaderTmp = sub("#","X.",strHeaderTmp)
		#warning(paste("EASY WARNING:\n The header is commented by '#'. The first column name will be renamed from (for example) '#MARKER' to 'X.MARKER'!\n This new column name must be used throughout the ecf file!" ,sep="" ))
		strHeaderTmp = sub("#","",strHeaderTmp)
		warning(paste("EASY WARNING:\n The header is commented by '#'. The first column name will be renamed from (for example) '#MARKER' to 'MARKER'!\n This new column name must be used throughout the ecf file!" ,sep="" ))
	}
	object@blnHeaderCommented = blnHeaderCommented
	
	## 
	if(object@strSeparator == "\t") {
		isTAB = grepl("\t", strHeaderTmp, fixed=T)
		if(!isTAB) 
			warning(paste("EASY WARNING:\n There is no TAB in the header of file \n",object@fileIn,"\n Please make sure that you have defined the correct delimiter!" ,sep="" ))
		aHeaderTmp <- strsplit(strHeaderTmp,"\t")[[1]]
		#object@ls_afileInHeaders[[i]][k] <- aHeaderTmp
	} else if(object@strSeparator == ",") {
		isCOMMA = grepl(",", strHeaderTmp, fixed=T)
		if(!isCOMMA) 
			warning(paste("EASY WARNING:\n There is no COMMA in the header of file \n",object@fileIn,"\n Please make sure that you have defined the correct delimiter!" ,sep="" ))
		aHeaderTmp <- strsplit(strHeaderTmp,",")[[1]]
		#object@ls_afileInHeaders[[i]][k] <- aHeaderTmp
	} else if(object@strSeparator == " ") {
		isSPACE = grepl(" ", strHeaderTmp, fixed=T)
		if(!isSPACE) 
			warning(paste("EASY WARNING:\n There is no SPACE in the header of file \n",object@fileIn,"\n Please make sure that you have defined the correct delimiter!" ,sep="" ))
		aHeaderTmp <- strsplit(strHeaderTmp," ")[[1]]
		#object@ls_afileInHeaders[[i]][k] <- aHeaderTmp
	} else {
		#### Whitespace consisting of spaces and tabs
		strHeaderTmp2 <- gsub("\t", " ", strHeaderTmp)
		aHeaderTmp <- strsplit(strHeaderTmp2," ")[[1]][strsplit(strHeaderTmp2," ")[[1]]!=""]
		
	}
	
	### Allow for P-value -> P.value
	
	aHeaderTmpOld = aHeaderTmp
	aHeaderTmp <- gsub("-",".",aHeaderTmp)
	if(any(grepl("-",aHeaderTmpOld))) 
		warning(paste("EASY WARNING:\n Columns \n",paste(aHeaderTmpOld[which(grepl("-",aHeaderTmpOld))],collapse=","),"\n contain a '-' that will be renamed to '.'! Therefore the new column names \n",paste(aHeaderTmp[which(grepl("-",aHeaderTmpOld))],collapse=","),"\n must be used throughout the ecf file!" ,sep="" ))
	
	object@aHeaderRead <- object@aHeader <- aHeaderTmp
	
	
	####################################################################	
	#### Set object@aClasses to enable fast reading of input file
	## 
	if(length(object@acolIn) != length(object@acolInClasses) & object@acolInClasses[1] != "")
		stop(paste("EASY ERROR:EASYMERGE\n Length of --acolIn differs from length of --acolInClasses for file\n",object@fileIn,"\n Please check DEFINE or EASYIN statements !!!", sep=""))
	
	if(!all(object@acolNewName == "")) {
		## acolNewName defined
		if(length(object@acolIn) != length(object@acolNewName))
			stop(paste("EASY ERROR:EASYMERGE\n Length of --acolIn differs from length of --acolNewName for file\n",object@fileIn,"\n Please check DEFINE or EASYIN statements !!!", sep=""))	
	}	
	
	aClassesTmp <- rep("NULL",length(aHeaderTmp))
	
	#if(all(object@acolIn == "")) {
	if(object@acolInClasses[1] == "") {
		## acolInClasses not defined
		## use best guess class from first 10 rows for all columns
		tbl_10rows <- read.table(object@fileIn, nrows = 10, header=T, sep = object@strSeparator, na.strings = object@astrMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", skip = object@numRowSkip)
		aClasses_10rows <- sapply(tbl_10rows, class)
		#aClassesTmp[is.na(aiMatchHeader)] <- aClasses_10rows[is.na(aiMatchHeader)] ##best guess
		aClassesTmp <- aClasses_10rows
	} else {
		## acolIn defined for a subset of columns
		## only use defined columns
		
		#aiMatchColIn = match(object@acolIn, object@aHeader)
		aiMatchColIn = match(tolower(object@acolIn), tolower(object@aHeader))
		if(any(is.na(aiMatchColIn)))
			stop(paste("EASY ERROR:EASYMERGE\n Defined column \n",paste(object@acolIn[which(is.na(aiMatchColIn))],collapse=";")," not available in file\n",object@fileIn,"\n Please check !!!", sep=""))
		aClassesTmp[aiMatchColIn] <- object@acolInClasses
	}
	
	object@aClassesRead <- object@aClasses <- aClassesTmp
	
	#### Check class definitions
	
	isClassOk = object@aClasses%in%c("character","numeric","integer","double","logical","NULL")
	
	if(any(!isClassOk)) 
		stop(paste("EASY ERROR:EASYMERGE\n Class \n",paste(object@aClasses[which(!isClassOk)],collapse="\n")," not defined\n Please define class 'character','numeric','double','logical', 'integer' or 'NULL' for colums\n ",paste(object@aHeader[which(!isClassOk)],collapse="\n")," !!!", sep=""))

		
	return(object)
}

EASYMERGE.read <- function(object) {

	cat(paste("   + Merging ",object@fileIn, "... \n"))
	cat(paste("      --colMerge ",object@colMerge, "\n"))
	cat(paste("      --strSuffix ",object@strSuffix, "\n"))
	
	if(!object@blnUseFastRead) {
		object@tblGWA 	<- tryCatch(
			as.data.table(
				read.table(	object@fileIn, 
					header=T, 
					sep = object@strSeparator, 
					na.strings = object@astrMissing, 
					stringsAsFactors=FALSE, 
					strip.white = TRUE, 
					comment.char = "", 
					colClasses = object@aClassesRead,
					skip=object@numRowSkip)), 
			error = function(err) {
				strError = err$message
				val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
				g=scan(file = object@fileIn, what=character(0), n = -1,sep = "\n",quiet=TRUE)
				iRow = which(grepl(paste(val,object@strSeparator,sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]
				stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n Please specify correct column class in --acolInClasses .\n ", sep=""))
			}
		)
		#object@tblGWA <- as.data.table(object@tblGWA)
	} else{

		isGz = tolower(substring(object@fileIn,nchar(object@fileIn)-2,nchar(object@fileIn)))==".gz"
		isBgz = tolower(substring(object@fileIn,nchar(object@fileIn)-3,nchar(object@fileIn)))==".bgz"
		
		if(isGz | isBgz) strFread <- paste("zcat ",object@fileIn,sep="")
		else strFread <- object@fileIn
		
		aSelect = which(object@aClassesRead!="NULL")
		
		object@tblGWA 	<- tryCatch(
			fread(strFread, header=TRUE, na.strings = object@astrMissing, stringsAsFactors=FALSE, colClasses = object@aClassesRead, skip=object@numRowSkip, select = aSelect),
			error = function(err) {
				strError = err$message
				val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
				g=scan(file = object@fileIn, what=character(0), n = -1,sep = "\n",quiet=TRUE)
				iRow = which(grepl(paste(val,object@strSeparator,sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]
				stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n Please specify correct column class in --acolInClasses .\n ", sep=""))
			}
		)
	}
	
	if(object@blnHeaderCommented) {
		#names(object@tblGWA)[1] <- sub("X.","",names(object@tblGWA)[1])
		setnames(object@tblGWA, names(object@tblGWA)[1], sub("X.","",names(object@tblGWA)[1]))
	}
	
	if(dim(object@tblGWA)[1]==0)
		stop(paste("EASY ERROR:EASYMERGE\n There are no rows available in \n",object@fileIn,"\n The file is empty!!!\n", sep=""))
	
	iRemoveHead = which(object@aClassesRead == "NULL")
	if(length(iRemoveHead)>0) {
		object@aHeader <- object@aHeaderRead[-iRemoveHead]
		object@aClasses <- object@aClassesRead[-iRemoveHead]
	}
	
	if(all(object@acolIn != "")) {
		#  Sort according to acolIn, case unsensitive!
		
		iMatchSort=match(tolower(object@acolIn),tolower(object@aHeader))
		# Resort:
		object@aHeader = object@aHeader[iMatchSort]
		object@aClasses = object@aClasses[iMatchSort]
		object@tblGWA = object@tblGWA[,iMatchSort,with=FALSE]
		# Rename:
		object@aHeader <- object@acolIn
		#names(object@tblGWA) <- object@acolIn
		object@tblGWA <- setNames(object@tblGWA, object@acolIn)
		
		if(length(object@acolNewName)==length(object@acolIn)) {
			## acolNewName defined
			object@aHeader <- object@acolNewName
			#names(object@tblGWA) <- object@acolNewName
			object@tblGWA <- setNames(object@tblGWA, object@acolNewName)
		}
	
	}
	if(object@acolInClasses[1] != "") {
		## check if read classes match the requested classes and coerce if needed
		aClassesRead <- sapply(object@tblGWA,class)
		
		aidxCast2Num <- which(aClassesRead == "character" & object@aClasses == "numeric")
		if(length(aidxCast2Num)>0) {
			for(idxCast2Num in aidxCast2Num) object@tblGWA[[idxCast2Num]] <- as.numeric(object@tblGWA[[idxCast2Num]])
			warning(paste("EASY WARNING:GWADATA\n Coerced columns",object@aHeader[aidxCast2Num]," from character to numeric!" ,sep="" ))
		}
		aidxCast2Int <- which(aClassesRead == "character" & object@aClasses == "integer")
		if(length(aidxCast2Int)>0) {
			for(idxCast2Int in aidxCast2Int) object@tblGWA[[idxCast2Int]] <- as.integer(object@tblGWA[[idxCast2Int]])
			warning(paste("EASY WARNING:GWADATA\n Coerced columns",object@aHeader[aidxCast2Int]," from character to integer!" ,sep="" ))			
		}
		aidxCast2Dou <- which(aClassesRead == "character" & object@aClasses == "double")
		if(length(aidxCast2Dou)>0) {
			for(idxCast2Dou in aidxCast2Dou) object@tblGWA[[idxCast2Dou]] <- as.integer(object@tblGWA[[idxCast2Dou]])
			warning(paste("EASY WARNING:GWADATA\n Coerced columns",object@aHeader[aidxCast2Dou]," from character to double!" ,sep="" ))			
		}
	}
	return(object)
}


EASYMERGE.read.10rows <- function(object) {

	#object@tblGWA 	<- read.table(object@fileIn, nrows = 10, header=T, sep = object@strSeparator, na.strings = object@strMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = object@aClasses)	
	
	object@tblGWA 	<- tryCatch(
		as.data.table(
			read.table(	object@fileIn, 
				nrows = 10, 
				header=T, 
				sep = object@strSeparator, 
				na.strings = object@astrMissing, 
				stringsAsFactors=FALSE, 
				strip.white = TRUE, 
				comment.char = "", 
				colClasses = object@aClasses,
				skip=object@numRowSkip)),
		error = function(err) {
			strError = err$message
			val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
			g=scan(file = object@fileIn, what=character(0), n = -1,sep = "\n",quiet=TRUE)
			iRow = which(grepl(paste(val,object@strSeparator,sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]			
			stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n Please specifiy correct column class in --acolInClasses .\n ", sep=""))
		}
	)
	
	if(dim(object@tblGWA)[1]==0)
		stop(paste("EASY ERROR:EASYMERGE\n There are no rows available in \n ",object@fileIn,"\n The file is empty!!!\n", sep=""))
	
	iRemoveHead = which(object@aClassesRead == "NULL")
	if(length(iRemoveHead)>0) {
		object@aHeader <- object@aHeaderRead[-iRemoveHead]
		object@aClasses <- object@aClassesRead[-iRemoveHead]
	}
	
	if(all(object@acolIn != "")) {
		#  Sort according to acolIn, case unsensitive!
		
		iMatchSort=match(tolower(object@acolIn),tolower(object@aHeader))
		# Resort:
		object@aHeader = object@aHeader[iMatchSort]
		object@aClasses = object@aClasses[iMatchSort]
		object@tblGWA = object@tblGWA[,iMatchSort,with=FALSE]
		# Rename:
		object@aHeader <- object@acolIn
		#names(object@tblGWA) <- object@acolIn
		object@tblGWA <- setNames(object@tblGWA, object@acolIn)
		
		if(length(object@acolNewName)==length(object@acolIn)) {
			## acolNewName defined
			object@aHeader <- object@acolNewName
			#names(object@tblGWA) <- object@acolNewName
			object@tblGWA <- setNames(object@tblGWA, object@acolNewName)
		}
	}
	if(object@acolInClasses[1] != "") {
		## check if read classes match the requested classes and coerce if needed
		aClassesRead <- sapply(object@tblGWA,class)
		
		aidxCast2Num <- which(aClassesRead == "character" & object@aClasses == "numeric")
		if(length(aidxCast2Num)>0) {
			for(idxCast2Num in aidxCast2Num) object@tblGWA[[idxCast2Num]] <- as.numeric(object@tblGWA[[idxCast2Num]])
		}
		aidxCast2Int <- which(aClassesRead == "character" & object@aClasses == "integer")
		if(length(aidxCast2Int)>0) {
			for(idxCast2Int in aidxCast2Int) object@tblGWA[[idxCast2Int]] <- as.integer(object@tblGWA[[idxCast2Int]])
		}
		aidxCast2Dou <- which(aClassesRead == "character" & object@aClasses == "double")
		if(length(aidxCast2Dou)>0) {
			for(idxCast2Dou in aidxCast2Dou) object@tblGWA[[idxCast2Dou]] <- as.integer(object@tblGWA[[idxCast2Dou]])
		}
	}
	
	return(object)
}

EASYMERGE.GWADATA.valid <- function(objEASYMERGE, objGWA) {
	
	isAv <- objEASYMERGE@colMerge %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:EASYMERGE\n Defined column colMerge \n",objEASYMERGE@colMerge, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))

	### Check duplicate colnames in merged data set

	aHeadIn <- objGWA@aHeader
	iMarkerIn = match(objEASYMERGE@colMerge, aHeadIn)
	aHeadIn[-iMarkerIn] <- paste(aHeadIn[-iMarkerIn],"",sep="")
	
	aHeadRef <- objEASYMERGE@aHeader
	iMarkerRef = match(objEASYMERGE@colMerge, aHeadRef)
	aHeadRef[-iMarkerRef] <- paste(aHeadRef[-iMarkerRef],objEASYMERGE@strSuffix,sep="")
	
	aHeadIntersect = intersect(aHeadIn[-iMarkerIn],aHeadRef[-iMarkerRef])
	if(length(aHeadIntersect)>0)
		stop(paste(" EASY ERROR:EASYMERGE\n Column \n",paste(aHeadIntersect,collapse="\n"), 
					"\n will be duplicated after merging data to \n",objGWA@fileIn,
					"\n PLease specify correct suffixes strSuffix or rename columns in one file.", sep="")
			)
}

EASYMERGE.run <- function(objEASYMERGE, objGWA) {
	
	#cat(paste(" + Reading ",object@fileIn, "... \n"))
	if(objEASYMERGE@blnRemoveDuplicates) {
		idxRemove = which(duplicated(objEASYMERGE@tblGWA[[objEASYMERGE@colMerge]]))
		if(length(idxRemove)>0) {
			objEASYMERGE@tblGWA = objEASYMERGE@tblGWA[-idxRemove,]
		}
	}
	
	objGWA.merged <- GWADATA.merge(objGWA, objEASYMERGE, 
						strSuffix.In = "", 
						strSuffix.Add = objEASYMERGE@strSuffix, 
						blnAll.In = TRUE, 
						blnAll.Add = FALSE, 
						strBy.In = objEASYMERGE@colMerge, 
						strBy.Add = objEASYMERGE@colMerge
					) 
		
	return(objGWA.merged)
}

################################################################################################################################
################################################################################################################################
###### Wrapper for class setting
################################################################################################################################
##### Wrapper for constructing the object WITH validity checks
#EASYMERGE <- function(fileIn, fileInTag, strMissing, strSeparator, colMarker, acolPrimaryKey, aHeader, aClasses){ 
EASYMERGE <- function(){ 
	## Wrapper/constructor for class definition
	EASYMERGEout <- new("EASYMERGE")
	return(EASYMERGEout)

}

################################################################################################################################
################################################################################################################################
