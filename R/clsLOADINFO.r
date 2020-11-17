setClass("LOADINFO",
	representation = representation(
						strEqcCommand				=	"character",
						colInMarker					=	"character",
						fileInfoBase				=	"character",
						afileInfo					=	"character",
						colInfoMarker				=	"character",
						blnCreateCpaid				=	"logical",
						blnUseInfoMarker			=	"logical",
						blnUseInfoChrPos			=	"logical",
						blnUseInfoPos				=	"logical",
						colInfoChr					=	"character",
						colInfoPos					=	"character",
						colInfoA1					=	"character",
						colInfoA2					=	"character",
						blnUseFastRead				=	"logical",
						tblInfo						=	"data.table",
						acolIn 						=	"character",
						acolInClasses				=	"character",
						acolNewName					=	"character",
						strMissing					= 	"character",
						strSeparator				= 	"character",
						aHeaderRead					= 	"character",
						aClassesRead				= 	"character",
						acolAdd						= 	"character",
						colInfoRsq					= 	"character",
						blnUseChrPattern 			= 	"logical",
						blnLoadAutosomesOnly 		= 	"logical",
						blnLoadGonosomesOnly 		= 	"logical"
						),
	prototype = prototype(
						strEqcCommand				=	"",
						colInMarker					=	"cpaid",
						fileInfoBase				=	"",
						afileInfo					=	"",
						colInfoMarker				=	"",
						blnCreateCpaid				=	FALSE,
						blnUseInfoMarker			=	FALSE,
						blnUseInfoChrPos			=	FALSE,
						blnUseInfoPos				=	FALSE,
						colInfoChr					=	"",
						colInfoPos					=	"",
						colInfoA1					=	"",
						colInfoA2					=	"",
						blnUseFastRead				=	TRUE,
						tblInfo						=	data.table(),
						acolIn 						=	"",
						acolInClasses				=	"",
						acolNewName					=	"",
						strMissing					= 	"-",
						strSeparator				= 	"\t",
						aHeaderRead					= 	"",
						aClassesRead				= 	"",
						acolAdd						= 	"",
						colInfoRsq					= 	"",
						blnUseChrPattern			= 	FALSE,
						blnLoadAutosomesOnly 		= 	FALSE,
						blnLoadGonosomesOnly 		= 	FALSE
						)
)

LOADINFO.set <- function(strEqcCommand, objLI, objGWADATA.default) {

	objLI@blnUseFastRead <- objGWADATA.default@blnUseFastRead
	
	aEqcSlotNamesIn = c("colInMarker", "fileInfoBase", "colInfoMarker", "colInfoChr", "colInfoPos", "colInfoA1", "colInfoA2", 
						"blnUseFastRead",	"acolIn", "acolInClasses", "acolNewName","strMissing", "strSeparator", "acolAdd","colInfoRsq","blnLoadAutosomesOnly","blnLoadGonosomesOnly")
	
	objEqcReader <- EqcReader(strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(objLI, tmpSlot) <- tmpSlotVal
		}
	}
	
	return(objLI)
}

# setGeneric("setLOADINFO", function(object) standardGeneric("setLOADINFO"))
# setMethod("setLOADINFO", signature = (object = "LOADINFO"), function(object, objGWADATA.default) {
	
	# object@blnUseFastRead <- objGWADATA.default@blnUseFastRead
	
	# aEqcSlotNamesIn = c("colInMarker","colInA1","colInA2","fileMap","colMapMarker","colMapChr","colMapPos","strTag","colInChr","colInPos","blnUseInMarker","blnUseFastRead")
	
	# ## astrPatterns
	# ## *_CHR_POS
	# ## chrCHR:POS
	
	# ### Last 4 are inherited from class GWADATA and can be used with LOADINFO for reference file!
	
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
LOADINFO.init <- function(objLI) {
	
	##############################
	## get files
	fileInfoBase <- objLI@fileInfoBase
	objLI@blnUseChrPattern = grepl("<CHR>",fileInfoBase,fixed=TRUE)
	afileInfo <- c()
	
	if(objLI@blnUseChrPattern) {
		
		pathInfo = dirname(fileInfoBase)
		fileInfoBaseName = basename(fileInfoBase)
		astrSplitFileInfo = strsplit(fileInfoBaseName,"<CHR>",fixed=T)[[1]]
		aAllFilesPath = list.files(pathInfo)
		
		isUsed = rep(TRUE,length(aAllFilesPath))
		if(length(astrSplitFileInfo)==0) stop(paste("EASY ERROR:LOADINFO\n Could not split \n ",fileInfoBase,"\n by <CHR>!!!\n", sep=""))
		
		for(i in 1:length(astrSplitFileInfo)) isUsed = isUsed & grepl(astrSplitFileInfo[i],aAllFilesPath)
		
		if(all(!isUsed)) stop(paste("EASY ERROR:LOADINFO\n Could not find any info files - please revise --fileInfoBase !!!\n", sep=""))
		
		afileInfo = paste(pathInfo,"/",aAllFilesPath[isUsed],sep="")
		
		aChr = c()
		for(fileInfo in afileInfo) {
			chr = gsub(paste(pathInfo,"/",sep=""),"",fileInfo)
			for(strSplitFileInfo in astrSplitFileInfo) chr = gsub(strSplitFileInfo,"",chr,fixed=T)
			aChr = c(aChr, chr)
		}
	
		if(objLI@blnLoadAutosomesOnly) afileInfo = afileInfo[aChr%in%c(1:22)]
		if(objLI@blnLoadGonosomesOnly) afileInfo = afileInfo[!aChr%in%c(1:22)]
		
		# for(chr in c(as.character(c(1:25)),"X")) {
			# fileInfoChr = gsub("<CHR>",chr,fileInfoBase)
			# if(file.exists(fileInfoChr)) afileInfo <- c(afileInfo, fileInfoChr)
		# }
		
	} else {
		if(file.exists(fileInfoBase)) {
			afileInfo = fileInfoBase
		} else {
			stop(paste("EASY ERROR:LOADINFO\n Could not find any info file - please revise --fileInfoBase !!!\n", sep=""))
		}
	}
	
	# if(length(afileInfo)==0)
		# stop(paste("EASY ERROR:LOADINFO\n Info file(s) \n ",fileInfoBase,"\n do(es) not exist!!!\n", sep=""))

	objLI@afileInfo <- afileInfo
		
	##############################
	## check defined columns for creation of cpaid
	## check whether cpaid should be created and if so - from what: 
	#if(objLI@colInfoChr != "" | objLI@colInfoPos != "" | objLI@colInfoA1 != "" | objLI@colInfoA2 != "") {
	if(objLI@colInfoChr != "" | objLI@colInfoPos != "" | objLI@colInfoA1 != "" | objLI@colInfoA2 != "") {
		objLI@blnCreateCpaid 	<- TRUE
		
		if(objLI@colInfoA1=="") stop(paste(" EASY ERROR:LOADINFO\n Column colInfoA1 is undefined - Please set!", sep=""))
		if(objLI@colInfoA2=="") stop(paste(" EASY ERROR:LOADINFO\n Column colInfoA2 is undefined - Please set!", sep=""))
		
		if(objLI@colInfoChr == "" & objLI@colInfoPos != "") {
			objLI@blnUseInfoPos <- TRUE
		} else if(objLI@colInfoChr != "" & objLI@colInfoPos != "") {
			objLI@blnUseInfoChrPos 	<- TRUE
		} else {
			objLI@blnUseInfoMarker 	<- TRUE
			if(objLI@colInfoMarker=="") stop(paste(" EASY ERROR:LOADINFO\n Column colInfoMarker is undefined - Please set!", sep=""))
		}
	} else {
		## no create, merge by colInfoMarker
		
		objLI@blnCreateCpaid 	<- FALSE
		if(objLI@colInfoMarker=="") stop(paste(" EASY ERROR:LOADINFO\n Column colInfoMarker is undefined - Please set or create cpaid!", sep=""))
	}
	
	##############################
	### from GWADATA init; get aHeaderRead, aClassesRead
	if(objLI@strSeparator == "TAB") objLI@strSeparator <- "\t"
	if(objLI@strSeparator == "WHITESPACE") {
		objLI@strSeparator <- ""
		if(objLI@blnUseFastRead) 
			stop("EASY ERROR:\n '--blnUseFastRead 1' cannot be used with '--strSeparator WHITESPACE'.\n Please set --strSeparator to TAB, SPACE or COMMA\n OR set '--blnUseFastRead 0'!")
	}
	if(objLI@strSeparator == "SPACE") 		objLI@strSeparator <- " "
	if(objLI@strSeparator == "COMMA") 		objLI@strSeparator <- ","
	
	##### Check File Separator
	if(all(objLI@strSeparator != c("\t", "", " ", ",")))
		stop("EASY ERROR:\n Wrong File separator defined.\n Please use TAB, WHITESPACE, SPACE or COMMA!")
	
	## create classes array (NULL) from first file
	fileInfoFirst = afileInfo[1]
	
	strHeaderTmp = scan(file=fileInfoFirst,what="character", n=1,sep="\n",quiet=TRUE)	
	if(objLI@strSeparator == "\t") {
		isTAB = grepl("\t", strHeaderTmp, fixed=T)
		if(!isTAB) 
			stop(paste("EASY WARNING:\n There is no TAB in the header of file \n",fileInfoFirst,"\n Please make sure that you have defined the correct delimiter!" ,sep="" ))
		aHeaderTmp <- strsplit(strHeaderTmp,"\t")[[1]]
	} else if(objLI@strSeparator == ",") {
		isCOMMA = grepl(",", strHeaderTmp, fixed=T)
		if(!isCOMMA) 
			stop(paste("EASY WARNING:\n There is no COMMA in the header of file \n",fileInfoFirst,"\n Please make sure that you have defined the correct delimiter!" ,sep="" ))
		aHeaderTmp <- strsplit(strHeaderTmp,",")[[1]]
	} else if(objLI@strSeparator == " ") {
		isSPACE = grepl(" ", strHeaderTmp, fixed=T)
		if(!isSPACE) 
			stop(paste("EASY WARNING:\n There is no SPACE in the header of file \n",fileInfoFirst,"\n Please make sure that you have defined the correct delimiter!" ,sep="" ))
		aHeaderTmp <- strsplit(strHeaderTmp," ")[[1]]
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
	aHeaderTmp <- gsub("(",".",aHeaderTmp,fixed=T)
	if(any(grepl("(",aHeaderTmpOld,fixed=T))) 
		warning(paste("EASY WARNING:\n Columns \n",paste(aHeaderTmpOld[which(grepl("(",aHeaderTmpOld,fixed=T))],collapse=","),"\n contain a '(' that will be renamed to '.'! Therefore the new column names \n",paste(aHeaderTmp[which(grepl("(",aHeaderTmpOld,fixed=T))],collapse=","),"\n must be used throughout the ecf file!" ,sep="" ))
	aHeaderTmp <- gsub(")",".",aHeaderTmp,fixed=T)
	if(any(grepl(")",aHeaderTmpOld,fixed=T))) 
		warning(paste("EASY WARNING:\n Columns \n",paste(aHeaderTmpOld[which(grepl(")",aHeaderTmpOld,fixed=T))],collapse=","),"\n contain a ')' that will be renamed to '.'! Therefore the new column names \n",paste(aHeaderTmp[which(grepl(")",aHeaderTmpOld,fixed=T))],collapse=","),"\n must be used throughout the ecf file!" ,sep="" ))
	
	objLI@aHeaderRead <- aHeaderTmp
	
	if(objLI@acolIn[1]=="") objLI@acolIn = objLI@aHeaderRead
	if(objLI@acolNewName[1]=="") objLI@acolNewName = objLI@acolIn
	
	### check availability of columns
	isColAvailable = objLI@acolIn %in% objLI@aHeaderRead
	if(any(!isColAvailable)) {
		strError = paste("EASY ERROR:LOADINFO\n Columns ",paste(objLI@acolIn[!isColAvailable],sep=",")," from --acolIn are not available in info file \n",fileInfoFirst,"\n Please check the columns to be read !!!", sep="")
		strError = paste(strError, "\n Available columns are: ", paste(objLI@aHeaderRead,collapse=","),sep="")
		stop(strError)
	}
	if(length(objLI@acolIn) != length(objLI@acolInClasses) & objLI@acolInClasses[1] != "") {
		stop(paste("EASY ERROR:LOADINFO\n Length of --acolIn differs from length of --acolInClasses for file\n",fileInfoFirst,"\n Please check these parameters !!!", sep=""))
	}
	if(length(objLI@acolIn) != length(objLI@acolNewName) & objLI@acolNewName[1] != "") {
		stop(paste("EASY ERROR:LOADINFO\n Length of --acolIn differs from length of --acolNewName for file\n",fileInfoFirst,"\n Please check these parameters !!!", sep=""))
	}
	
	### check columns for cpaid or marker
	if(objLI@blnCreateCpaid) {
		## require colInfoA1, colInfoA2
		isAvailable = objLI@colInfoA1 %in% objLI@acolNewName
		if(!isAvailable) {
			stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoA1,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
		}
		isAvailable = objLI@colInfoA2 %in% objLI@acolNewName
		if(!isAvailable) {
			stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoA2,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
		}
		
		if(objLI@blnUseInfoMarker) {
			## require colInfoMarker
			isAvailable = objLI@colInfoMarker %in% objLI@acolNewName
			if(!isAvailable) {
				stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoMarker,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
			}
		} else if(objLI@blnUseInfoChrPos){
			## require colInfoChr, colInfoPos, 
			isAvailable = objLI@colInfoChr %in% objLI@acolNewName
			if(!isAvailable) {
				stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoChr,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
			}
			isAvailable = objLI@colInfoPos %in% objLI@acolNewName
			if(!isAvailable) {
				stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoPos,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
			}
		} else {
			## require colInfoPos & use pattern
			isAvailable = objLI@colInfoPos %in% objLI@acolNewName
			if(!isAvailable) {
				stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoPos,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
			}
			if(!objLI@blnUseChrPattern) {
				stop(paste("EASY ERROR:LOADINFO\n If colInfoPos is defined without colInfoChr, pattern <CHR> must be used to obtain the chromosome. Please check !!!", sep=""))
			}
		}
	} else {
		## require colInfoMarker
		isAvailable = objLI@colInfoMarker %in% objLI@acolNewName
		if(!isAvailable) {
			stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoMarker,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
		}
	}
	
	### check columns to be added 
	
	if(objLI@acolAdd[1]=="" & objLI@colInfoRsq=="") stop(paste("EASY ERROR:LOADINFO\n No column to be added --acolAdd or --colInfoRsq defined. Please set --acolAdd or --colInfoRsq !!!", sep=""))
	
	if(objLI@acolAdd[1]!="" & objLI@colInfoRsq!="") stop(paste("EASY ERROR:LOADINFO\n Either set --acolAdd OR --colInfoRsq. Please remove --acolAdd or --colInfoRsq !!!", sep=""))
	
	if(objLI@acolAdd[1]!="" & objLI@colInfoRsq=="") {
		## acolAdd defined
		aisAvailable = objLI@acolAdd %in% objLI@acolNewName
		if(any(!aisAvailable)) {
			stop(paste("EASY ERROR:LOADINFO\n Columns ",paste(objLI@acolAdd[!aisAvailable],sep=","),"from --acolAdd are not available in info file \n",fileInfoFirst,"\n or not defined at --acolIn. Please check !!!", sep=""))
		}
	}
	if(objLI@acolAdd[1]=="" & objLI@colInfoRsq!="") {
		## colInfoRsq defined
		objLI@acolAdd = objLI@colInfoRsq
		isAvailable = objLI@colInfoRsq %in% objLI@acolNewName
		if(!isAvailable) {
			stop(paste("EASY ERROR:LOADINFO\n Column ",objLI@colInfoRsq,"is not available in info file \n",fileInfoFirst,"\n or not defined properly at --acolIn/--acolNewName. Please check !!!", sep=""))
		}
	}
	
	##### check classes
	aClassesTmp <- rep("NULL",length(aHeaderTmp))
		
	if(objLI@acolInClasses[1] == "") {
		
		## acolInClasses not defined
		## use best guess class from first 10 rows for all columns
		tbl_10rows <- read.table(fileInfoFirst, nrows = 10, header=TRUE, sep = objLI@strSeparator, na.strings = objLI@strMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "")
		aClasses_10rows <- sapply(tbl_10rows, class)
		#aClassesTmp[is.na(aiMatchHeader)] <- aClasses_10rows[is.na(aiMatchHeader)] ##best guess
		# cast integer to numeric
		aClasses_10rows[aClasses_10rows == "integer"] <- "numeric"
		
		aClassesTmp <- aClasses_10rows
	} else {
		## acolIn defined for a subset of columns
		## only use defined columns
	
		aiMatchColIn = match(tolower(objLI@acolIn), tolower(aHeaderTmp))
		if(any(is.na(aiMatchColIn)))
			stop(paste("EASY ERROR:LOADINFO\n Defined column \n",paste(objLI@acolIn[which(is.na(aiMatchColIn))],collapse=";")," not available in file\n",fileInfoFirst,"\n Please check !!!", sep=""))
		aClassesTmp[aiMatchColIn] <- objLI@acolInClasses
	}
		
	objLI@aClassesRead <- aClassesTmp
		
	#### Check class definitions
		
	isClassOk = objLI@aClassesRead%in%c("character","numeric","integer","double","logical","NULL")
		
	if(any(!isClassOk)) 
		stop(paste("EASY ERROR:LOADINFO\n Class \n",paste(objLI@aClassesRead[which(!isClassOk)],collapse="\n")," not defined\n Please define class 'character','numeric','double','logical', 'integer' or 'NULL' for colums\n ",paste(aHeaderTmp[which(!isClassOk)],collapse="\n")," !!!", sep=""))
	
	##############################
	
	return(objLI)

}

LOADINFO.GWADATA.valid <- function(objLI, objGWA) {
	
	isAv <- objLI@colInMarker %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:LOADINFO\n Defined column colInMarker \n",objLI@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
		
	
	# if(objLI@blnUseInfoMarker) {
		
		
		# isAv <- objLI@colInMarker %in% objGWA@aHeader
		# if(!isAv)
			# stop(paste(" EASY ERROR:LOADINFO\n Defined column colInMarker \n",objLI@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
			
		# ## colInMarker is not required when format is got from chrIn posIn
	# }
	
	# isAv <- objLI@colInA1 %in% objGWA@aHeader
	# if(!isAv)
		# stop(paste(" EASY ERROR:LOADINFO\n Defined column colInA1 \n",objLI@colInA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	# isAv <- objLI@colInA2 %in% objGWA@aHeader
	# if(!isAv)
		# stop(paste(" EASY ERROR:LOADINFO\n Defined column colInA2 \n",objLI@colInA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	# if(objLI@blnUseInPos) {
		# isAv <- objLI@colInChr %in% objGWA@aHeader
		# if(!isAv)
			# stop(paste(" EASY ERROR:LOADINFO\n Defined column colInChr \n",objLI@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct --colInChr OR remove --colInchr and --colInPos.", sep=""))
	
		# isAv <- objLI@colInPos %in% objGWA@aHeader
		# if(!isAv)
			# stop(paste(" EASY ERROR:LOADINFO\n Defined column colInPos \n",objLI@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct --colInPos OR remove --colInchr and --colInPos.", sep=""))
	# }
	
}

LOADINFO.read <- function(objLI, blnReadAll, blnUseFastRead) {
	
	colInfoMarker		= objLI@colInfoMarker
	blnUseInfoMarker 	= objLI@blnUseInfoMarker
	colInfoChr			= objLI@colInfoChr
	colInfoPos			= objLI@colInfoPos
	blnUseInfoChrPos	= objLI@blnUseInfoChrPos
	blnUseInfoPos		= objLI@blnUseInfoPos
	colInfoA1			= objLI@colInfoA1
	colInfoA2			= objLI@colInfoA2
	blnCreateCpaid		= objLI@blnCreateCpaid
	afileInfo 			<- objLI@afileInfo
	aClassesRead 		<- objLI@aClassesRead
	strSeparator 		<- objLI@strSeparator
	strMissing 			<- objLI@strMissing
	blnUseChrPattern	<- objLI@blnUseChrPattern
	fileInfoBase 		<- objLI@fileInfoBase
	
	blnUseFastRead<-NA
	blnUseFastRead <- objLI@blnUseFastRead
	
	tblInfo <- data.table()
	
	if(blnReadAll) {
		for(fileInfo in afileInfo) {
			cat(paste("   + Loading ",fileInfo, "... \n"))
			
			if(!blnUseFastRead) {
				#tblInfoTmp 	<- as.data.table(read.table(fileInfo, header=T, sep = "\t", stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = "character"))
				tblInfoTmp 	<- as.data.table(read.table(fileInfo, header=T, sep = strSeparator, na.strings = strMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = aClassesRead))
			} else {
				
				if(tolower(substring(fileInfo,nchar(fileInfo)-2,nchar(fileInfo)))==".gz") strFread <- paste("zcat ",fileInfo,sep="")
				else strFread <- fileInfo
				
				aSelect = which(aClassesRead!="NULL")
				
				tblInfoTmp 	<- tryCatch(
					#fread(strFread, sep="\t", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character","character","integer")),
					fread(strFread, sep=strSeparator, na.strings = strMissing, header=TRUE, stringsAsFactors=FALSE, colClasses = aClassesRead, select = aSelect),
					error = function(err) {
						strError = err$message
						val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
						g=scan(file = fileInfo, what=character(0), n = -1,sep = "\n",quiet=TRUE)
						iRow = which(grepl(paste(val,"\t",sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]
						stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n ", sep=""))
					}
				)
				
				for(strCol in names(tblInfoTmp)) setnames(tblInfoTmp, strCol, gsub(")",".",gsub("(",".",strCol,fixed=T),fixed=T))
			}
			
			strFileInfo = fileInfo
			if(blnUseChrPattern) {
				astrParts = unlist(strsplit(fileInfoBase,"<CHR>",fixed=T))
				for(strPart in astrParts) strFileInfo = gsub(strPart,"",strFileInfo,fixed=T)
			}
			
			isOk = strFileInfo%in%as.character(c(1:22))|grepl("female",tolower(strFileInfo))|grepl("women",tolower(strFileInfo))|grepl("male",tolower(strFileInfo)) | grepl("men",tolower(strFileInfo))
			if(!isOk) stop(paste("EASY ERROR:LOADINFO\n Extracted infoTag \n",strFileInfo,"\n unequal 1:22, female, women, male or men!!!\n", sep=""))
			
			tblInfoTmp <- cbind(tblInfoTmp, infoTag=rep(strFileInfo,nrow(tblInfoTmp)))
						
			tblInfo <- rbind(tblInfo, tblInfoTmp)
		}
	} else {
		## read 10 rows
				
		for(fileInfo in afileInfo) {
			cat(paste("\n      -> ",fileInfo))
			
			tblInfoTmp = as.data.table(read.table(fileInfo, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = aClassesRead, sep = strSeparator, na.strings = strMissing, nrows = 10))
			
			strFileInfo = fileInfo
			if(blnUseChrPattern) {
				astrParts = unlist(strsplit(fileInfoBase,"<CHR>",fixed=T))
				for(strPart in astrParts) strFileInfo = gsub(strPart,"",strFileInfo,fixed=T)
			}
			tblInfoTmp <- cbind(tblInfoTmp, infoTag=rep(strFileInfo,nrow(tblInfoTmp)))

			tblInfo <- rbind(tblInfo, tblInfoTmp)
		}
			cat(paste("\n   -> "))
	}
	
	### rename columns from acolIn to acolnewName!
	setnames(tblInfo, objLI@acolIn, objLI@acolNewName)
	
	
	if(dim(tblInfo)[1]==0)
		stop(paste("EASY ERROR:LOADINFO\n There are no rows available in \n",objLI@fileInfoBase,"\n The file is empty!!!\n", sep=""))
	
	if(blnCreateCpaid) {
	
		a1info <- as.character(tblInfo[[colInfoA1]])
		a2info <- as.character(tblInfo[[colInfoA2]])
		
		chrinfo <- rep(NA,length(a1info))
		posinfo <- rep(NA,length(a1info))
		
		if(blnUseInfoMarker) {
			markerinfo = tblInfo[[colInfoMarker]]
			## remove chr
			markerinfo <- gsub("chr","",markerinfo)
			## split by : to obtain 1:123:whatever
			idx2Colon = which(grepl(":", markerinfo, fixed=TRUE))
			if(length(idx2Colon)>0) {
				markerinfo <- markerinfo[idx2Colon]
				lsMarkerSplit = strsplit(markerinfo, ":", fixed=TRUE)
				chrSplit = unlist(lapply(lsMarkerSplit,function(x) x[1]))
				posSplit = unlist(lapply(lsMarkerSplit,function(x) x[2]))
				is3Use = chrSplit%in%c(as.character(1:25),"X")
				idx3Use = which(is3Use & !is.na(suppressWarnings(as.integer(posSplit))))
				if(length(idx3Use)>0) {
					chrinfo[idx2Colon[idx3Use]] <- chrSplit[idx3Use]
					posinfo[idx2Colon[idx3Use]] <- posSplit[idx3Use]
				}
				rm(chrSplit,posSplit,lsMarkerSplit,idx3Use,is3Use)
			}
			rm(idx2Colon,markerinfo)
		} else if(blnUseInfoChrPos) {
			chrinfo <- tblInfo[[colInfoChr]]
			posinfo <- tblInfo[[colInfoPos]]
		} else {
			## blnUseInfoPos and chr from pattern
			chrinfo <- tblInfo[["infoTag"]]
			chrinfo[grepl("male",chrinfo)|grepl("men",chrinfo)] <- "X"
			posinfo <- tblInfo[[colInfoPos]]
		}
		
		a1info[is.na(a1info)] <- "NA"
		a2info[is.na(a2info)] <- "NA"
		isA1First = a1info < a2info
				
		cpaid <- ifelse(isA1First, paste(chrinfo,":",posinfo,":",a1info,"_",a2info,sep=""), paste(chrinfo,":",posinfo,":",a2info,"_",a1info,sep=""))
		tblInfo <- cbind(tblInfo,cpaid)
		
		isNaChr <- is.na(chrinfo)|is.na(posinfo)
		if(any(isNaChr)) tblInfo <- tblInfo[which(!isNaChr),]
	}

	objLI@tblInfo <- tblInfo
	
	return(objLI)
}
#############################################################################################################################
LOADINFO.run <- function(objLI, objGWA, objREPORT, isValidScript) {
	
	blnUseChrPattern	<- objLI@blnUseChrPattern
	colInMarker <- objLI@colInMarker
	colInfoMarker	<- objLI@colInfoMarker
	blnCreateCpaid	<- objLI@blnCreateCpaid
	tblInfo	<- objLI@tblInfo
	acolAdd <- objLI@acolAdd
	
	## Important columns: cpaid/marker, Rsq
	## acolAdd
	
	if(blnUseChrPattern) {
		## tblInfo$infoTag contains 1:22, X.no.auto_male, X.no.auto_female
		## remove values where sex does not match sex of input!
		## always keep 1:22
		blnIsWomen = grepl("female",tolower(objGWA@fileInShortName))|grepl("women",tolower(objGWA@fileInShortName))
		blnIsMen = !blnIsWomen & (grepl("male",tolower(objGWA@fileInShortName)) | grepl("men",tolower(objGWA@fileInShortName)))
		
		# if(blnIsWomen) {
			# ## keep 1:22, female/women tag
			# ablnKeep = tblInfo$infoTag%in%as.character(c(1:22)) | grepl("female",tolower(tblInfo$infoTag)) | grepl("women",tolower(tblInfo$infoTag))
			# tblInfo = tblInfo[ablnKeep,]
			
		# } else if(blnIsMen){
			# ## remove female/women tag
			# ablnKeep = !grepl("female",tolower(tblInfo$infoTag)) & !grepl("women",tolower(tblInfo$infoTag))
			# tblInfo = tblInfo[ablnKeep,]
		# }
		### switch order of code to add female to ALL: 
		if(blnIsMen) {
			## remove female/women tag
			ablnKeep = !grepl("female",tolower(tblInfo$infoTag)) & !grepl("women",tolower(tblInfo$infoTag))
			tblInfo = tblInfo[ablnKeep,]
		} else {
			## female or ALL
			## keep 1:22, female/women tag
			ablnKeep = tblInfo$infoTag%in%as.character(c(1:22)) | grepl("female",tolower(tblInfo$infoTag)) | grepl("women",tolower(tblInfo$infoTag))
			tblInfo = tblInfo[ablnKeep,]
		}
		
	}
	
	strMergeInfoBy = ifelse(blnCreateCpaid, "cpaid", colInfoMarker)
	
	## duplicate handling !
	aisDuplicated  = duplicated(tblInfo[[strMergeInfoBy]]) | duplicated(tblInfo[[strMergeInfoBy]],fromLast = TRUE)
	if(any(aisDuplicated)) {
		
		## remove lower value of acolAdd[1]
		tblDup = tblInfo[aisDuplicated,]
		# order to improve readability of output!
		tblDup = tblDup[order(tblDup[[strMergeInfoBy]]),]
		
		objGWADup <- GWADATA.copy(objGWA)
		objGWADup <- GWADATA.settbl(objGWADup, tblDup)
		if(isValidScript) {
			GWADATA.write(objGWADup, strSuffix = paste(".LI.duplicated_info",sep=""))
			warning(paste("EASY WARNING: There are duplicated info values for ",length(which(aisDuplicated))/2," variants. Only the smaller info value will be added! ","\n",sep="" ))
		}
		
		tblInfo = tblInfo[!aisDuplicated,]
		
		## order by acolAdd[1]
		tblDup = tblDup[order(tblDup[[acolAdd[1]]]),]
		aisDuplicated = duplicated(tblDup[[strMergeInfoBy]])
		tblDup = tblDup[!aisDuplicated,]
		
		tblInfo = rbind(tblInfo, tblDup)
	}
	
	idxMatch = match(objGWA@tblGWA[[colInMarker]], tblInfo[[strMergeInfoBy]])
	idxMatchIn = which(!is.na(idxMatch))
	
	for(colAdd in acolAdd) {
		aVal <- rep(NA,nrow(objGWA@tblGWA))
		aVal[idxMatchIn] <- tblInfo[[colAdd]][idxMatch[idxMatchIn]]
		objGWA <- GWADATA.cbind(objGWA, aVal, colAdd)
	}
	
	objREPORT <- REPORT.addval(objREPORT,"LI.added",length(idxMatchIn))
	
	return(list(objGWA,objREPORT))
}
#############################################################################################################################
LOADINFO <- function(strEqcCommand, objGWADATA.default){ 
	## Wrapper for class definition
	LOADINFOout <- 	new("LOADINFO")
	LOADINFOout <- 	LOADINFO.set(strEqcCommand, LOADINFOout,objGWADATA.default)
	LOADINFOout	<-	LOADINFO.init(LOADINFOout)
	
	return(LOADINFOout)

}
