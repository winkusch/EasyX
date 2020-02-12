setClass("GWADATA",
	representation = representation(
						### EASYIN PARAMS
						fileIn						=	"character",
						fileInShortName				=	"character",
						fileInTag					=	"character",
						fileInMergeTag				=	"character",
						fileInRbindTag				=	"character",
						fileInStrat					=	"character",
						fileInTrait					=	"character",
						fileInType					=	"character",
						strMissing					= 	"character",
						strSeparator				= 	"character",
						acolIn						=	"character",
						acolInClasses				=	"character",
						acolNewName					=	"character",
						pathOut						=	"character",
						###
						#bln10Rows					=	"logical",
						### Set within class
						aHeader						= 	"character",
						aClasses					= 	"character",
						aHeaderRead					= 	"character",
						aClassesRead				= 	"character",
						tblGWA						=	"data.table",
						blnMergedEasyin				=	"logical",
						blnMergedStrat				=	"logical",
						blnRbindTraits				=	"logical",
						blnHeaderCommented			=	"logical",
						numRowSkip					=	"numeric",
						numRowRead					=	"numeric",
						strRetainHead				=	"character",
						strRetainEnd				=	"character",
						blnGarbageCleaning			=	"logical",
						blnUseFastRead				=	"logical",
						blnLogMemory				=	"logical",
						blnLogTime					=	"logical",
						blnOverwriteResults			=	"logical",
						astrSetNumCol				=	"character",
						numRowAlloc					=	"numeric",
						numRowReadCount				=	"numeric",
						astrMissing					= 	"character",
						blnHeader					= 	"logical",
						fileTranslator 				= 	"character"
						),
	prototype = prototype(
						### EASYIN PARAMS
						fileIn						=	"",
						fileInShortName				=	"",
						fileInTag					=	"1",
						fileInMergeTag				=	"1",
						fileInRbindTag				=	"1",
						fileInStrat					=	"1",
						fileInTrait					=	"1",
						fileInType					=	"GWADATA",
						strMissing					= 	"",
						strSeparator				= 	"TAB",
						acolIn						=	"",
						acolInClasses				=	"",
						acolNewName					=	"",
						pathOut						=	".",
						#bln10Rows					=	FALSE,
						#### Set withinh class
						aHeader						= 	"",
						aClasses					= 	"",
						aHeaderRead					= 	"",
						aClassesRead				= 	"",
						tblGWA						=	data.table(),
						blnMergedEasyin				=	FALSE,
						blnMergedStrat				=	FALSE,
						blnRbindTraits				=	FALSE,
						blnHeaderCommented			=	FALSE,
						numRowSkip					=	0,
						numRowRead					=	-1,
						strRetainHead				=	"",
						strRetainEnd				=	"",
						blnGarbageCleaning			=	TRUE,
						blnUseFastRead				=	TRUE,
						blnLogMemory				=	TRUE,
						blnLogTime					=	TRUE,
						blnOverwriteResults			=	FALSE,
						astrSetNumCol				=	"",
						numRowAlloc					=	0,
						numRowReadCount				=	0,
						astrMissing					= 	c("NA",".","NaN","na","nan"),
						blnHeader					= 	TRUE,
						fileTranslator 				= 	""
						)
)



# GWADATA.set <- function(objGWA) {

	# return(objGWA)
# }

GWADATA.define <- function(objGWA, strConfigCommand) {

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
						"fileTranslator")
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(strConfigCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(objGWA, tmpSlot) <- tmpSlotVal
		}
	}
	
	objGWA@pathOut <- sub("EASY_INSTALL_DIR",system.file("extdata", package="EasyQC"),objGWA@pathOut)
	objGWA@pathOut <- ifelse(objGWA@pathOut == "./", getwd(), objGWA@pathOut)
	objGWA@pathOut <- ifelse(objGWA@pathOut == ".", getwd(), objGWA@pathOut)
	
	return(objGWA)
}
GWADATA.easyin <- function(objGWA, strConfigCommand) {

	#aEqcSlotNamesIn = c("fileIn","fileInShortName", "fileInTag", "fileInType", "strMissing", "strSeparator", "acolIn", "acolInClasses", "pathOut")
	aEqcSlotNamesIn = c("fileIn",
						"fileInShortName", 
						"fileInTag", 
						"fileInMergeTag", 
						"fileInRbindTag", 
						"fileInStrat", 
						"fileInTrait", 
						"fileInType", 
						"blnGarbageCleaning", 
						"strMissing", 
						"strSeparator", 
						"acolIn", 
						"acolInClasses", 
						"acolNewName", 
						"pathOut", 
						"blnUseFastRead",
						"astrSetNumCol",
						"numRowSkip",
						"numRowAlloc",
						"blnHeader",
						"fileTranslator")
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(strConfigCommand,aEqcSlotNamesIn)
	
	for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
		tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
		tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
		
		if(all(!is.na(tmpSlotVal))) slot(objGWA, tmpSlot) <- tmpSlotVal
	}
	
	objGWA@fileIn<-sub("EASY_INSTALL_DIR",system.file("extdata", package="EasyX"),objGWA@fileIn)
	
	return(objGWA)
}

GWADATA.getmetalscriptfiles <- function(objGWADATA) {

	fileMetalScript = objGWADATA@fileIn
	
	vDir<-scan(file = fileMetalScript, what=character(0), n = -1, sep = "\n",quiet=TRUE)
	isUsed = (!grepl("#",vDir)) & grepl("PROCESS",vDir)
	arrRowsUsed <- vDir[isUsed]
	## Remove "PROCESS"
	arrRowsUsed <- substring(arrRowsUsed,8,nchar(arrRowsUsed))
	## Remove whitespace
	arrRowsUsed <- gsub("\t","",arrRowsUsed,fixed=T)
	afilesIn_temp <- gsub(" ","",arrRowsUsed,fixed=T)
				
	if(length(afilesIn_temp) < 1) 
		stop(paste("ECF ERROR:\n No files processed in METALSCRIPT \n",fileMetalScript,"\n!!!\n", sep=""))
	
	ls_objGWADATA = list()
	
	for(i in 1:length(afilesIn_temp)) {
		objGWADATAtmp <- objGWADATA
			
		objGWADATAtmp@fileIn 			= afilesIn_temp[i]
		objGWADATAtmp@fileInShortName 	= strsplit(afilesIn_temp[i],"/")[[1]][length(strsplit(afilesIn_temp[i],"/")[[1]])]
		#objGWADATAtmp@fileInTag 		= strsplit(fileMetalScript,"/")[[1]][length(strsplit(fileMetalScript,"/")[[1]])]
		objGWADATAtmp@fileInType		= "GWADATA"
		
		
		ls_objGWADATA[[i]] <- objGWADATAtmp
	}
	
	return(ls_objGWADATA)
}


GWADATA.getfilelistfiles <- function(objGWADATA) {

	fileFileList = objGWADATA@fileIn

	
			aRowIn = scan(file = fileFileList, what=character(0), n = -1, sep = "\n",quiet=TRUE)

			isRemoveFromList = substring(aRowIn,1,1) == "#"
			aRowIn <- aRowIn[which(!isRemoveFromList)]
			
			ls_RowIn = strsplit(aRowIn, "\t")
			
			afilesIn_temp 	= c()
			#afileInTag_temp = c()
			
			for(iRow in 1:length(ls_RowIn)) {
				if(length(ls_RowIn[[iRow]]) == 2) {
					afilesIn_temp 	= c(afilesIn_temp, ls_RowIn[[iRow]][1])
					#afileInTag_temp = c(afileInTag_temp, ls_RowIn[[iRow]][2])
				} else {
					afilesIn_temp 	= c(afilesIn_temp, ls_RowIn[[iRow]][1])
					#afileInTag_temp = c(afileInTag_temp, strsplit(fileFileList,"/")[[1]][length(strsplit(fileFileList,"/")[[1]])])		### Set each tag to current file including fileList
				}
			}
				
	if(length(afilesIn_temp) < 1) 
		stop(paste("ECF ERROR:\n No files processed in FILELIST \n",fileFileList,"\n!!!\n", sep=""))
	
	ls_objGWADATA = list()
	
	for(i in 1:length(afilesIn_temp)) {
		objGWADATAtmp <- objGWADATA
			
		objGWADATAtmp@fileIn 			= afilesIn_temp[i]
		objGWADATAtmp@fileInShortName 	= strsplit(afilesIn_temp[i],"/")[[1]][length(strsplit(afilesIn_temp[i],"/")[[1]])]
		#objGWADATAtmp@fileInTag 		= afileInTag_temp[i]
		objGWADATAtmp@fileInType		= "GWADATA"

		ls_objGWADATA[[i]] <- objGWADATAtmp
	}
	
	return(ls_objGWADATA)
}


GWADATA.getfiles <- function(objGWADATA) {
		
	pathIn = gsub("*","",objGWADATA@fileIn)
	
	afilesIn_temp = list.files(pathIn, full.names = TRUE)
				
	if(length(afilesIn_temp) < 1) 
		stop(paste("ECF ERROR:\n No files available in \n",pathIn,"\n!!!\n", sep=""))
	
	ls_objGWADATA = list()
	
	for(i in 1:length(afilesIn_temp)) {
		objGWADATAtmp <- objGWADATA
			
		objGWADATAtmp@fileIn 			= afilesIn_temp[i]
		objGWADATAtmp@fileInShortName 	= strsplit(afilesIn_temp[i],"/")[[1]][length(strsplit(afilesIn_temp[i],"/")[[1]])]
		objGWADATAtmp@fileInTag 		= "1"
		objGWADATAtmp@fileInMergeTag 	= "1"
		objGWADATAtmp@fileInRbindTag 	= "1"
		objGWADATAtmp@fileInTrait 		= "1"
		objGWADATAtmp@fileInStrat 		= "1"
		objGWADATAtmp@fileInType		= "GWADATA"
		
		
		ls_objGWADATA[[i]] <- objGWADATAtmp
	}
	
	return(ls_objGWADATA)
}

GWADATA.init <- function(object) {
		
	if(object@strMissing != "") {
		object@astrMissing <- unique(c(object@astrMissing, object@strMissing))
	}
	
	####################################################################
	##### Reset file separator
	if(object@strSeparator == "TAB") 		object@strSeparator <- "\t"
	if(object@strSeparator == "WHITESPACE") {
		object@strSeparator <- ""
		if(object@blnUseFastRead) 
			stop("EASY ERROR:\n '--blnUseFastRead 1' cannot be used with '--strSeparator WHITESPACE'.\n Please set --strSeparator to TAB, SPACE or COMMA\n OR set '--blnUseFastRead 0'!")
	}
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
	##### Check availablity of pathOut
	if(!file.exists(object@pathOut) & object@pathOut!="NA") {
		stop(paste("EASY ERROR:\n pathOut \n ",object@pathOut,"\n does not exist!\n Please use DEFINE or EASYIN statement to set pathOut.", sep=""))
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
	
	
	####################################################################
	##### Set object@aHeader
	
	numRowSkip <- object@numRowSkip

	if(object@fileInType == "Raremetalworker") {
		# ## blabla
		# #CHROM ...
		# ...
		# #GC=...
		# head:
		### get head to be retained
		strRetainHead <- ""
	
		strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
		#strRowTmp = scan(file="testscan_head_blank.txt",what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = TRUE)
		
		isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)	
		isReadNextLine = isCommentedRow | strRowTmp == ""
		
		while(isReadNextLine) {
			numRowSkip = numRowSkip + 1
			if(isCommentedRow) {
				strRetainHead = ifelse(strRetainHead == "", strRowTmp, paste(strRetainHead,strRowTmp,sep="\n"))
			}
			strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
			#strRowTmp = scan(file="testscan_head_blank.txt",what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = TRUE)
			isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)	
			isReadNextLine = isCommentedRow | strRowTmp == ""
			
			if(length(strRowTmp)==0) break
		}
		
		## get End to be retained		
		# eof:
		# UNIX ONLY:
		nc <- nchar(object@fileIn)
		
		if(substr(object@fileIn,nc-2,nc)==".gz"){
			out <- system(paste("zcat ",object@fileIn," | wc -l",sep=""),intern=TRUE)
		} else {
			out <- system(sprintf("wc -l %s",object@fileIn),intern=TRUE)
		}
		
		numRowTotal <- as.integer(sub(sprintf("[ ]*([0-9]+)[ ]%s",object@fileIn),"\\1",out))
		numRowSkipFromEnd = 0
		strRetainEnd = ""
		strRowTmp = scan(file=object@fileIn,what="character",skip=numRowTotal-1-numRowSkipFromEnd, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
		isCommentedRow = grepl("#",strRowTmp,fixed=FALSE)	
		isReadPrevLine = isCommentedRow | strRowTmp == ""
		while(isReadPrevLine) {
			numRowSkipFromEnd = numRowSkipFromEnd + 1
			if(isCommentedRow) {
				#strRetainEnd = ifelse(strRetainEnd == "", strRowTmp, paste(strRowTmp,strRetainEnd,sep="\n"))
				strRetainEnd = paste(strRowTmp,strRetainEnd,sep="\n")
			}
			strRowTmp = scan(file=object@fileIn,what="character",skip=numRowTotal-1-numRowSkipFromEnd, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
			#strRowTmp = scan(file="testscan_head_blank.txt",what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = TRUE)
			isCommentedRow = grepl("#",strRowTmp,fixed=FALSE)	
			isReadPrevLine = isCommentedRow | strRowTmp == ""
			
			if(length(strRowTmp)==0) break
		}
		
		numRowRead = numRowTotal - numRowSkip - numRowSkipFromEnd - 1
		
		object@strRetainHead 	= strRetainHead
		object@numRowSkip 		= numRowSkip
		object@numRowRead 		= numRowRead
		object@strRetainEnd 	= strRetainEnd
		
	}
	#if(object@blnRvtests) {
	# if(object@fileInType == "Rvtests") {
		# # ## blabla
		# # CHROM ...
		# # ...
		# # head:
		
		# strRetainHead <- ""
		# strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
		# #strRowTmp = scan(file="testscan_head_blank.txt",what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = TRUE)
		
		# isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)	
		# isReadNextLine = isCommentedRow | strRowTmp == ""
		
		# while(isReadNextLine) {
			# numRowSkip = numRowSkip + 1
			# if(isCommentedRow) {
				# strRetainHead = ifelse(strRetainHead == "", strRowTmp, paste(strRetainHead,strRowTmp,sep="\n"))
			# }
			# strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
			# #strRowTmp = scan(file="testscan_head_blank.txt",what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = TRUE)
			# isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)	
			# isReadNextLine = isCommentedRow | strRowTmp == ""
			
			# if(length(strRowTmp)==0) break
		# }
		
		# object@strRetainHead 	= strRetainHead
		# object@numRowSkip 		= numRowSkip
	# }
	
	# if((object@fileInType == "Rvtests" | object@fileInType == "Raremetalworker") & object@blnUseFastRead) {
		# warning(paste("EASY WARNING:\n blnUseFastRead=1 (fread) cannot be used with Rvtests or Raremetalworker files and will be replaced by blnUseFastRead=0 (read.table)!" ,sep="" ))
		# object@blnUseFastRead = FALSE
	# }
	
	if(object@fileInType == "Raremetalworker" & object@blnUseFastRead) {
		warning(paste("EASY WARNING:\n blnUseFastRead=1 (fread) cannot be used with Raremetalworker files and will be replaced by blnUseFastRead=0 (read.table)!" ,sep="" ))
		object@blnUseFastRead = FALSE
	}
	
	######################### vcf handling
	#### get rows at beginning with ## -> vcf, eg Rvtests output
	
	strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
	isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)
	isDoubleCommentRow = isCommentedRow | strRowTmp == ""
	if(isDoubleCommentRow) {
		strRetainHead <- ""
		numRowDoubleCommentVcf = 0
		
		while(isDoubleCommentRow) {
			numRowDoubleCommentVcf = numRowDoubleCommentVcf + 1
			numRowSkip = numRowSkip + 1
			
			if(isCommentedRow) {
				strRetainHead = ifelse(strRetainHead == "", strRowTmp, paste(strRetainHead,strRowTmp,sep="\n"))
			}
			strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = FALSE)
			#strRowTmp = scan(file="testscan_head_blank.txt",what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE, blank.lines.skip = TRUE)
			isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)	
			isDoubleCommentRow = isCommentedRow | strRowTmp == ""
			
			if(length(strRowTmp)==0) break
		}
		object@strRetainHead <- strRetainHead
		object@numRowSkip <- numRowSkip
		
		# if(numRowDoubleCommentVcf>0) warning(paste("EASY WARNING:\n File contains ",numRowDoubleCommentVcf," ## rows at the beginning that are skipped for reading and added back in for writing!" ,sep="" ))
		if(numRowDoubleCommentVcf>0) cat(paste("EASY READING:\n File contains ",numRowDoubleCommentVcf," ## rows at the beginning that are skipped for reading and added back in for writing!" ,sep="" ))
	}
	######################### vcf handling
	
	
	# ## 14-01-13: Get number of rows to skip at the beginning:
	# strHeaderRetain = object@strHeaderRetain
	# numRowSkip = 0
	# strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE)
	# isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)
	# while(isCommentedRow) {
		# numRowSkip = numRowSkip + 1
		# strHeaderRetain = ifelse(strHeaderRetain == "", strRowTmp, paste(strHeaderRetain,strRowTmp,sep="\n"))
		# strRowTmp = scan(file=object@fileIn,what="character",skip=numRowSkip, n=1,sep="\n",quiet=TRUE)
		# isCommentedRow = grepl("##",strRowTmp,fixed=FALSE)	
	# }
	# object@strHeaderRetain = strHeaderRetain
	# object@numRowSkip = numRowSkip
	## 
	
	if(object@blnHeader) {
		## set aHeaderRead, aClassesRead
		
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
		aHeaderTmp <- gsub("(",".",aHeaderTmp,fixed=T)
		if(any(grepl("(",aHeaderTmpOld,fixed=T))) 
			warning(paste("EASY WARNING:\n Columns \n",paste(aHeaderTmpOld[which(grepl("(",aHeaderTmpOld,fixed=T))],collapse=","),"\n contain a '(' that will be renamed to '.'! Therefore the new column names \n",paste(aHeaderTmp[which(grepl("(",aHeaderTmpOld,fixed=T))],collapse=","),"\n must be used throughout the ecf file!" ,sep="" ))
		aHeaderTmp <- gsub(")",".",aHeaderTmp,fixed=T)
		if(any(grepl(")",aHeaderTmpOld,fixed=T))) 
			warning(paste("EASY WARNING:\n Columns \n",paste(aHeaderTmpOld[which(grepl(")",aHeaderTmpOld,fixed=T))],collapse=","),"\n contain a ')' that will be renamed to '.'! Therefore the new column names \n",paste(aHeaderTmp[which(grepl(")",aHeaderTmpOld,fixed=T))],collapse=","),"\n must be used throughout the ecf file!" ,sep="" ))
		
		
		
		object@aHeaderRead <- object@aHeader <- aHeaderTmp
		
		####################################################################	
		#### Set object@aClasses to enable fast reading of input file
		## 
		
		if(length(object@acolIn) != length(object@acolInClasses) & object@acolInClasses[1] != "")
			stop(paste("EASY ERROR:GWADATA\n Length of --acolIn differs from length of --acolInClasses for file\n",object@fileIn,"\n Please check DEFINE or EASYIN statements !!!", sep=""))
		
		if(!all(object@acolNewName == "")) {
			## acolNewName defined
			if(length(object@acolIn) != length(object@acolNewName))
				stop(paste("EASY ERROR:GWADATA\n Length of --acolIn differs from length of --acolNewName for file\n",object@fileIn,"\n Please check DEFINE or EASYIN statements !!!", sep=""))	
		}	
		
		aClassesTmp <- rep("NULL",length(aHeaderTmp))
		
		#if(all(object@acolIn == "")) {
		if(object@acolInClasses[1] == "") {
			
			## acolInClasses not defined
			## use best guess class from first 10 rows for all columns
			tbl_10rows <- read.table(object@fileIn, nrows = 10, header=object@blnHeader, sep = object@strSeparator, na.strings = object@astrMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", skip = object@numRowSkip)
			aClasses_10rows <- sapply(tbl_10rows, class)
			#aClassesTmp[is.na(aiMatchHeader)] <- aClasses_10rows[is.na(aiMatchHeader)] ##best guess
			# cast integer to numeric
			aClasses_10rows[aClasses_10rows == "integer"] <- "numeric"
			
			aClassesTmp <- aClasses_10rows
		} else {
			## acolIn defined for a subset of columns
			## only use defined columns
			
			#aiMatchColIn = match(object@acolIn, object@aHeader)
			aiMatchColIn = match(tolower(object@acolIn), tolower(object@aHeader))
			if(any(is.na(aiMatchColIn)))
				stop(paste("EASY ERROR:GWADATA\n Defined column \n",paste(object@acolIn[which(is.na(aiMatchColIn))],collapse=";")," not available in file\n",object@fileIn,"\n Please check !!!", sep=""))
			aClassesTmp[aiMatchColIn] <- object@acolInClasses
		}
		
		object@aClassesRead <- object@aClasses <- aClassesTmp
		
		#### Check class definitions
		
		isClassOk = object@aClasses%in%c("character","numeric","integer","double","logical","NULL")
		
		if(any(!isClassOk)) 
			stop(paste("EASY ERROR:GWADATA\n Class \n",paste(object@aClasses[which(!isClassOk)],collapse="\n")," not defined\n Please define class 'character','numeric','double','logical', 'integer' or 'NULL' for colums\n ",paste(object@aHeader[which(!isClassOk)],collapse="\n")," !!!", sep=""))

			
	} else {
		## blnHeader = FALSE
		
		object@aHeaderRead <- object@aHeader <- ""
		
		if(!object@blnHeader & all(object@acolNewName == "")) {
			stop(paste("EASY ERROR:GWADATA\n blnHeader set to FALSE requires setting of --acolNewName for file\n",object@fileIn,"\n Please add --acolNewName to set the column names !!!", sep=""))	
		}
		
		aClassesTmp <- rep("NULL",length(object@acolNewName))
		
		if(object@acolInClasses[1] == "") {
			tbl_10rows <- read.table(object@fileIn, nrows = 10, header=object@blnHeader, sep = object@strSeparator, na.strings = object@astrMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", skip = object@numRowSkip)
			aClasses_10rows <- sapply(tbl_10rows, class)
			aClasses_10rows[aClasses_10rows == "integer"] <- "numeric"
			aClassesTmp <- aClasses_10rows
		} else {
			aClassesTmp <- object@acolInClasses
		}
		
		object@aClassesRead <- object@aClasses <- aClassesTmp
		isClassOk = object@aClasses%in%c("character","numeric","integer","double","logical","NULL")
		if(any(!isClassOk)) 
			stop(paste("EASY ERROR:GWADATA\n Class \n",paste(object@aClasses[which(!isClassOk)],collapse="\n")," not defined\n Please define class 'character','numeric','double','logical', 'integer' or 'NULL' for colums\n ",paste(object@aHeader[which(!isClassOk)],collapse="\n")," !!!", sep=""))
	}

	return(object)
}

GWADATA.read <- function(object) {
	
	cat(paste("   + Reading ",object@fileIn, "... \n"))
	
	if(!object@blnUseFastRead) {
		object@tblGWA 	<- tryCatch(
			as.data.table(
				read.table(	object@fileIn, 
					header=object@blnHeader, 
					sep = object@strSeparator, 
					na.strings = object@astrMissing, 
					stringsAsFactors=FALSE, 
					strip.white = TRUE, 
					comment.char = "", 
					colClasses = object@aClassesRead, 
					skip = object@numRowSkip,
					nrows = object@numRowRead)), 
			error = function(err) {
				strError = err$message
				val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
				g=scan(file = object@fileIn, what=character(0), n = -1,sep = "\n",quiet=TRUE)
				iRow = which(grepl(paste(val,object@strSeparator,sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]
				stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n Please specify correct column class in --acolInClasses .\n ", sep=""))
			}
		)
		#object@tblGWA <- as.data.table(object@tblGWA)
	} else {
		
		isGz = tolower(substring(object@fileIn,nchar(object@fileIn)-2,nchar(object@fileIn)))==".gz"
		isBgz = tolower(substring(object@fileIn,nchar(object@fileIn)-3,nchar(object@fileIn)))==".bgz"
		
		if(isGz | isBgz) strFread <- paste("zcat ",object@fileIn,sep="")
		else strFread <- object@fileIn
		
		### update for new data.table package
		aSelect = which(object@aClassesRead!="NULL")
		
		object@tblGWA 	<- tryCatch(
			fread(strFread, header=object@blnHeader, na.strings = object@astrMissing, stringsAsFactors=FALSE, colClasses = object@aClassesRead, skip=object@numRowSkip, select = aSelect),
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
	
	if(!object@blnHeader) {
		setnames(object@tblGWA, names(object@tblGWA), object@acolNewName)
		object@acolIn <- object@aHeader <- object@acolNewName
	}
	
	if(dim(object@tblGWA)[1]==0)
		stop(paste("EASY ERROR:GWADATA\n There are no rows available in \n",object@fileIn,"\n The file is empty!!!\n", sep=""))
	
	object@numRowReadCount <- dim(object@tblGWA)[1]
	
	if(object@numRowReadCount < object@numRowAlloc) {
		### add NA values at end of data table until numRowAlloc rows
		### set numRowAlloc
		object@numRowReadCount <- dim(object@tblGWA)[1]
		tBind = setNames(as.data.table(matrix(NA,object@numRowAlloc - dim(object@tblGWA)[1],dim(object@tblGWA)[2])), names(object@tblGWA))
		object@tblGWA <- rbind(object@tblGWA, tBind)
	}
	
	iRemoveHead = which(object@aClassesRead == "NULL")
	if(length(iRemoveHead)>0) {
		object@aHeader <- object@aHeaderRead[-iRemoveHead]
		object@aClasses <- object@aClassesRead[-iRemoveHead]
	}
	
	if(all(object@acolIn != "")) {
		#  Sort according to acolIn, case unsensitive!
		
		iMatchSort=match(tolower(object@acolIn),tolower(object@aHeader))
		# Resort to acolIn:
		object@aHeader 	= object@aHeader[iMatchSort]
		object@aClasses = object@aClasses[iMatchSort]
		object@tblGWA 	= object@tblGWA[,iMatchSort,with=FALSE]
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


GWADATA.read.10rows <- function(object) {

	#object@tblGWA 	<- read.table(object@fileIn, nrows = 10, header=T, sep = object@strSeparator, na.strings = object@strMissing, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = object@aClasses)	
	
	object@tblGWA 	<- tryCatch(
		as.data.table(
			read.table(	object@fileIn, 
				nrows = 10, 
				header=object@blnHeader, 
				sep = object@strSeparator, 
				na.strings = object@astrMissing, 
				stringsAsFactors=FALSE, 
				strip.white = TRUE, 
				comment.char = "", 
				colClasses = object@aClasses, 
				skip = object@numRowSkip)),
		error = function(err) {
			strError = err$message
			val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
			g=scan(file = object@fileIn, what=character(0), n = -1,sep = "\n",quiet=TRUE)
			iRow = which(grepl(paste(val,object@strSeparator,sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]			
			stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n Please specifiy correct column class in --acolInClasses .\n ", sep=""))
		}
	)
	
	if(object@blnHeaderCommented) {
		#names(object@tblGWA)[1] <- sub("X.","",names(object@tblGWA)[1])
		setnames(object@tblGWA, names(object@tblGWA)[1], sub("X.","",names(object@tblGWA)[1]))
	}
	
	if(!object@blnHeader) {
		setnames(object@tblGWA, names(object@tblGWA), object@acolNewName)
		object@acolIn <- object@aHeader <- object@acolNewName
	}
	
	if(dim(object@tblGWA)[1]==0)
		stop(paste("EASY ERROR:GWADATA\n There are no rows available in \n ",object@fileIn,"\n The file is empty!!!\n", sep=""))
	
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

### CBIND METHOD
GWADATA.merge <- function(objGWA, objGWA.Add, strSuffix.In = ".in", strSuffix.Add = ".add", blnAll.In = FALSE, blnAll.Add = FALSE, strBy.In = NA, strBy.Add = NA) {
	
	if(is.na(strBy.In) & is.na(strBy.Add)) strBy.In <- strBy.Add <- intersect(objGWA@aHeader, objGWA.Add@aHeader)
	
	### Keep everything from X, Add columns from Y
	
	
	iMarker.In = match(strBy.In, objGWA@aHeader)
	aHeaderIn 	<- objGWA@aHeader
	aHeaderIn[-iMarker.In] <- paste(aHeaderIn[-iMarker.In],strSuffix.In,sep="")
	objGWA@tblGWA  <- setNames(objGWA@tblGWA,aHeaderIn)
	objGWA@aHeader <- aHeaderIn

	# setkeyv(objGWA@tblGWA, strBy.In)

	iMarker.Add = match(strBy.Add, objGWA.Add@aHeader)
	aHeaderAdd 	<- objGWA.Add@aHeader
	aHeaderAdd[-iMarker.Add] <- paste(aHeaderAdd[-iMarker.Add],strSuffix.Add,sep="")
	objGWA.Add@tblGWA  <- setNames(objGWA.Add@tblGWA,aHeaderAdd)
	objGWA.Add@aHeader <- aHeaderAdd
	### 
	## Rename marker column in dt 2 to marker column in dt 1
	setnames(objGWA.Add@tblGWA, strBy.Add, strBy.In)
	
	## set

	# setkeyv(objGWA.Add@tblGWA, strBy.In)
	
	objGWA@tblGWA <- merge(objGWA@tblGWA, objGWA.Add@tblGWA, 
								by = strBy.In, 
								all.x = blnAll.In, all.y = blnAll.Add, 
								suffixes = c(strSuffix.In, strSuffix.Add),
								allow.cartesian=TRUE
								)
	
	# objGWA@tblGWA <- merge(objGWA@tblGWA, objGWA.Add@tblGWA, 
								# by.x = strBy.In, by.y = strBy.Add, 
								# all.x = blnAll.In, all.y = blnAll.Add, 
								# suffixes = c(strSuffix.In, strSuffix.Add)
								# )
	## suffixes = c("", "") produces duplicate columns
	isDuplicateCol <- duplicated(names(objGWA@tblGWA))
	if(any(isDuplicateCol)) 
		stop("Duplicate column produced within merge command. PLease specify correct suffixes.")
	
	objGWA@aHeader <- names(objGWA@tblGWA)
	objGWA@aClasses <- sapply(objGWA@tblGWA,class)
	#for(i in 1:ncol(objGWA.Out@tblGWA)) objGWA.Out@aClasses[i] <- class(objGWA.Out@tblGWA[,i])
	
	return(objGWA)	
}
GWADATA.rbind <- function(objGWAIn, objGWAAdd) {
	
	if(length(objGWAIn@aHeader)!=length(objGWAAdd@aHeader))
		stop(paste("EASY ERROR:GWADATA\n Rbind not possible for GWADATA sets with unequal number of columns!!!", sep=""))
	if(any(objGWAIn@aHeader != objGWAAdd@aHeader))
		stop(paste("EASY ERROR:GWADATA\n Rbind not possible for GWADATA sets with unequal headers!!!", sep=""))

	nrowIn <- dim(objGWAIn@tblGWA)[1]
	nrowAdd <- dim(objGWAAdd@tblGWA)[1]
	# 
	
	if(objGWAIn@numRowReadCount > 0 & objGWAIn@numRowReadCount < nrowIn) {
		## add values at numRowReadCount + 1
		if((objGWAIn@numRowReadCount + nrowAdd) < nrowIn) {
			## full match, no need to allocate extra space
			## add objGWAAdd at numRowReadCount + 1 
			objGWAIn@tblGWA[(objGWAIn@numRowReadCount+1):(objGWAIn@numRowReadCount+nrowAdd),] <- objGWAAdd@tblGWA
			objGWAIn@numRowReadCount = objGWAIn@numRowReadCount + nrowAdd
		} else {
			## remove last allocated rows and do manual rbind 
			objGWAIn@tblGWA <- objGWAIn@tblGWA[-((objGWAIn@numRowReadCount+1):nrowIn),]
			objGWAIn@tblGWA <- rbind(objGWAIn@tblGWA, objGWAAdd@tblGWA)
			objGWAIn@numRowReadCount <- 0
		}
	} else { 
		objGWAIn@tblGWA <- rbind(objGWAIn@tblGWA, objGWAAdd@tblGWA)
		objGWAIn@numRowReadCount <- dim(objGWAIn@tblGWA)[1]
	}
	rm(objGWAAdd)
	
	return(objGWAIn)	
}
### CBIND METHOD
GWADATA.cbind <- function(objGWA, aNewCol, strNewColName, blnOverwrite=FALSE) {
	
	strNewColName2 <- strNewColName
	
	isAlreadyInTable = strNewColName2%in%objGWA@aHeader
	
	# if(length(aNewCol) == 1) {
		# aNewCol = rep(aNewCol, dim(objGWA@tblGWA)[1])
	# }
	
	if(length(aNewCol) != dim(objGWA@tblGWA)[1])
		stop(paste("EASY ERROR:GWADATA\n Cbind not possible for new column \n ",strNewColName,". \n Different number of values!!!", sep=""))
		
	if(isAlreadyInTable & !blnOverwrite) {
		iCount = 1
		while(isAlreadyInTable) {
			strNewColName2 = paste(strNewColName,".",iCount,sep="")
			isAlreadyInTable = strNewColName2%in%objGWA@aHeader
			iCount = iCount + 1
		}
		
		#objGWA@tblGWA = cbind(objGWA@tblGWA, aNewCol, stringsAsFactors = FALSE)
		objGWA@tblGWA = cbind(objGWA@tblGWA, aNewCol)
		#names(objGWA@tblGWA)[ncol(objGWA@tblGWA)] <- strNewColName2
		setnames(objGWA@tblGWA, names(objGWA@tblGWA)[ncol(objGWA@tblGWA)], strNewColName2)
		
		objGWA@aHeader <- c(objGWA@aHeader, strNewColName2)
		objGWA@aClasses <- c(objGWA@aClasses, class(aNewCol))
	} else if(isAlreadyInTable & blnOverwrite) {
		
		iMatch = match(strNewColName, objGWA@aHeader)
		objGWA@tblGWA[[iMatch]] = aNewCol
		#objGWA@aHeader: no change
		objGWA@aClasses[iMatch] <- class(aNewCol)
	
	} else {
		### not in Table -> Add at the end
		#objGWA@tblGWA = cbind(objGWA@tblGWA, aNewCol, stringsAsFactors = FALSE)
		objGWA@tblGWA = cbind(objGWA@tblGWA, aNewCol)
		# names(objGWA@tblGWA)[ncol(objGWA@tblGWA)] <- strNewColName
		setnames(objGWA@tblGWA, names(objGWA@tblGWA)[ncol(objGWA@tblGWA)], strNewColName)
		
		objGWA@aHeader <- c(objGWA@aHeader, strNewColName)
		objGWA@aClasses <- c(objGWA@aClasses, class(aNewCol))
	}
	
	return(objGWA)	
}

GWADATA.getcol <- function(objGWA, col) {
	
	iMatch = match(col, objGWA@aHeader)
	
	if(any(is.na(iMatch)))
		stop(paste("EASY ERROR:GWADATA.getcol\n Column to extract \n ",paste(col[which(is.na(iMatch))],collapse="\n")," not available in\n ",objGWA@fileInShortName, "\n !!!", sep=""))
	
	return(objGWA@tblGWA[[iMatch]])	
}

GWADATA.getcols <- function(objGWA, aCols, blnSuppressError=FALSE) {
	
	iMatch = match(aCols, objGWA@aHeader)
	
	if(any(is.na(iMatch)) & !blnSuppressError)
		stop(paste("EASY ERROR:GWADATA.getcols\n Column to extract \n ",paste(aCols[which(is.na(iMatch))],collapse="\n")," not available in\n ",objGWA@fileInShortName, "\n !!!", sep=""))
	
	if(all(is.na(iMatch)))
		stop(paste("EASY ERROR:GWADATA.getcols\n None of the requested columns \n ",paste(aCols[which(is.na(iMatch))],collapse="\n")," is available in\n ",objGWA@fileInShortName, "\n !!!", sep=""))
	
	iMatch <- iMatch[which(!is.na(iMatch))]
	
	objGWA@tblGWA 	= objGWA@tblGWA[,iMatch,with=FALSE]
	objGWA@aHeader 	= objGWA@aHeader[iMatch]
	objGWA@aClasses = objGWA@aClasses[iMatch]
	
	#if(any(iMatch == objGWA@icolMarker)) objGWA@icolMarker = which(iMatch == objGWA@icolMarker)
	
	return(objGWA)	
}

GWADATA.removecols <- function(objGWA, aCols) {
	
	iMatch = match(aCols, objGWA@aHeader)
	iMatch = iMatch[!is.na(iMatch)]
	
	if(length(iMatch)>0) {
		objGWA@tblGWA = objGWA@tblGWA[,-iMatch,with=FALSE]
		objGWA@aHeader = objGWA@aHeader[-iMatch]
		objGWA@aClasses = objGWA@aClasses[-iMatch]
	}
	
	return(objGWA)	
}

GWADATA.getrows <- function(objGWA, iRowIndices) {
	
	if(length(iRowIndices)>0) {
		if(any(abs(iRowIndices) > dim(objGWA@tblGWA)[1]))
			stop(paste("EASY ERROR:GWADATA.getrows\n Index exceeds dimension!!!\n", sep=""))
		
		objGWA@tblGWA = objGWA@tblGWA[iRowIndices,]
	}
	if(length(iRowIndices) == 0) {
		objGWA@tblGWA = objGWA@tblGWA[rep(FALSE,nrow(objGWA@tblGWA)),]
	}
	
	return(objGWA)
}


GWADATA.removerows <- function(objGWA, iRowIndices) {
	
	if(length(iRowIndices)>0) {
		if(any(abs(iRowIndices) > dim(objGWA@tblGWA)[1]))
			stop(paste("EASY ERROR:GWADATA.removerows\n Index exceeds dimension!!!\n", sep=""))
		
		objGWA@tblGWA = objGWA@tblGWA[-abs(iRowIndices),]
	}
	
	return(objGWA)	
}
GWADATA.renamecol <- function(objGWA, colOld, colNew) {
	
	objGWA@aHeader[objGWA@aHeader == colOld] <- colNew
	setnames(objGWA@tblGWA, colOld, colNew)
	#names(objGWA@tblGWA)[names(objGWA@tblGWA) == colOld] <- colNew
	
	if(any(objGWA@aHeader != names(objGWA@tblGWA))) 
		stop(paste("EASY ERROR:GWADATA.renamecol\n aHeader does not match names(tblGWA)!!!\n", sep=""))
	
	return(objGWA)	
}
GWADATA.copy <- function(objGWA) {
		
	#objGWA@tblGWA <- data.frame()
	objGWA@tblGWA 	<- data.table()
	objGWA@aHeader 	<- objGWA@aClasses <- ""
	
	return(objGWA)	
}
GWADATA.settbl <- function(objGWA, tblGWA) {
		
	objGWA@tblGWA 	<- as.data.table(tblGWA)
	objGWA@aHeader 	<- names(tblGWA)
	objGWA@aClasses <- sapply(tblGWA,class)
	
	return(objGWA)	
}
GWADATA.reset <- function(objGWA) {
		
	## check names vs aHeader
	#if(length(objGWA@aHeader) != length(names(objGWA@tblGWA))) {
		## reset using tbl-Names, print warning
		
	isHeadIdentical = identical(names(objGWA@tblGWA), objGWA@aHeader)	
	
	if(!isHeadIdentical) {
		isNotInaHeader = !names(objGWA@tblGWA)%in%objGWA@aHeader
		if(any(isNotInaHeader)) 
			warning(paste("EASY WARNING:GWADATA\n Columns",names(objGWA@tblGWA)[which(isNotInaHeader)]," from tblGWA are not present in aHeader and will be re-set!" ,sep="" ))

		isNotInTblNames = !objGWA@aHeader%in%names(objGWA@tblGWA)
		if(any(isNotInTblNames)) 
			warning(paste("EASY WARNING:GWADATA\n Columns",objGWA@aHeader[which(isNotInTblNames)]," from tblGWA are not present in aHeader and will be re-set!" ,sep="" ))
		
		objGWA@aHeader <- names(objGWA@tblGWA)
	}
	
	isClassIdentical = identical(sapply(objGWA@tblGWA,class), objGWA@aClasses)	
	if(!isClassIdentical) {
		objGWA@aClasses <- sapply(objGWA@tblGWA,class)
	}
	
	isClassOk = objGWA@aClasses%in%c("character","numeric","integer","double","logical","NULL")
	
	if(any(!isClassOk)) 
		stop(paste("EASY ERROR:GWADATA\n Class \n",paste(objGWA@aClasses[which(!isClassOk)],collapse="\n")," not defined\n Please define class 'character','numeric','double','logical', 'integer' or 'NULL' for colums\n ",paste(objGWA@aHeader[which(!isClassOk)],collapse="\n")," !!!", sep=""))

	return(objGWA)	
}
GWADATA.sort <- function(objGWA, acolSort, ablnDescending) {
	
	anumDirection <- ifelse(ablnDescending, -1, 1)
	setorderv(objGWA@tblGWA, acolSort, anumDirection)
		
	return(objGWA)	
}
GWADATA.write <- function(objGWA, strMode = "txt", strPrefix = "", strSuffix = "", strSep = "\t", strMissing = "NA", strTabixParam="", blnHeader = TRUE, blnWrite10rows=FALSE, colSnps = "rsid", blnWriteSnpList=FALSE, strSnpListSep = "\n") {		
	
	
	pathOut <- objGWA@pathOut
	
	if(strsplit(pathOut,"")[[1]][nchar(pathOut)] != "/") pathOut <- paste(pathOut,"/",sep="")
	
	fileInShortName = objGWA@fileInShortName
	fileInShortName = gsub(".txt","",fileInShortName)
	fileInShortName = gsub(".gz","",fileInShortName)
	fileInShortName = gsub(".bgz","",fileInShortName)
	isMetaScoreAssocFile = ifelse(grepl(".MetaScore.assoc", fileInShortName), 1, 0)
	
	if(nchar(strPrefix)>0) {
		if(strsplit(strPrefix,"")[[1]][nchar(strPrefix)] != ".")
		strPrefix = paste(strPrefix,".",sep="") 
	}
	if(nchar(strSuffix)>0) {
		if(strsplit(strSuffix,"")[[1]][1] != ".")
		strSuffix = paste(".",strSuffix,sep="") 
	}
	
	if(isMetaScoreAssocFile) {
		fileInShortName = gsub(".MetaScore.assoc","",fileInShortName)
		fileOutBase	<- paste(pathOut,strPrefix,fileInShortName,strSuffix,".MetaScore.assoc",sep="")
	} else {
		fileOutBase	<- paste(pathOut,strPrefix,fileInShortName,strSuffix,sep="")
	}
	
	if(strMode == "gz") {
		## write as gz file
		if(nchar(fileOutBase)>3) {
			if(substring(fileOutBase, nchar(fileOutBase)-2, nchar(fileOutBase)) == ".gz")
				fileOutBase <- substring(fileOutBase, 1, nchar(fileOutBase)-3)
		}
		
		fileOut	<- paste(fileOutBase,".gz",sep="")
		if(objGWA@blnOverwriteResults & file.exists(fileOut)) file.remove(fileOut)
		i = 1
		while(file.exists(fileOut)) {
			#fileOut <- paste(fileOutBase,".",i,".txt.gz",sep="")
			fileOut <- paste(fileOutBase,".",i,".gz",sep="")
			i = i + 1
		}

		fileOutGZ <- gzfile(fileOut, "w")
		if(objGWA@strRetainHead == "") {
			write.table(objGWA@tblGWA, fileOutGZ, row.names=F, quote=F, sep=strSep, na=strMissing, col.names=blnHeader)	
			
		} else {
			sink(fileOutGZ)
			cat(objGWA@strRetainHead)
			cat("\n")
			#if(objGWA@blnRaremetalworker) cat("#") 
			if(objGWA@fileInType == "Raremetalworker") cat("#") 
			sink()
			suppressWarnings(write.table(objGWA@tblGWA, fileOutGZ, row.names=F, quote=F, sep=strSep, na=strMissing, append=TRUE, col.names=blnHeader))
		}
		if(objGWA@strRetainEnd != "") {
			sink(fileOutGZ, append=TRUE)
			cat(objGWA@strRetainEnd)
			sink()
		}
		close(fileOutGZ)
		
	} else {
		## write as tab-del text file
		## also if strMode = bgz !
		if(nchar(fileOutBase)>4) {
			if(substring(fileOutBase, nchar(fileOutBase)-3, nchar(fileOutBase)) == ".txt") 
				fileOutBase <- substring(fileOutBase, 1, nchar(fileOutBase)-4)
		}
		
		fileOut	<- paste(fileOutBase,".txt",sep="")
		if(objGWA@blnOverwriteResults & file.exists(fileOut)) file.remove(fileOut)
		i = 1
		while(file.exists(fileOut)) {
			fileOut <- paste(fileOutBase,".",i,".txt",sep="")
			i = i + 1
		}
		if(objGWA@strRetainHead == "") {
			write.table(objGWA@tblGWA, fileOut, row.names=F, quote=F, sep=strSep, na=strMissing, col.names=blnHeader)		
		} else {
			sink(fileOut)
			cat(objGWA@strRetainHead)
			cat("\n")
			#if(objGWA@blnRaremetalworker) cat("#") 
			if(objGWA@fileInType == "Raremetalworker") cat("#") 
			sink()
			suppressWarnings(write.table(objGWA@tblGWA, fileOut, row.names=F, quote=F, sep=strSep, na=strMissing, append=TRUE, col.names=blnHeader))
		}
		if(objGWA@strRetainEnd != "") {
			sink(fileOut, append=TRUE)
			cat(objGWA@strRetainEnd)
			sink()
		}
		
	}
	
	if(strMode == "bgz") {
		### bgz written text file
		system(paste("bgzip ",fileOut,sep=""))
		fileOut <- paste(fileOut,".gz",sep="")
	}
	if(strTabixParam != "") {
		if(substr(strTabixParam,1,1)=="'") strTabixParam = substr(strTabixParam,2,nchar(strTabixParam))
		if(substr(strTabixParam,nchar(strTabixParam),nchar(strTabixParam))=="'") strTabixParam = substr(strTabixParam,1,nchar(strTabixParam)-1)
		
		system(paste("tabix ",strTabixParam, " ", fileOut,sep=""))
		# tabix -s 1 -b 2 -e 2 -S 1 CLEAN.${study}.MetaScore.assoc.gz
	}
	
	if(blnWrite10rows) {
		write.table(objGWA@tblGWA[1:10,], paste(fileOut,".10rows",sep=""), row.names=F, quote=F, sep=strSep, na=strMissing, col.names=blnHeader)
	}
	if(blnWriteSnpList) {
		iMatch = match(colSnps, objGWA@aHeader)
		if(any(is.na(iMatch)))
			stop(paste("EASY ERROR:GWADATA.write\n SNP column to write \n ",colSnps," not available in\n ",objGWA@fileInShortName, "\n !!!", sep=""))

		sink(paste(fileOut,".snps",sep=""))
		cat(paste(objGWA@tblGWA[[iMatch]],collapse=strSnpListSep))
		sink()
	}
	
}

################################################################################################################################
################################################################################################################################
###### Wrapper for class setting
################################################################################################################################
##### Wrapper for constructing the object WITH validity checks
#GWADATA <- function(fileIn, fileInTag, strMissing, strSeparator, colMarker, acolPrimaryKey, aHeader, aClasses){ 
GWADATA <- function(){ 
	## Wrapper/constructor for class definition
	GWADATAout <- new("GWADATA")
	return(GWADATAout)

}

################################################################################################################################
################################################################################################################################
