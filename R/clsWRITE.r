setClass("WRITE",
	representation = representation(
						strEqcCommand	=	"character",
						strMode			=	"character",
						strPrefix		=	"character",
						strSuffix		=	"character",
						strSep			=	"character",
						strMissing		=	"character",
						strTabixParam	=	"character",
						blnHeader		=	"logical",
						blnChrWise		=	"logical",
						colChr			=	"character",
						blnChunkWise	=	"logical",
						colChunk		=	"character",
						acolOut			=	"character",
						blnWrite10rows	=	"logical",
						blnWriteSnpList	=	"logical",
						strSnpListSep	=	"character",
						colSnps			=	"character",
						pathOut			=	"character"
						),
	prototype = prototype(
						strEqcCommand	=	"",
						strMode			=	"txt",
						strPrefix		=	"",
						strSuffix		=	"",
						strSep			=	"TAB",
						strMissing		=	"NA",
						strTabixParam	=	"",
						blnHeader		=	TRUE,
						blnChrWise		=	FALSE,
						colChr			=	"",
						blnChunkWise	=	FALSE,
						colChunk		=	"",
						acolOut			=	"",
						blnWrite10rows	=	TRUE,
						blnWriteSnpList	=	FALSE,
						strSnpListSep	=	"\n",
						colSnps			=	"rsid",
						pathOut			=	""
						)
	#contains = c("EcfReader")
)

setGeneric("setWRITE", function(object) standardGeneric("setWRITE"))
setMethod("setWRITE", signature = (object = "WRITE"), function(object) {
	
	aEqcSlotNamesIn = c("strMode", "strPrefix","strSuffix", "strSep", "strMissing","strTabixParam","blnHeader","acolOut","blnChrWise","colChr","blnWrite10rows","blnWriteSnpList","strSnpListSep","colSnps","pathOut","blnChunkWise","colChunk")
	
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
validWRITE <- function(objWRITE) {
	
	if(all(objWRITE@strSep != c("TAB", "SPACE", "COMMA")))
		stop("EASY ERROR:\nWRITE\nWrong File separator defined\nPlease use TAB, WHITESPACE, SPACE or COMMA!")
	
	## Reset separator
	if(objWRITE@strSep == "TAB") objWRITE@strSep <- "\t"
	if(objWRITE@strSep == "SPACE") objWRITE@strSep <- " "
	if(objWRITE@strSep == "COMMA") objWRITE@strSep <- ","
	
	if(objWRITE@strSnpListSep == "TAB") objWRITE@strSnpListSep <- "\t"
	if(objWRITE@strSnpListSep == "SPACE") objWRITE@strSnpListSep <- " "
	if(objWRITE@strSnpListSep == "COMMA") objWRITE@strSnpListSep <- ","
	
	
	
	if(all(objWRITE@strMode != c("txt", "gz", "bgz")))
		stop("EASY ERROR:\nWRITE\nWrong Writing Mode strMode defined\nPlease use txt, gz or bgz!")
	
	if(objWRITE@blnChrWise & objWRITE@colChr=="")
		stop("EASY ERROR:\nWRITE: Please define --colChr when running WRITE with --blnChrWise 1!")
	
	if(objWRITE@blnChunkWise & objWRITE@colChunk=="")
		stop("EASY ERROR:\nWRITE: Please define --colChunk when running WRITE with --blnChunkWise 1!")
	
	if(objWRITE@pathOut != "") {
		
		objWRITE@pathOut <- ifelse(objWRITE@pathOut == "./", getwd(), objWRITE@pathOut)
		objWRITE@pathOut <- ifelse(objWRITE@pathOut == ".", getwd(), objWRITE@pathOut)
		
		if(!file.exists(objWRITE@pathOut) & objWRITE@pathOut!="NA") {
			stop(paste("EASY ERROR:\n WRITE: --pathOut ",objWRITE@pathOut,"\n does not exist!\n Please revise or remove to use default.", sep=""))
		}
	}
	
	return(objWRITE)
}

#############################################################################################################################
WRITE.run <- function(objWRITE, objGWA, objREPORT) {
	
	if(objWRITE@acolOut[1] != "") {
		
		if(any(!(objWRITE@acolOut%in%objGWA@aHeader)))
			stop(paste(" EASY ERROR:WRITE\n Defined acolOut columns \n",paste(objWRITE@acolOut[which(!(objWRITE@acolOut%in%objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
			
		objGWA <- GWADATA.getcols(objGWA, objWRITE@acolOut, blnSuppressError=TRUE)
	}
	
	if(objWRITE@pathOut != "") {
		objGWA@pathOut = objWRITE@pathOut
	}
	
	if(objWRITE@blnChrWise) {
		
		## workaround problem with reporting:
		for(chr in 1:22) objREPORT <- REPORT.addval(objREPORT,paste("numVarOut.chr",chr,sep=""),"NA")
		objREPORT <- REPORT.addval(objREPORT,paste("numVarOut.chrX",sep=""),"NA")
		## 
		
		aChr = GWADATA.getcol(objGWA,objWRITE@colChr)
		aChrUni = unique(aChr)
		
		for(chr in aChrUni) {
			aidxChr = which(aChr==chr & !is.na(aChr))
			if(length(aidxChr)>1) {
				objGWA.chr = GWADATA.getrows(objGWA,aidxChr)
				strSuffixChr = paste(objWRITE@strSuffix,".chr",chr,sep="")
				GWADATA.write(objGWA.chr, 
								strMode = objWRITE@strMode,
								strPrefix = objWRITE@strPrefix,
								strSuffix = strSuffixChr,
								strSep = objWRITE@strSep,
								strMissing = objWRITE@strMissing,
								strTabixParam = objWRITE@strTabixParam,
								blnHeader = objWRITE@blnHeader,
								blnWrite10rows = objWRITE@blnWrite10rows,
								blnWriteSnpList = objWRITE@blnWriteSnpList,
								strSnpListSep = objWRITE@strSnpListSep,
								colSnps = objWRITE@colSnps)
							
				# objREPORT <- REPORT.addval(objREPORT,paste("numVarOut.chr",chr,sep=""),nrow(objGWA@tblGWA))
				# objREPORT <- REPORT.setval(objREPORT,paste("numVarOut.chr",chr,sep=""),nrow(objGWA@tblGWA))
				objREPORT <- REPORT.setval(objREPORT,paste("numVarOut.chr",chr,sep=""),nrow(objGWA.chr@tblGWA))
			}
		}
	} else if(objWRITE@blnChunkWise) {
		
		aChunk = GWADATA.getcol(objGWA,objWRITE@colChunk)
		aChunkUni = unique(aChunk)
		
		## workaround problem with reporting:
		for(chunk in aChunkUni) objREPORT <- REPORT.addval(objREPORT,paste("numVarOut.chunk",chunk,sep=""),"NA")
				
		for(chunk in aChunkUni) {
			aidxChunk = which(aChunk==chunk & !is.na(aChunk))
			if(length(aidxChunk)>1) {
				objGWA.chunk = GWADATA.getrows(objGWA,aidxChunk)
				strSuffixChunk = paste(objWRITE@strSuffix,".chunk",chunk,sep="")
				GWADATA.write(objGWA.chunk, 
								strMode = objWRITE@strMode,
								strPrefix = objWRITE@strPrefix,
								strSuffix = strSuffixChunk,
								strSep = objWRITE@strSep,
								strMissing = objWRITE@strMissing,
								strTabixParam = objWRITE@strTabixParam,
								blnHeader = objWRITE@blnHeader,
								blnWrite10rows = objWRITE@blnWrite10rows,
								blnWriteSnpList = objWRITE@blnWriteSnpList,
								strSnpListSep = objWRITE@strSnpListSep,
								colSnps = objWRITE@colSnps)
							
				objREPORT <- REPORT.setval(objREPORT,paste("numVarOut.chunk",chunk,sep=""),nrow(objGWA.chunk@tblGWA))
			}
		}
	} else {
	
		GWADATA.write(objGWA, 
						strMode = objWRITE@strMode,
						strPrefix = objWRITE@strPrefix,
						strSuffix = objWRITE@strSuffix,
						strSep = objWRITE@strSep,
						strMissing = objWRITE@strMissing,
						strTabixParam = objWRITE@strTabixParam,
						blnHeader = objWRITE@blnHeader,
						blnWrite10rows = objWRITE@blnWrite10rows,
						blnWriteSnpList = objWRITE@blnWriteSnpList,
						strSnpListSep = objWRITE@strSnpListSep,
						colSnps = objWRITE@colSnps)
	}
	objREPORT <- REPORT.setval(objREPORT,"numVarOut",nrow(objGWA@tblGWA))
	
	return(objREPORT)
}

WRITE <- function(strEqcCommand){ 
	## Wrapper for class definition
	WRITEout <- setWRITE(new("WRITE", strEqcCommand = strEqcCommand))
	#validWRITE(WRITEout)
	WRITEout.valid <- validWRITE(WRITEout)
	return(WRITEout.valid)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

# setValidity("WRITE", function(object){
	# print("WRITE-CHECK")
	
	
	
	# print(TRUE)
	# return(TRUE)
# })

