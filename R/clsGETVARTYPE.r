setClass("GETVARTYPE",
	representation = representation(
						strEqcCommand		=	"character",
						colInCpaid			=	"character",
						colOut			=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInCpaid			=	"",
						colOut				=	"VARTYPE"
						)
	#contains = c("EcfReader")
)

setGeneric("setGETVARTYPE", function(object) standardGeneric("setGETVARTYPE"))
setMethod("setGETVARTYPE", signature = (object = "GETVARTYPE"), function(object) {
	
	aEqcSlotNamesIn = c("colInCpaid", "colOut")
	#aEcfSlotNamesIn = c("arcdEditCol", "acolEdit")

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
validGETVARTYPE <- function(objGVT) {

	return(TRUE)
}

GETVARTYPE.GWADATA.valid <- function(objGVT, objGWA){ 

	if(!(objGVT@colInCpaid %in% objGWA@aHeader))
		stop(paste("EASY ERROR:GETVARTYPE\n Column colInCpaid\n",objGVT@colInCpaid," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
	
}

#############################################################################################################################
GETVARTYPE.run <- function(objGVT, objGWA, objREPORT, isValidScript) {

	colInCpaid 	<- objGVT@colInCpaid
	colOut 		<- objGVT@colOut
	
	cpaid = GWADATA.getcol(objGWA,colInCpaid)
	
	lsSplit = strsplit(cpaid,":")
	# chr <- unlist(lapply(lsSplit, function(x) x[1]))
	# pos <- unlist(lapply(lsSplit, function(x) x[2]))
	# chrpos = paste(chr,pos,sep=":")
	# isDuplicatedPos = duplicated(chrpos)
	# if(any(isDuplicatedPos)) isDuplicatedPos = chrpos%in%chrpos[isDuplicatedPos]
	
	alleles <- unlist(lapply(lsSplit, function(x) x[3]))
	rm(lsSplit)
	
	lsAllSplit = strsplit(alleles,"_")
	a1 <- toupper(unlist(lapply(lsAllSplit, function(x) x[1])))
	a2 <- toupper(unlist(lapply(lsAllSplit, function(x) x[2])))
	rm(alleles)
	rm(lsAllSplit)
	
	vartype = rep(NA, length(cpaid))
	### 
	
	## count SNPs
	isSnp = a1%in%c("A","C","G","T") & a2%in%c("A","C","G","T")
	vartype[isSnp] <- "SNP"
	
	## count INDELs
	isBothMissing <- (a1=="NA"|a1=="-"|a1==".") & (a2=="NA"|a2=="-"|a2==".")
	isIndelOneMissing <- !isBothMissing & ((a1=="NA"|a1=='<DEL>'|a1=='-') | (a2=="NA"|a2=='<DEL>'|a2=='-'))
	isIndelRID <- (a1=="R"&(a2=="D"|a2=="I")) | (a2=="R"&(a1=="D"|a1=="I"))
	isIndelAllID <- (a1%in%c("A","C","G","T")&(a2=="D"|a2=="I")) | (a2%in%c("A","C","G","T")&(a1=="D"|a1=="I"))
	# isIndelSeq <- nchar(a1)!=nchar(a2) & a1!="NA" & a2!="NA" & !grepl("<",a1) & !grepl("<",a2)
	## update v2:
	isIndelSeq <- (nchar(a1)!=nchar(a2) | (nchar(a1)>1 & nchar(a2)>1)) & a1!="NA" & a2!="NA" & !grepl("<",a1) & !grepl("<",a2)
	if(any(isIndelSeq)) {
		idxPotentialIndelSeq = which(isIndelSeq)
		isSeqA1 = unlist(lapply(strsplit(a1[isIndelSeq],""),function(x) all(x%in%c("A","C","G","T","N"))))
		isSeqA2 = unlist(lapply(strsplit(a2[isIndelSeq],""),function(x) all(x%in%c("A","C","G","T","N"))))
		isIndelSeq[idxPotentialIndelSeq[which(!(isSeqA1&isSeqA2))]] <- FALSE
	}
	vartype[isIndelOneMissing|isIndelRID|isIndelAllID|isIndelSeq] <- "INDEL"
	
	## count other types using <INS:ALE:BU> or <CN0>
	isOtherA1 = grepl("<",a1)
	vartype[isOtherA1] <- a1[isOtherA1]
	isOtherA2 = grepl("<",a2)
	vartype[isOtherA2] <- a2[isOtherA2]
	vartype[isOtherA1|isOtherA2] <- "OTHER"
	
	objREPORT <- REPORT.addval(objREPORT,"GVT.SNP",length(which(isSnp)))
	objREPORT <- REPORT.addval(objREPORT,"GVT.Indel",length(which(isIndelOneMissing|isIndelRID|isIndelAllID|isIndelSeq)))
	objREPORT <- REPORT.addval(objREPORT,"GVT.Indel_SEQ",length(which(isIndelSeq)))
	objREPORT <- REPORT.addval(objREPORT,"GVT.Indel_RID",length(which(isIndelRID)))
	objREPORT <- REPORT.addval(objREPORT,"GVT.Indel_OneAlleleMissing",length(which(isIndelOneMissing)))
	objREPORT <- REPORT.addval(objREPORT,"GVT.Indel_AllID",length(which(isIndelAllID)))
	objREPORT <- REPORT.addval(objREPORT,"GVT.OtherType",length(which(isOtherA1|isOtherA2)))
	
	##################
	##################
	
	isNotExtractable <- !(isSnp|isIndelOneMissing|isIndelRID|isIndelAllID|isIndelSeq|isOtherA1|isOtherA2)
	objREPORT <- REPORT.addval(objREPORT,"GVT.NotExtractable",length(which(isNotExtractable)))
	
	if(any(isNotExtractable) & isValidScript) {
		objGWA.notextractable <- GWADATA.getrows(objGWA, which(isNotExtractable))
		GWADATA.write(objGWA.notextractable, strSuffix = paste(".GVT.NotExtractable",sep=""))
	}
		
	lsOut <- list(objGWA, objREPORT)
	
	return(lsOut)
}

GETVARTYPE <- function(strEqcCommand){ 
	## Wrapper for class definition
	GETVARTYPEout <- setGETVARTYPE(new("GETVARTYPE", strEqcCommand = strEqcCommand))
	validGETVARTYPE(GETVARTYPEout)
	return(GETVARTYPEout)	
}
