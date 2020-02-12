setClass("CREATECPTID",
	representation = representation(
						strEqcCommand		=	"character",
						colInMarker			=	"character",
						colInA1				=	"character",
						colInA2				=	"character",
						colInChr			=	"character",
						colInPos			=	"character",
						blnUseInPos			=	"logical",
						fileMap				=	"character",
						tblMap				=	"data.table",
						colMapMarker		=	"character",
						colMapChr			=	"character",
						colMapPos			=	"character",
						blnUseMap			=	"logical",
						strTag				=	"character",
						blnUseInMarker		=	"logical",
						blnUseFastRead		=	"logical"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInMarker			=	"",
						colInA1				=	"",
						colInA2				=	"",
						colInChr			=	"",
						colInPos			=	"",
						blnUseInPos			=	FALSE,
						fileMap				=	"",
						tblMap				=	data.table(),
						colMapMarker		=	"rsmid",
						colMapChr			=	"chr",
						colMapPos			=	"pos",
						blnUseMap			=	FALSE,
						strTag				=	"",
						blnUseInMarker		=	TRUE,
						blnUseFastRead		=	TRUE
						)
)

CREATECPTID.set <- function(strEqcCommand, objCPT, objGWA.default) {

	objCPT@blnUseFastRead <- objGWA.default@blnUseFastRead
	
	aEqcSlotNamesIn = c("colInMarker","colInA1","colInA2","fileMap","colMapMarker","colMapChr","colMapPos","strTag","colInChr","colInPos","blnUseInMarker","blnUseFastRead")
	
	## astrPatterns
	## *_CHR_POS
	## chrCHR:POS
	
	### Last 4 are inherited from class GWADATA and can be used with CREATECPTID for reference file!
	
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

# setGeneric("setCREATECPTID", function(object) standardGeneric("setCREATECPTID"))
# setMethod("setCREATECPTID", signature = (object = "CREATECPTID"), function(object, objGWADATA.default) {
	
	# object@blnUseFastRead <- objGWADATA.default@blnUseFastRead
	
	# aEqcSlotNamesIn = c("colInMarker","colInA1","colInA2","fileMap","colMapMarker","colMapChr","colMapPos","strTag","colInChr","colInPos","blnUseInMarker","blnUseFastRead")
	
	# ## astrPatterns
	# ## *_CHR_POS
	# ## chrCHR:POS
	
	# ### Last 4 are inherited from class GWADATA and can be used with CREATECPTID for reference file!
	
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
validCREATECPTID <- function(objCPT) {
	
	if(objCPT@fileMap != "") {
		
		objCPT@blnUseMap <- TRUE

		if(!file.exists(objCPT@fileMap))
			stop(paste("EASY ERROR:CREATECPTID\n Mapping file \n ",objCPT@fileMap,"\n does not exist!!!\n", sep=""))
	}
	
	if(objCPT@colInMarker == "") {
		objCPT@blnUseInMarker <- FALSE
	}
		
	if(objCPT@colInChr != "" & objCPT@colInPos != "") 
		objCPT@blnUseInPos = TRUE
	
	return(objCPT)

}

CREATECPTID.GWADATA.valid <- function(objCPT, objGWA) {
	
	if(objCPT@blnUseInMarker | objCPT@blnUseMap) {
		isAv <- objCPT@colInMarker %in% objGWA@aHeader
		if(!isAv)
			stop(paste(" EASY ERROR:CREATECPTID\n Defined column colInMarker \n",objCPT@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
			
		## colInMarker is not required when format is got from chrIn posIn
	}
	
	isAv <- objCPT@colInA1 %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPTID\n Defined column colInA1 \n",objCPT@colInA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isAv <- objCPT@colInA2 %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPTID\n Defined column colInA2 \n",objCPT@colInA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	if(objCPT@blnUseInPos) {
		isAv <- objCPT@colInChr %in% objGWA@aHeader
		if(!isAv)
			stop(paste(" EASY ERROR:CREATECPTID\n Defined column colInChr \n",objCPT@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct --colInChr OR remove --colInchr and --colInPos.", sep=""))
	
		isAv <- objCPT@colInPos %in% objGWA@aHeader
		if(!isAv)
			stop(paste(" EASY ERROR:CREATECPTID\n Defined column colInPos \n",objCPT@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct --colInPos OR remove --colInchr and --colInPos.", sep=""))
	}
	
}

CREATECPTID.read <- function(objCPT, blnReadAll, blnUseFastRead) {
	
	fileMap <- objCPT@fileMap
	
	blnUseFastRead<-NA
	blnUseFastRead <- objCPT@blnUseFastRead
	
	if(blnReadAll) {
		cat(paste("   + Reading ",fileMap, "... \n"))
		if(!blnUseFastRead) {
			objCPT@tblMap 	<- as.data.table(read.table(fileMap, header=T, sep = "\t", stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = "character"))
		} else {
			
			if(tolower(substring(fileMap,nchar(fileMap)-2,nchar(fileMap)))==".gz") strFread <- paste("zcat ",fileMap,sep="")
			else strFread <- fileMap
					
			objCPT@tblMap 	<- tryCatch(
				#fread(strFread, sep="\t", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character","character","integer")),
				fread(strFread, sep="\t", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character","character","character")),
				error = function(err) {
					strError = err$message
					val=strsplit(strError,"'",fixed=T)[[1]][length(strsplit(strError,"'",fixed=T)[[1]])]
					g=scan(file = fileMap, what=character(0), n = -1,sep = "\n",quiet=TRUE)
					iRow = which(grepl(paste(val,"\t",sep=""),g,fixed=T) | grepl(paste(val,"\n",sep=""),g,fixed=T))[1]
					stop(paste(strError,"\n EASY ERROR:\n Cannot read '",val,"' from row '",iRow,"' !!!\n ", sep=""))
				}
			)
		}
	} else {
		objCPT@tblMap = as.data.table(read.table(fileMap, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", colClasses = "character", sep = "\t", nrows = 10))
		
		if(names(objCPT@tblMap)[1]!=objCPT@colMapMarker) 
			stop(paste(" EASY ERROR:CREATECPTID\n First column in \n",objCPT@fileMap, "\n is not named \n",objCPT@colMapMarker, "\n Please correct colMapMarker or fileMap.", sep=""))
		if(names(objCPT@tblMap)[2]!=objCPT@colMapChr) 
			stop(paste(" EASY ERROR:CREATECPTID\n Second column in \n",objCPT@fileMap, "\n is not named \n",objCPT@colMapChr, "\n Please correct colMapChr or fileMap.", sep=""))
		if(names(objCPT@tblMap)[3]!=objCPT@colMapPos) 
			stop(paste(" EASY ERROR:CREATECPTID\n Third column in \n",objCPT@fileMap, "\n is not named \n",objCPT@colMapPos, "\n Please correct colMapPos or fileMap.", sep=""))
	}
	
	if(dim(objCPT@tblMap)[1]==0)
		stop(paste("EASY ERROR:CREATECPTID\n There are no rows available in \n",fileMap,"\n The file is empty!!!\n", sep=""))
	
	if(dim(objCPT@tblMap)[2]!=3)
		stop(paste("EASY ERROR:CREATECPTID\n --fileMap \n",fileMap,"\n must contain three columns !!!\n", sep=""))
	
	
	isAv <- objCPT@colMapMarker %in% names(objCPT@tblMap)
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPTID\n Defined column colMapMarker \n",objCPT@colMapMarker, "\n is not available in fileMap. PLease specify correct column name.", sep=""))
	
	isAv <- objCPT@colMapChr %in% names(objCPT@tblMap)
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPTID\n Defined column colMapChr \n",objCPT@colMapChr, "\n is not available in fileMap. PLease specify correct column name.", sep=""))
	
	isAv <- objCPT@colMapPos %in% names(objCPT@tblMap)	
	if(!isAv)
		stop(paste(" EASY ERROR:CREATECPTID\n Defined column colMapPos \n",objCPT@colMapPos, "\n is not available in fileMap. PLease specify correct column name.", sep=""))
	
	return(objCPT)
}
#############################################################################################################################
CREATECPTID.run <- function(objCPT, objGWA, objREPORT, isValidScript) {
	
	colInMarker <- objCPT@colInMarker
	colInA1 	<- objCPT@colInA1
	colInA2 	<- objCPT@colInA2
	strTag 		<- objCPT@strTag
	colMapMarker <- objCPT@colMapMarker
	colMapChr 	<- objCPT@colMapChr
	colMapPos 	<- objCPT@colMapPos
	colInChr 	<- objCPT@colInChr
	colInPos 	<- objCPT@colInPos
	blnUseInPos 	<- objCPT@blnUseInPos
	blnUseInMarker 	<- objCPT@blnUseInMarker
	blnUseMap 		<- objCPT@blnUseMap
	
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
		
	chr <- pos <- cptid <- rep(NA,nrow(objGWA@tblGWA))
		
 ###### 1.Try to get chr/pos from map (if fileMap defined)
	if(objCPT@blnUseMap) {
		## merge chr pos
		idxMatch = match(objGWA@tblGWA[[colInMarker]], objCPT@tblMap[[colMapMarker]])
		idxMatchIn = which(!is.na(idxMatch))
		chr[idxMatchIn] <- objCPT@tblMap[[colMapChr]][idxMatch[idxMatchIn]]
		pos[idxMatchIn] <- objCPT@tblMap[[colMapPos]][idxMatch[idxMatchIn]]
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.frommap",sep=""),length(idxMatchIn))
		rm(idxMatch)
		rm(idxMatchIn)
	}
	
	if(blnUseInMarker) {
		
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.from_chrpos_format",sep=""),0)
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.from_cb_format",sep=""),0)
		
		idxNaChr = which(is.na(chr))
		if(length(idxNaChr)>0) {
			###### 2.Try to get chr/pos from chr1:123:AT_T
		
			## try to extract chr, pos from "chr1:123:AT_T" or "1:1234:fewe" or "1:1234"
			marker <- objGWA@tblGWA[[colInMarker]][idxNaChr]
			## remove chr
			marker <- gsub("chr","",marker)
			## split by : to obtain 1:123:whatever
			idx2Colon = which(grepl(":", marker, fixed=TRUE))
			if(length(idx2Colon)>0) {
				marker <- marker[idx2Colon]
				lsMarkerSplit = strsplit(marker, ":", fixed=TRUE)
				chrSplit = unlist(lapply(lsMarkerSplit,function(x) x[1]))
				posSplit = unlist(lapply(lsMarkerSplit,function(x) x[2]))
				is3Use = chrSplit%in%c(as.character(1:22),"X")
				idx3Use = which(is3Use & !is.na(suppressWarnings(as.integer(posSplit))))
				if(length(idx3Use)>0) {
					chr[idxNaChr[idx2Colon[idx3Use]]] <- chrSplit[idx3Use]
					pos[idxNaChr[idx2Colon[idx3Use]]] <- posSplit[idx3Use]
					#objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.from_chrpos_format",sep=""),length(idx3Use))
					objREPORT <- REPORT.setval(objREPORT,paste(strTag,"CPTID.from_chrpos_format",sep=""),length(idx3Use))
				}
				rm(chrSplit,posSplit,lsMarkerSplit,idx3Use,is3Use)
			}
			rm(idx2Colon,marker)

			## recheck NEXT FORMAT
			idxNaChr = which(is.na(chr))
			if(length(idxNaChr)>0) {
				###### 3.Try to get chr/pos from c1b123SNP
			
				## try to extract chr, pos from "c1b123INDEL" or "c1b123SNP"
				marker <- objGWA@tblGWA[[colInMarker]][idxNaChr]
				
				idx2cbf = which(grepl("c([1-9])|([1-2][0-9])|[x|X]b",marker) & grepl("INDEL$|SNP$",marker))
				## INDEL$ true when blablaINDEL , false when blaINDELbla
				## [1-9]OR[1-2][0-9]OR[x|X] true when blablaINDEL , false when blaINDELbla
				
				if(length(idx2cbf)>0) {
					## c12b12345INDEL
					marker <- marker[idx2cbf]
					marker <- gsub("INDEL","",marker)
					marker <- gsub("SNP","",marker)
					## c12b12345
					marker <- gsub("c","",marker)
					## 12b12345
					lsMarkerSplit = strsplit(marker, "b", fixed=TRUE)
					chrSplit = unlist(lapply(lsMarkerSplit,function(x) x[1]))
					posSplit = unlist(lapply(lsMarkerSplit,function(x) x[2]))
					
					is3Use = chrSplit%in%c(as.character(1:22),"X")
					idx3Use = which(is3Use & !is.na(suppressWarnings(as.integer(posSplit))))
					if(length(idx3Use)>0) {
						chr[idxNaChr[idx2cbf[idx3Use]]] <- chrSplit[idx3Use]
						pos[idxNaChr[idx2cbf[idx3Use]]] <- posSplit[idx3Use]
						#objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.from_cb_format",sep=""),length(idx3Use))
						objREPORT <- REPORT.setval(objREPORT,paste(strTag,"CPTID.from_cb_format",sep=""),length(idx3Use))
					}
					rm(chrSplit,posSplit,lsMarkerSplit,idx3Use,is3Use)
				}
				rm(idx2cbf,marker)
			}
		}
		rm(idxNaChr)
	}
	#####
	## recheck other format ? 
	## TBI
	#####
	if(blnUseInPos) {
		
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.from_chr_pos",sep=""),0)
		###### 4.Try to get chr/pos from input chr/pos
		idxNaChr = which(is.na(chr))
		if(length(idxNaChr)>0) {
			idx2Use = which(!is.na(objGWA@tblGWA[[colInChr]][idxNaChr])&!is.na(objGWA@tblGWA[[colInPos]][idxNaChr]))
			chr[idxNaChr[idx2Use]] <- as.character(objGWA@tblGWA[[colInChr]][idxNaChr[idx2Use]])
			pos[idxNaChr[idx2Use]] <- as.character(objGWA@tblGWA[[colInPos]][idxNaChr[idx2Use]])
			#objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.from_chr_pos",sep=""),length(idx2Use))
			objREPORT <- REPORT.setval(objREPORT,paste(strTag,"CPTID.from_chr_pos",sep=""),length(idx2Use))
			rm(idx2Use)
		}
		rm(idxNaChr)
	}
	
	# isMatch = !is.na(chr) & !is.na(pos) & ((objGWA@tblGWA[[colInA1]] %in% c("A","C","G","T") & objGWA@tblGWA[[colInA2]] %in% c("A","C","G","T")) | (objGWA@tblGWA[[colInA1]] %in% c("I","D") & objGWA@tblGWA[[colInA2]] %in% c("I","D")))
	isMatch = !is.na(chr) & !is.na(pos)
	type = ifelse(objGWA@tblGWA[[colInA1]] %in% c("I","D") & objGWA@tblGWA[[colInA2]] %in% c("I","D"), ":ID", "")
	cptid[isMatch] <- paste(chr[isMatch],":",pos[isMatch],type[isMatch],sep="")
	
	if(any(!isMatch) & blnUseInMarker) {
		### set to previous id
		cptid[!isMatch] <- objGWA@tblGWA[[colInMarker]][!isMatch]
	}
	
	objGWA <- GWADATA.cbind(objGWA, cptid, "cptid")
	
	isScientific <- grepl("e+",cptid,fixed=TRUE)
	if(any(isScientific)) {
		
		objGWA.scientific <- GWADATA.copy(objGWA)
		objGWA.scientific <- GWADATA.getrows(objGWA, which(isScientific))
		GWADATA.write(objGWA.scientific, strSuffix = paste(".",strTag,"CPTID.scientific",sep=""))
		stop(paste(" EASY ERROR:CREATECPTID\n Some of the created cptids use a scientific format (see file *.CPTID.scientific.txt in the output)! \n",
					"Please make sure that the position column --colInPos or --colMapPos from the input or the mapping file do NOT contain a scientific format for position of the affected markers.", 
					sep=""))
	}
	rm(cptid)
	
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.nomatch",sep=""),length(which(!isMatch)))
	if(any(!isMatch) & isValidScript) {
		objGWA.nomatch <- GWADATA.copy(objGWA)
		objGWA.nomatch <- GWADATA.getrows(objGWA, which(!isMatch))
		GWADATA.write(objGWA.nomatch, strSuffix = paste(".",strTag,"CPTID.nomatch",sep=""))
		rm(objGWA.nomatch)
	}
	
	isAlleleOk <- ((objGWA@tblGWA[[colInA1]] %in% c("A","C","G","T") & objGWA@tblGWA[[colInA2]] %in% c("A","C","G","T")) | (objGWA@tblGWA[[colInA1]] %in% c("I","D") & objGWA@tblGWA[[colInA2]] %in% c("I","D")))
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"CPTID.allelemismatch",sep=""),length(which(!isAlleleOk)))
	if(any(!isAlleleOk) & isValidScript) {
		objGWA.allelemismatch <- GWADATA.copy(objGWA)
		objGWA.allelemismatch <- GWADATA.getrows(objGWA, which(!isAlleleOk))
		GWADATA.write(objGWA.allelemismatch, strSuffix = paste(".",strTag,"CPTID.allelemismatch",sep=""))
		rm(objGWA.allelemismatch)
	}
	
	rm(isMatch)
	rm(isAlleleOk)
	
	return(list(objGWA,objREPORT))
}
#############################################################################################################################
CREATECPTID <- function(strEqcCommand, objGWA.default){ 
	## Wrapper for class definition
	CREATECPTIDout <- new("CREATECPTID")
	CREATECPTIDout <- CREATECPTID.set(strEqcCommand, CREATECPTIDout,objGWA.default)
	CREATECPTIDout<-validCREATECPTID(CREATECPTIDout)
	
	return(CREATECPTIDout)

}
