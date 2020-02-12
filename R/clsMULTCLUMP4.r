setClass("MULTCLUMP",
	representation = representation(
						strEqcCommand		=	"character",
						arcdCriterion		=	"character",
						acolMClump			=	"character",
						astrMClumpTag		=	"character"
						# colInMarker			=	"character",
						# numPvalLim			=	"numeric",
						# numPosLim			=	"numeric",
						# numR2Lim			=	"numeric",
						# filePLINK			=	"character",
						# fileBfile			=	"character",
						# blnAddClumpInfo 	= 	"logical",
						# strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						arcdCriterion		=	"",
						acolMClump			=	"",
						astrMClumpTag		=	""
						# colInMarker			=	"",
						# numPvalLim			=	5e-8,
						# numPosLim			=	500000,
						# numR2Lim			=	0.2,
						# filePLINK			=	"",
						# fileBfile			=	"",
						# blnAddClumpInfo 	= 	FALSE,
						# strTag				=	""
						),
	contains = c("CLUMP")
)

setGeneric("setMULTCLUMP", function(object) standardGeneric("setMULTCLUMP"))
setMethod("setMULTCLUMP", signature = (object = "MULTCLUMP"), function(object) {
	
	aEqcSlotNamesIn = c("arcdCriterion","acolMClump","astrMClumpTag","colInMarker","numPosLim","numR2Lim","filePLINK","fileBfile","blnAddClumpInfo","blnRemovePlinkFiles","strTag")

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
validMULTCLUMP <- function(objMULTCLUMP) {
	
	### Valid with GWADATA?
	
	if(objMULTCLUMP@arcdCriterion[1] == "") 
		warning(paste("EASY WARNING:MULTCLUMP\n No criterion arcdCriterion defined. All data will be used for clumping. Buffering may need some time!" ,sep="" ))
		#cat(paste(" EASY WARNING:MULTCLUMP\n No criterion arcdCriterion defined. All data will be used for clumping.", sep=""))
	if(objMULTCLUMP@colInMarker == "") 
		stop(paste(" EASY ERROR:MULTCLUMP\n No column colInMarker defined. Please set colInMarker.", sep=""))
	if(objMULTCLUMP@acolMClump[1] == "") 
		stop(paste(" EASY ERROR:MULTCLUMP\n No column acolMClump defined. Please set acolMClump.", sep=""))
	if(objMULTCLUMP@astrMClumpTag[1] == "") 
		stop(paste(" EASY ERROR:MULTCLUMP\n No astrMClumpTag defined. Please set astrMClumpTag.", sep=""))
		
		
	if(!file.exists(objMULTCLUMP@filePLINK))
		stop(paste("EASY ERROR:MULTCLUMP\n File filePLINK\n ",objMULTCLUMP@filePLINK,"\n does not exist.", sep=""))

	if(!file.exists(paste(objMULTCLUMP@fileBfile,".bed",sep="")))
		stop(paste("EASY ERROR:MULTCLUMP\n File fileBfile\n ",objMULTCLUMP@fileBfile,"\n does not exist.", sep=""))
	
	return(TRUE)
}
MULTCLUMP.GWADATA.valid <- function(objMULTCLUMP, objGWA) {
	
	isAv <- objMULTCLUMP@colInMarker %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:MULTCLUMP\n Defined column colInMarker \n",objMULTCLUMP@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	aisMatch = objMULTCLUMP@acolMClump %in% objGWA@aHeader
	if(any(!aisMatch))
		stop(paste("EASY ERROR:MULTCLUMP\n Column \n",objMULTCLUMP@acolMClump[which(!aisMatch)[1]]," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	return(TRUE)
}

#############################################################################################################################
MULTCLUMP.run <- function(objMULTCLUMP, objGWA, objREPORT,isValidScript) {

	arcdCriterion 	<- objMULTCLUMP@arcdCriterion
	acolMClump 		<- objMULTCLUMP@acolMClump
	astrMClumpTag	<- objMULTCLUMP@astrMClumpTag
	colInMarker 	<- objMULTCLUMP@colInMarker
	##Use everything that survives the criteria:
	numPvalLim 		<- 1 
	## numPvalLim 		<- objMULTCLUMP@numPvalLim
	numPosLim 		<- objMULTCLUMP@numPosLim
	numR2Lim 		<- objMULTCLUMP@numR2Lim
	filePLINK 		<- objMULTCLUMP@filePLINK
	fileBfile 		<- objMULTCLUMP@fileBfile
	blnRemovePlinkFiles <- objMULTCLUMP@blnRemovePlinkFiles
	blnAddClumpInfo	<- objMULTCLUMP@blnAddClumpInfo
	strTag 			<- objMULTCLUMP@strTag
	
	##### 

	if(isValidScript) {
		fileBasePlink = paste(objGWA@pathOut,"/",objGWA@fileInShortName,sep="")
		fileBasePlink = sub(".txt","",fileBasePlink)
		fileBasePlink = sub(".gz","",fileBasePlink)
		if(nchar(strTag)>0) fileBasePlink = paste(fileBasePlink,".",strTag,sep="")
		
		nFields = length(arcdCriterion)
		afileInPlink <- c()
		
		aSNPsMeetingAnyCriterion <- c()
		
		for(iField in 1:nFields) {
			rcdCrit = arcdCriterion[iField]
			colClump = acolMClump[iField]
			strClumpTag <- astrMClumpTag[iField]
			
			objRCD 	<- RCD(rcdCrit)
			out 	<- RCD.eval(objRCD, objGWA)
			
			objGWA.clumpin <- GWADATA.getrows(objGWA, which(out))
			is_na = is.na(GWADATA.getcol(objGWA.clumpin, colClump))
			numClumpNA = length(which(is_na))
			objGWA.clumpin@tblGWA <- objGWA.clumpin@tblGWA[!is_na,]
			
			objGWA.clumpin <- GWADATA.getcols(objGWA.clumpin, c(colInMarker,colClump))
			objGWA.clumpin <- GWADATA.renamecol(objGWA.clumpin, colInMarker, "SNP")
			objGWA.clumpin <- GWADATA.renamecol(objGWA.clumpin, colClump, "P")
			
			aSNPsMeetingAnyCriterion <- c(aSNPsMeetingAnyCriterion,GWADATA.getcol(objGWA.clumpin, "SNP"))
			
			fileInPlinkTmp	<- paste(fileBasePlink ,".",strClumpTag,".plink_in",sep="")
			write.table(objGWA.clumpin@tblGWA, fileInPlinkTmp, row.names=F, quote=F, sep="\t")		
			
			afileInPlink <- c(afileInPlink,fileInPlinkTmp)
		}
		
		fileOutPlink	<- paste(fileBasePlink ,".plink_out",sep="")
		if(objGWA@blnOverwriteResults & file.exists(paste(fileOutPlink,".clumped",sep=""))) file.remove(paste(fileOutPlink,".clumped",sep=""))
		i = 1
		while(file.exists(paste(fileOutPlink,".clumped",sep=""))) {
			#fileOut <- paste(fileOutBase,".",i,".txt.gz",sep="")
			fileOutPlink <- paste(fileOutPlink,".",i,sep="")
			i = i + 1
		}
		
		#### PLINK clump
		## bp to kb
		numPosLim <- round(numPosLim/1000)
		
		plink_Call = paste(filePLINK,"--noweb","--bfile",fileBfile,"--clump",paste(afileInPlink,collapse=","),"--out",fileOutPlink,"--clump-kb",numPosLim, "--clump-r2",numR2Lim,"--clump-p1",numPvalLim,"--clump-p2",numPvalLim,"--clump-field P","--clump-verbose")
		system(plink_Call)
		
		## creates three files fileOutPlink.clumped, fileOutPlink.log, fileOutPlink.nosex

		vDir<-scan(file = paste(fileOutPlink,".clumped",sep=""), what=character(0), n = -1, sep = "\n",quiet=TRUE,blank.lines.skip=FALSE)
		
		strSplitClump = "------------------------------------------------------------------"

		iClumpChange = grep(strSplitClump, vDir)

		numClumps = length(iClumpChange)

		cat(paste("Found", numClumps, "independent clumps ... "))
		cat("\n")
		
		tblClumpAll 	= data.frame()
		tblClumpIndex 	= data.frame()
		
		cat("Reformatting PLINK output ... ")
		cat("\n")
		for(i in 1:numClumps) {
			
			if(i == 1) vDirClump = vDir[1:(iClumpChange[i] - 1)]
			else vDirClump = vDir[(iClumpChange[i-1] + 1):(iClumpChange[i] - 1)]
			
			for(j in 1:length(vDirClump)) while(grepl('  ',vDirClump[j])) vDirClump[j]=gsub('  ',' ', vDirClump[j]) 
			
			########### REWRITE TO BLOCKS DEFINED BY which(vDirClump)=="" !!!!!!
			## JUST DISTINGUISH BLOCKS -> DONE
			
			aiBlocks = which(vDirClump=="")
			
			for(j in 1:(length(aiBlocks)-1)) {
				vDirTemp = vDirClump[aiBlocks[j]:(aiBlocks[j+1])]
				vDirTemp = vDirTemp[vDirTemp!=""]
				
				if(grepl(" CHR F SNP BP P TOTAL NSIG S05 S01 S001 S0001", vDirTemp[1], fixed=T)) {
					vDirClumpIndex = vDirTemp
					g=strsplit(vDirClumpIndex," ")
					g=lapply(g,function(x) x[-1])
					d = data.frame(t(g[[2]]),stringsAsFactors=FALSE)
					names(d) = t(g[[1]])
					#d = cbind(d, "aLociTag" = i)
					d = cbind(d, "aLociTag" = i, "aTopHit" = i)
					
					### P will be added later via acolAdd!
					idxP = match("P", names(d))
					d = d[,-idxP]
					
					tblClumpIndex = rbind(tblClumpIndex, d)
				}
				
				if(grepl(" KB RSQ ALLELES F P ", vDirTemp[1], fixed=T)) {
					vDirTempPart2 = vDirClump[aiBlocks[j+1]:(aiBlocks[j+2])]
					vDirTempPart2 = vDirTempPart2[vDirTempPart2!=""]
					vDirClumpOut = c(vDirTemp,vDirTempPart2)
					vDirClumpOut = gsub("(INDEX) ","",vDirClumpOut,fixed=T)
					vDirClumpOut = vDirClumpOut[vDirClumpOut!=""]
					vDirClumpOut[1] = paste(" SNP",vDirClumpOut[1],sep="")
					
					#vDirClumpOut[1] = gsub("ANNOT", paste(strsplit(clump.annot,",")[[1]],collapse=" "), vDirClumpOut[1])
					#vDirClumpOut = gsub(", "," ",vDirClumpOut,fixed=TRUE)
					
					g=strsplit(vDirClumpOut," ")
					g=lapply(g,function(x) x[-1])
					
					for(k in 2:length(g)) {
						if(length(g[[k]]) != length(g[[1]])) {
							ag = g[[k-1]]
							ag[(length(ag)-length(g[[k]])+1):length(ag)] <- g[[k]]
							#ag[(length(ag)-1):length(ag)] <- g[[k]]
							g[[k]] <- ag
						}
					}
					
					d=data.frame()
					for(k in 2:length(g)) d = rbind(d, data.frame(t(g[[k]]),stringsAsFactors=FALSE))
					names(d) = t(g[[1]])
					aTopHit = rep(NA,dim(d)[1])
					aTopHit[1] = i
					d = cbind(d, "aLociTag" = i, "aTopHit" = aTopHit)
					
					### P will be added later via acolAdd!
					idxP = match("P", names(d))
					d = d[,-idxP]
					
					tblClumpAll = rbind(tblClumpAll, d)
						
				}	
			}
		}
		
		# tblOut <- tblOut[,-match(c("ALLELES","F"),names(tblOut))]

		tblClumpIndex <- tblClumpIndex[,c("SNP","F","aLociTag","aTopHit")]
		tblClumpIndex$KB <- 0
		tblClumpIndex$RSQ <- 1
		
		##tblClumpAll <- tblClumpAll[,-match(c("ALLELES","F"),names(tblClumpAll))]
		tblClumpAll <- tblClumpAll[,c("SNP","F","aLociTag","aTopHit","KB","RSQ")]
		## merge files (loci with only one SNP are not listed in tblClumpAll !!!
		## merge by SNP/aLociTag/aTopHit
		
		isSingleMarkerLocus = !tblClumpIndex$SNP%in%tblClumpAll$SNP
		
		tblClumpOut <- rbind(tblClumpAll,tblClumpIndex[which(isSingleMarkerLocus),])
		
		## Recode F column
		tblClumpOut$F = as.character(tblClumpOut$F)
		
		for(i in 1:length(astrMClumpTag)) {
			tblClumpOut$F[tblClumpOut$F == as.character(i)] <- astrMClumpTag[i]
		}

		# SNP   F aLociTag aTopHit
		# 1  rs1558902 BMI        1       1
		# 2  rs4784323 BMI        1      NA
		# 3  rs4784323 WHR        1      NA
		# 4  rs7206790 BMI        1      NA
		# 5  rs7206790 WHR        1      NA
		# 6  rs8047395 BMI        1      NA
		
		#############################################
		### Compile aCrossTraitLocusId, aCrossTraitLocusTag, aCrossTraitTopHit

		aCrossTraitLocusId <- aCrossTraitLocusTag <- aCrossTraitTopHit <-rep(NA,nrow(tblClumpOut))
		aLociTagUni <- unique(sort(tblClumpOut$aLociTag))
		
		for(i in 1:length(aLociTagUni)) {
			curLocustag <- aLociTagUni[i]
			isCurLocus <- tblClumpOut$aLociTag == curLocustag
			
			aCrossTraitLocusId[isCurLocus] <- i
			
			astrFtmp <- tblClumpOut$F[isCurLocus]
			astrFtmpUniSorted <- sort(unique(astrFtmp))
			aCrossTraitLocusTag[isCurLocus] <- paste(astrFtmpUniSorted,collapse=";")
			
			isCurLocusTopHit <- isCurLocus & !is.na(tblClumpOut$aTopHit)
			#aCrossTraitTopHit[isCurLocusTopHit] <- paste(astrFtmpUniSorted,collapse=";")
			aCrossTraitTopHit[isCurLocusTopHit] <- i
			
		}
		
		tblClumpOut <- cbind(tblClumpOut, aCrossTraitLocusId)
		tblClumpOut <- cbind(tblClumpOut, aCrossTraitLocusTag)
		tblClumpOut <- cbind(tblClumpOut, aCrossTraitTopHit)
		
		 # SNP   F aLociTag aTopHit aCrossTraitLocusId aCrossTraitLocusTag aCrossTraitTopHit
		# 1  rs1558902 BMI        1       1                  1             BMI;WHR	 BMI;WHR
		# 2  rs4784323 BMI        1      NA                  1             BMI;WHR	NA
		# 3  rs4784323 WHR        1      NA                  1             BMI;WHR	NA	
		# 4  rs7206790 BMI        1      NA                  1             BMI;WHR	NA
		# 5  rs7206790 WHR        1      NA                  1             BMI;WHR	NA

		#############################################
		### Remove duplicates
		
		tblClumpOut <- tblClumpOut[!duplicated(tblClumpOut$SNP),]
		
		#####
		tblClumpOut <- tblClumpOut[,c("SNP","aCrossTraitLocusId","aCrossTraitTopHit","aCrossTraitLocusTag","KB","RSQ")]
		
		if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
		
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numCrossTraitLoci",sep=""),max(aCrossTraitLocusId,na.rm=TRUE))
		#names(tblClumpOut)[2:7] <- paste(strTag,names(tblClumpOut)[2:7],sep="")
		names(tblClumpOut)[-1] <- paste(strTag,names(tblClumpOut)[-1],sep="")
		
		objGWA.clumpout <- GWADATA()
		objGWA.clumpout <- GWADATA.settbl(objGWA.clumpout,tblClumpOut)
		
		# iRename = which(names(objGWA.clumpout@tblGWA)%in%names(objGWA@tblGWA) & names(objGWA.clumpout@tblGWA)!="SNP")
		# if(length(iRename)>0) {
			# for(iRenameTmp in iRename)
				# objGWA.clumpout <- GWADATA.renamecol(objGWA.clumpout, names(objGWA.clumpout@tblGWA)[iRenameTmp], paste(names(objGWA.clumpout@tblGWA)[iRenameTmp],".ref",sep=""))
		# }
		
		isUsedForClumping <- GWADATA.getcol(objGWA, colInMarker)%in%aSNPsMeetingAnyCriterion
		objGWA.multclumpin <- GWADATA.getrows(objGWA, isUsedForClumping)
			
		objGWA.clump 	<- GWADATA.merge(objGWA.multclumpin, objGWA.clumpout, blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = colInMarker, strBy.Add = "SNP", strSuffix.In = "", strSuffix.Add ="" )
		### At this point, the P-Values are available in the data frame! Could add trait-specific top hits!
		
		### compile aCrossTraitTopTrait
		tblClumpTmp = as.data.frame(objGWA.clump@tblGWA)
		tblClumpTmp <- tblClumpTmp[,acolMClump]
		aCrossTraitTopTrait = rep(NA,nrow(tblClumpTmp))
		aCrossTraitTopHitTmp = GWADATA.getcol(objGWA.clump, paste(strTag,"aCrossTraitTopHit",sep=""))
		for(i in 1:nrow(tblClumpTmp)) {
			if(!is.na(aCrossTraitTopHitTmp[i])) {
				numRowMin = min(tblClumpTmp[i,],na.rm=T)
				aidxColRowMin = which(numRowMin == tblClumpTmp[i,])
				astrTopTrait = astrMClumpTag[aidxColRowMin]
				astrTopTraitSorted <- sort(unique(astrTopTrait))
				aCrossTraitTopTrait[i] <- paste(astrTopTraitSorted,collapse=";")
			}
		}
		objGWA.clump <- GWADATA.cbind(objGWA.clump, aCrossTraitTopTrait, paste(strTag,"aCrossTraitTopTrait",sep=""))
				
		objGWA.clump.x 	<- GWADATA.getrows(objGWA.clump, which(!is.na(GWADATA.getcol(objGWA.clump, paste(strTag,"aCrossTraitTopHit",sep="")))))
		objGWA.clump.nomatch <- GWADATA.getrows(objGWA.clump, which(is.na(GWADATA.getcol(objGWA.clump, paste(strTag,"aCrossTraitLocusId",sep="")))))	
	
		
		if(blnRemovePlinkFiles) {
			for(fileInPlink in afileInPlink) {
				file.remove(fileInPlink)
			}
			file.remove(paste(fileOutPlink,".clumped",sep=""))
			file.remove(paste(fileOutPlink,".hh",sep=""))
			file.remove(paste(fileOutPlink,".log",sep=""))
			file.remove(paste(fileOutPlink,".nof",sep=""))
		}
		
		if(blnAddClumpInfo) {
			### merges by intersect(names)		
			objGWA <- GWADATA.merge(objGWA,objGWA.clump, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = NA, strBy.Add = NA)
		}
		
	} else {
		objGWA.clump <- GWADATA.copy(objGWA)
		objGWA.clump.x <- GWADATA.copy(objGWA)
		objGWA.clump.nomatch <- GWADATA.copy(objGWA)
		
		strTag <- ifelse(strTag != "", paste(strTag,".",sep=""), strTag)
		objGWA.added <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag, "aCrossTraitLocusId",sep=""))
		objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)), paste(strTag, "aCrossTraitTopHit",sep=""))
		objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTag, "aCrossTraitLocusTag",sep=""))
		objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTag, "aCrossTraitTopTrait",sep=""))
		objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTag, "KB",sep=""))
		objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTag, "RSQ",sep=""))
		
		objGWA <- objGWA.added
		
		if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numCrossTraitLoci",sep=""),0)
	
	}
	
	return(list(objGWA, objGWA.clump,objGWA.clump.x,objGWA.clump.nomatch, objREPORT))

}

MULTCLUMP <- function(strEqcCommand){ 
	## Wrapper for class definition
	MULTCLUMPout <- setMULTCLUMP(new("MULTCLUMP", strEqcCommand = strEqcCommand))
	validMULTCLUMP(MULTCLUMPout)
	#MULTCLUMPout.valid <- validMULTCLUMP(MULTCLUMPout)
	return(MULTCLUMPout)
}

