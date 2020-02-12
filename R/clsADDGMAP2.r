setClass("ADDGMAP",
	representation = representation(
						strEqcCommand	= "character",
						rcdCriterion	= "character",
						colInChr		= "character",
						colInPos		= "character",
						fileGmap		= "character",
						colGmapChr		= "character",
						colGmapPos		= "character",
						colGmapGpos		= "character",
						strTag			= "character"
						),
	prototype = prototype(
						strEqcCommand	= "",
						rcdCriterion	= "",
						colInChr		= "",
						colInPos		= "",
						fileGmap		= "",
						colGmapChr		= "chr",
						colGmapPos		= "position",
						colGmapGpos		= "Genetic_Map.cM.",
						strTag			= ""
						)
	#contains = c("EcfReader")
)

setGeneric("setADDGMAP", function(object) standardGeneric("setADDGMAP"))
setMethod("setADDGMAP", signature = (object = "ADDGMAP"), function(object) {
	
	#aEqcSlotNamesIn = c("colInChr","colInPos","colInPval","numPvalLim","fileAnnot","colRefChr","colRefPos","strAnnotTag","colOutAnnot","numPosLim")
	aEqcSlotNamesIn = c("rcdCriterion","colInChr","colInPos","fileGmap","colGmapChr","colGmapPos","colGmapGpos","strTag")

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
validADDGMAP <- function(objADDGMAP) {
	
	if(objADDGMAP@rcdCriterion == "") 
		cat(paste(" EASY WARNING:ADDGMAP\n No criterion rcdCriterion defined. All data will be used for gene adding.", sep=""))
	
	if(objADDGMAP@colInChr == "") 
		stop(paste(" EASY ERROR:ADDGMAP\n No column colInChr defined. Please set colInChr.", sep=""))
	if(objADDGMAP@colInPos == "") 
		stop(paste(" EASY ERROR:ADDGMAP\n No column colInPos defined. Please set colInPos.", sep=""))
	
	if(objADDGMAP@fileGmap == "" | !file.exists(objADDGMAP@fileGmap)) {
			stop(paste("EASY ERROR:ADDGMAP\n File fileGmap\n ",objADDGMAP@fileGmap,"\n does not exist.\n Please check definition of --fileGmap.", sep=""))
	}
	
	return(TRUE)
}

ADDGMAP.GWADATA.valid <- function(objADDGMAP, objGWA) {
	
	isNotAv <- !(objADDGMAP@colInChr %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:ADDGMAP\n Defined column colInChr \n",objADDGMAP@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isNotAv <- !(objADDGMAP@colInPos %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:ADDGMAP\n Defined column colInPos \n",objADDGMAP@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	return(TRUE)
	
}
ADDGMAP.read <- function(objADDGMAP, blnReadAll) {
	
	fGm <- objADDGMAP@fileGmap
	
	if(blnReadAll) {
		tblG = read.table(fGm, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "")
	} else {
		tblG = read.table(fGm, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", nrows = 10)
	}
	
	if(dim(tblG)[1]==0)
		stop(paste("EASY ERROR:ADDGMAP\n There are no rows available in \n",fGm,"\n The file is empty!!!\n", sep=""))
	
	if(!(objADDGMAP@colGmapChr%in%names(tblG))) 
		stop(paste(" EASY ERROR:ADDGMAP\n Column colGmapChr \n",objADDGMAP@colGmapChr," \n cannot be found in fileGmap \n", fGm,".", sep=""))
	
	if(!(objADDGMAP@colGmapPos%in%names(tblG)))
		stop(paste(" EASY ERROR:ADDGMAP\n Column colGmapPos \n",objADDGMAP@colGmapPos," \n cannot be found in fileGmap \n", fGm,".", sep=""))
	
	if(!(objADDGMAP@colGmapGpos%in%names(tblG))) 
		stop(paste(" EASY ERROR:ADDGMAP\n Column colGmapGpos \n",objADDGMAP@colGmapGpos," \n cannot be found in fileGmap \n", fGm,".", sep=""))
	
	
	tblGShort = tblG[,c(objADDGMAP@colGmapChr,objADDGMAP@colGmapPos,objADDGMAP@colGmapGpos)]
	
	names(tblGShort) <- c("chr","pos","gmap_pos_cM")
	
	# objADDGMAP@tblGmap <- tblGShort
	
	return(tblGShort)
}


#############################################################################################################################
ADDGMAP.run <- function(objADDGMAP, objGWA, tblGmap) {
	
	rcdCriterion	= objADDGMAP@rcdCriterion
	colInChr		= objADDGMAP@colInChr
	colInPos		= objADDGMAP@colInPos
	fileGmap		= objADDGMAP@fileGmap
	# tblGmap         = objADDGMAP@tblGmap
	strTag			= objADDGMAP@strTag
	
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	
	objRCD 	= RCD(rcdCriterion)
	out 	= RCD.eval(objRCD, objGWA)
	numCrit = length(which(out))
	objGWA.addgmap <- GWADATA.getrows(objGWA, which(out))
	
	if(numCrit == 0) {
		objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"gpos",sep=""))
		return(objGWA)
	}
	
	aChr 		= GWADATA.getcol(objGWA.addgmap, colInChr)
	aPos 		= GWADATA.getcol(objGWA.addgmap, colInPos)
	
	aGmap_pos_cM <- rep(NA,length(aPos))
	
	lsGmap = list()
	# for(chr in 1:22) lsGmap = c(lsGmap, tblGmap[tblGmap$chr==chr,])
	for(chr in 1:22) lsGmap = c(lsGmap, list(tblGmap[tblGmap$chr==chr,]))
	
	print("Starting interpolation ... ")
	.timeinterStart <- Sys.time()
	
	for(i in 1:length(aChr)) {
		
		chr=aChr[i]
		pos=aPos[i]
		
		tblGmapChr = lsGmap[[as.integer(chr)]]
		
		isIdentical = tblGmapChr$pos==pos
		
		if(any(isIdentical)) {
			idxAvail = which(tblGmapChr$pos==pos)[1]
			aGmap_pos_cM[i] = tblGmapChr$gmap_pos_cM[idxAvail]
			
		} else {
			
			isLow = tblGmapChr$pos<pos
			isUpp = tblGmapChr$pos>pos
			
			isInterpolate = any(isLow) & any(isUpp)
			if(isInterpolate) {
				idxlow = max(which(isLow),na.rm=T)
				idxupp = min(which(isUpp),na.rm=T)
				
				pos1 = tblGmapChr$pos[idxlow]
				pos2 = tblGmapChr$pos[idxupp]
				cm1 = tblGmapChr$gmap_pos_cM[idxlow]
				cm2 = tblGmapChr$gmap_pos_cM[idxupp]
				
				aGmap_pos_cM[i] = ((pos-pos1)/(pos2-pos1))*(cm2-cm1)+cm1
			} else {
				## set to min
				
				isSetMin = all(isUpp)
				if(isSetMin) { 
					aGmap_pos_cM[i] = tblGmapChr$gmap_pos_cM[1]
				} else {
					isSetMax = all(isLow)
					if(isSetMax) {
						aGmap_pos_cM[i] = tblGmapChr$gmap_pos_cM[nrow(tblGmapChr)]
					} else {
						aGmap_pos_cM[i] = NA
					}
				}
			}
		
		}
	}
	
	.timeinterStop <- Sys.time()
	
	objGWA.addgmap = GWADATA.cbind(objGWA.addgmap, aGmap_pos_cM, paste(strTag,"gpos",sep=""))
	
	objGWA <- GWADATA.merge(objGWA,objGWA.addgmap, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = NA, strBy.Add = NA)
	
	return(objGWA)
}

ADDGMAP <- function(strEqcCommand){ 
	## Wrapper for class definition
	ADDGMAPout <- setADDGMAP(new("ADDGMAP", strEqcCommand = strEqcCommand))
	validADDGMAP(ADDGMAPout)
	#ADDGMAPout.valid <- validADDGMAP(ADDGMAPout)
	return(ADDGMAPout)
}

