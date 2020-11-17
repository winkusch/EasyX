setClass("GWASCATALOGUE",
	representation = representation(
						strEqcCommand	=	"character",
						colInChr		=	"character",
						colInPos		=	"character",
						colOutAnnot		=	"character",
						numPosLim		=	"numeric",
						fileGwasCatalogue = "character",
						fileMap 		= "character",
						blnSimple = 	"logical",
						blnGwsPval = 	"logical",
						astrTraits = 	"character"
						),
	prototype = prototype(
						strEqcCommand	=	"",
						colInChr		=	"",
						colInPos		=	"",
						colOutAnnot		=	"AnnotTag",
						numPosLim		=	500000,
						fileGwasCatalogue = "",
						fileMap 		= "",
						blnSimple = 	FALSE,
						blnGwsPval = 	TRUE,
						astrTraits =    ""
						)
	#contains = c("EcfReader")
)

setGeneric("setGWASCATALOGUE", function(object) standardGeneric("setGWASCATALOGUE"))
setMethod("setGWASCATALOGUE", signature = (object = "GWASCATALOGUE"), function(object) {
	
	#aEqcSlotNamesIn = c("colInChr","colInPos","colInPval","numPvalLim","fileAnnot","colRefChr","colRefPos","strAnnotTag","colOutAnnot","numPosLim")
	aEqcSlotNamesIn = c("colInChr","colInPos","colOutAnnot","numPosLim","fileGwasCatalogue","fileMap","blnSimple","blnGwsPval","astrTraits")

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
validGWASCATALOGUE <- function(objGWASCATALOGUE) {
	
	if(objGWASCATALOGUE@colInChr == "") 
		stop(paste(" EASY ERROR:GWASCATALOGUE\n No column colInChr defined. Please set colInChr.", sep=""))
	if(objGWASCATALOGUE@colInPos == "") 
		stop(paste(" EASY ERROR:GWASCATALOGUE\n No column colInPos defined. Please set colInPos.", sep=""))
	if(objGWASCATALOGUE@fileGwasCatalogue == "") 
		stop(paste(" EASY ERROR:GWASCATALOGUE\n No reference file defined. Please set fileGwasCatalogue.", sep=""))
	if(objGWASCATALOGUE@fileMap == "") 
		stop(paste(" EASY ERROR:GWASCATALOGUE\n No reference file defined. Please set fileMap.", sep=""))
		
	### Valid with GWADATA?
	

	if(!file.exists(objGWASCATALOGUE@fileGwasCatalogue))
		stop(paste("EASY ERROR:GWASCATALOGUE\n File fileGwasCatalogue\n ",objGWASCATALOGUE@fileGwasCatalogue,"\n does not exist.", sep=""))
	
	if(!file.exists(objGWASCATALOGUE@fileMap))
		stop(paste("EASY ERROR:GWASCATALOGUE\n File fileMap\n ",objGWASCATALOGUE@fileMap,"\n does not exist.", sep=""))
	
	return(TRUE)
}
GWASCATALOGUE.GWADATA.valid <- function(objGWASCATALOGUE, objGWA) {
	
	isNotAv <- !(objGWASCATALOGUE@colInChr %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:GWASCATALOGUE\n Defined column colInChr \n",objGWASCATALOGUE@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isNotAv <- !(objGWASCATALOGUE@colInPos %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:GWASCATALOGUE\n Defined column colInPos \n",objGWASCATALOGUE@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	iPos = match(objGWASCATALOGUE@colInPos, objGWA@aHeader)
	isPosNumeric <- objGWA@aClasses[iPos] == "numeric" | objGWA@aClasses[iPos] == "integer" | objGWA@aClasses[iPos] == "double"

	if(!isPosNumeric)
		stop(paste(" EASY ERROR:GWASCATALOGUE\n Defined column colInPos \n",objGWASCATALOGUE@colInPos, "\n is not numeric for GWA data-set \n",objGWA@fileIn,"\n . Please cast colInPos to numeric, integer or double.", sep=""))
	
}
#############################################################################################################################
GWASCATALOGUE.run <- function(objGWASCATALOGUE, objGWA) {
	
	colInChr		=	objGWASCATALOGUE@colInChr
	colInPos		=	objGWASCATALOGUE@colInPos
	colOutAnnot		= 	objGWASCATALOGUE@colOutAnnot
	numPosLim		=	objGWASCATALOGUE@numPosLim
	fileGwasCatalogue = objGWASCATALOGUE@fileGwasCatalogue
	fileMap 		= objGWASCATALOGUE@fileMap
	
	blnSimple = objGWASCATALOGUE@blnSimple
	blnGwsPval = objGWASCATALOGUE@blnGwsPval
	astrTraits = objGWASCATALOGUE@astrTraits
	#### 
	
	tcat<-read.csv(objGWASCATALOGUE@fileGwasCatalogue, header=T, stringsAsFactors=FALSE)
	
	tcat$rsid = unlist(lapply(strsplit(tcat$Variant.and.risk.allele,"-"),function(x) x[1]))
	
	if(blnGwsPval) tcat = tcat[tcat$P.value<=5e-8, ]
	
	if(astrTraits!="") {
		atraits = strsplit(astrTraits,";")[[1]]
		atraits = gsub("'","",atraits,fixed=T)
		isok = tcat$Reported.trait %in% atraits
		tcat = tcat[isok, ]
	}
	
	## add chr, pos to gwas catalogue data-set
	tmap<-fread(objGWASCATALOGUE@fileMap, header=T, stringsAsFactors=FALSE, data.table=F)
		
	tblLoci = merge(tcat, tmap, by = "rsid", all = F)
	colRefChr = "chr"
	colRefPos = "pos"
	colRefTag1 = "First.Author"
	colRefTag2 = "Reported.trait"
	
	numLoci = dim(tblLoci)[1]
	
	aInChr = GWADATA.getcol(objGWA, colInChr)
	aInPos = GWADATA.getcol(objGWA, colInPos)
	
	aRefChr = tblLoci[, which(names(tblLoci) == colRefChr)]
	aRefPos = tblLoci[, which(names(tblLoci) == colRefPos)]
	aRefTag = paste(tblLoci[, which(names(tblLoci) == colRefTag1)], "(", tblLoci[, which(names(tblLoci) == colRefTag2)], ")",sep = "")
	
	aTagOut = rep(NA, dim(objGWA@tblGWA)[1]) # not used before
	
	for(i in 1:numLoci) {
		chrTmp = aRefChr[i]
		posTmp = aRefPos[i]
		
		print(paste("GWASCATALOGUE annotation step ",i,"of",numLoci, " ... "))
		
		isCurrentLocus = aInChr == chrTmp & abs(aInPos - posTmp) <= numPosLim
		isCurrentLocus[is.na(isCurrentLocus)] = FALSE
		
		if(blnSimple) {
			tagTmp = "y"
			if(any(isCurrentLocus)) {
				aTagOut[isCurrentLocus] = tagTmp
			}
		} else {
			tagTmp = aRefTag[i]

			if(any(isCurrentLocus)) {
				if(any(!is.na(aTagOut[isCurrentLocus]))) {
					# isNa = is.na(aTagOut[isCurrentLocus])
					isAvail = unlist(lapply(strsplit(aTagOut[isCurrentLocus],";"),function(x) any(x==tagTmp)))
					## NA, TRUE or FALSE
					aTagOut[isCurrentLocus] = ifelse(is.na(isAvail), tagTmp, ifelse(isAvail, aTagOut[isCurrentLocus], paste(aTagOut[isCurrentLocus],tagTmp, sep=";")))
				} else {
					aTagOut[isCurrentLocus] = tagTmp
				}
			}
		}
	}
	
	objGWA <- GWADATA.cbind(objGWA, aTagOut, colOutAnnot, blnOverwrite=TRUE)
	
	return(objGWA)
}

GWASCATALOGUE <- function(strEqcCommand){ 
	## Wrapper for class definition
	GWASCATALOGUEout <- setGWASCATALOGUE(new("GWASCATALOGUE", strEqcCommand = strEqcCommand))
	validGWASCATALOGUE(GWASCATALOGUEout)
	#GWASCATALOGUEout.valid <- validGWASCATALOGUE(GWASCATALOGUEout)
	return(GWASCATALOGUEout)
}

