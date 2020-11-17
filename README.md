# EasyX (= EasyQC + EasyStrata)

R package for GWAS quality control (= EasyQC) and for evaluation of stratified GWAS (= EasyStrata). 

## Description

The EasyX R package combines the full functionality of two other R packages: 
- EasyQC (Quality control for GWAS, [Winkler et al. Nat Protoc 2014](https://pubmed.ncbi.nlm.nih.gov/24762786/))
- EasyStrata (Stratified GWAS evaluation, [Winkler et al. Bioinformatics 2015](https://pubmed.ncbi.nlm.nih.gov/25260699/)) 
</ul>
Any functions exported from EasyQC and EasyStrata are also available through EasyX.    
More information as well as template scripts and reference files can be found at [www.genepi-regensburg.de/software](https://www.genepi-regensburg.de/software). 

### Dependencies

EasyX depends on the following other R packages:  
- Cairo, plotrix, data.table, forestplot 
</ul>
These need to be installed prior to the installation of EasyX. 

### Installing

We recommend to use 'devtools' in R to install the package directly from github:  
```
library(devtools)
install_github("winkusch/EasyX")
```
Alternatively, you can download the tarball [EasyX_1.0.tar.gz](https://homepages.uni-regensburg.de/~wit59712/easyx/EasyX_1.0.tar.gz) and install it in R: 
```
install.packages("EasyX_1.0.tar.gz")
```

### Executing program

To start the program, you will have to load the package in R and call an ecf-script by the EasyX function: 
```
library(EasyX)
EasyX("/path2script/template.ecf")
```
Please check our website [www.genepi-regensburg.de/software](https://www.genepi-regensburg.de/software) for ecf-script templates. 
## Help

Please contact thomas.winkler@klinik.uni-regensburg.de if you need assistance. 

## Authors

Contributors names and contact info:  
Thomas Winkler ([@winkusch](https://twitter.com/winkusch))

## Version History

* 1.0.0
    * Initial Commit
	* basically a combination of EasyQC-v23.8 and EasyStrata-v18.1

## Citation

If you are using the package for GWAS quality control, please cite [Winkler et al. Nat Protoc 2014](https://pubmed.ncbi.nlm.nih.gov/24762786/).   
If you are using the package for (stratified) GWAS results evaluation, please cite [Winkler et al. Bioinformatics 2015](https://pubmed.ncbi.nlm.nih.gov/25260699/). 
