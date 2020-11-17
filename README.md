# EasyX (= EasyQC + EasyStrata)

R package for GWAS quality control (= EasyQC) and for evaluation of stratified GWAS (= EasyStrata). 

## Description

The EasyX R package combines the full functionality of two other R packages: 
- EasyQC (Quality control for GWAS, [Winkler et al. Nat Protoc 2014](https://pubmed.ncbi.nlm.nih.gov/24762786/))
- EasyStrata (Stratified GWAS evaluation, [Winkler et al. Bioinformatics 2015](https://pubmed.ncbi.nlm.nih.gov/25260699/))  
Any functions exported from EasyQC and EasyStrata are also available through EasyX.   
More information as well as template scripts and reference files can be found at [www.genepi-regensburg.de/software](https://www.genepi-regensburg.de/software). 

### Dependencies

EasyX depends on the following other R packages:  
- Cairo, plotrix, data.table, forestplot   
These need to be installed prior to the installation of EasyX. 

### Installing

We recommend to use 'devtools' ion R to install the package directly from github:  
```
install_github("winkusch/EasyX")
```

### Executing program

* To start the program, you will have to load the library and call an ecf-script by the EasyX function: 
```
library(EasyX)
EasyX("/path2script/template.ecf")
```
Please check our website [www.genepi-regensburg.de/software](https://www.genepi-regensburg.de/software) for ecf-script templates. 
## Help

Please contact thomas.winkler@klinik.uni-regensburg.de if you need assistance. 

## Authors

Contributors names and contact info:  
Thomas Winkler [@winkusch](https://twitter.com/winkusch)

## Version History

* 1.0.0
    * Initial Commit

## Citation

If you are using the package for GWAS quality control, please cite [Winkler et al. Nat Protoc 2014](https://pubmed.ncbi.nlm.nih.gov/24762786/). 
If you are using the package for (stratified) GWAS results evaluation, please cite [Winkler et al. Bioinformatics 2015](https://pubmed.ncbi.nlm.nih.gov/25260699/). 
