# Immuno-Oncology (IO) Explorer

A collection of R Shiny applications for exploring clinical multi-omic datasets from patients treated with Immune Checkpoint Inhibitors.

# Study Design 
All data are from clinical trial CA209-038 of Advanced Melanoma patients treated with Nivolumab with tumor biopsies collected for sequencing before and after Nivolumab treatment to observe genomic changes on immunotherapy. Roughly half the cohort had previously been treated with Ipilimumab and saw their cancer progress while on that treatment, the rest of the cohort had never been treated with Ipilimumab. Dataset includes 142 samples (either pre or on-Nivoluman) from 80 patients. Genomic data includes somatic mutations, RNA-seq and T-cell Receptor sequencing as well as various metrics derived from these data types (e.g. Tumor Mutational Burden (TMB), Mutational Signatures, Tumor Clonality,  Gene Set Enrichment Analysis (GSEA), etc))
## Directory Structure

### 'bms038/'
Primary application and data for visualizing all major analyses.

### 'TCR/'
Proof of concept application and data for visualizing T-cell receptor sequencing data.

### 'beeswarm/'
Proof of concept application and data for visualizing T-cell receptor sequencing data.


# Installation
1. Install R
```
sudo apt update -y
sudo apt install -y r-base
sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev
```
2. Install R Shiny Server from (https://posit.co/download/shiny-server/)
```
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
sudo gdebi shiny-server-1.5.22.1017-amd64.deb
```
3. Install Linux packages needed by specific R packages
```
# Install "Cairo" which is needed for R package of the same name
sudo apt install -y libcairo2-dev

# Install CMAKE to enable installation of R package 'ggpubr'
sudo apt install cmake
```

3. Install R packages
```
sudo R # 
# Inside R session:
install.packages(c("shiny", "ggplot2", "plotly","survival","GGally","grid","scales","DT","gridExtra","RColorBrewer","Cairo","ks", "forestplot","htmltools","bsplus","openxlsx","ggpubr","rlang","pheatmap","ggthemes"))

# Install GSVA package from BioConductor (still inside R session)
# NOTE: this can take longer than all previous steps combined (~45-90 minutes) and is only necessary for use of the Gene Expression widget
# Still in R
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("GSVA")

# Exit R session 
quit()
```

# Citation
Please cite Riaz et al. Tumor and Microenvironment Evolution during Immunotherapy with Nivolumab, Cell (2017), https://doi.org/10.1016/j.cell.2017.09.028 
