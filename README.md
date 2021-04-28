## Budding Yeast Coevolutionary Network

<p align="center">
  <img width="400" height="400" src="https://github.com/JLSteenwyk/budding_yeast_coevolution_network/blob/main/www/community_network.png">
</p>

Gene coevolution—which refers to gene pairs whose evolutionary rates covary across speciation events—is often observed among functionally related genes. We present a comprehensive gene coevolution network inferred from the examination of nearly three million gene pairs from 332 budding yeast species spanning ~400 million years of eukaryotic evolution. Modules within the network provide insight into cellular and genomic structure and function, such as genetic pleiotropy, genes functioning in distinct cellular compartments, vesicle transport, and DNA replication. 

To facilitate others being able to use information encoded in the budding yeast coevolutionary network, we built a shiny web application that allows researchers to obtain subnetworks of interest using lists of genes of interest.

## Quick Start

### 1) Installing dependencies
Function of this app requires the shiny and igraph R packages. To install these packages, execute the following commands within the R environment.
```R
install.packages("shiny", dep=T)
install.packages("igraph", dep=T)
```
This application has been tested using<br />
• R version v4.0.2 (2020-06-22) -- "Taking Off Again"<br />
• igraph_1.2.6<br />
• shiny_1.6.0<br />

### 2) Opening up the shiny app
After installing dependencies run the shiny app by executing the following command in R
```R
shiny::runGitHub("budding_yeast_coevolution_network", "JLSteenwyk")
```

### 3) Input list of gene identifiers
To obtain a subnetwork of interest, click the "Browse..." button and select a file that has a single column of genes of interest. Genes of interest can be specified using their <i>Saccharomyces cerevisiae</i>, <i>Candida albicans</i>, or orthologous group identifier from [Shen et al. (2018), Cell](https://jlsteenwyk.com/publication_pdfs/2018_Shen_et_al_Cell.pdf). For example, the file should look like the following if using <i>S. cerevisiae</i> identifiers:
```
YNL262W
YPR175W
YKL113C
YLR274W
...
```

If using <i>C. albicans</i> identifiers, the file should look like the following:
```
CR_09900C_A
CR_04560C_A
C2_06250C_A
C3_04300C_A
...
```

If using orthologous group identifiers from [Shen et al. (2018), Cell](https://jlsteenwyk.com/publication_pdfs/2018_Shen_et_al_Cell.pdf), the file should look like the following:
```
OG1737
OG1863
OG1874
OG1893
...
```

Exemplary lists are found in the <i>example_list</i> directory within this GitHub repository.

A plot will appear with the network with the nodes of interest. Below the network and to the left will be a plot of the degree (or number of coevolving genes) for a given gene. Below the network and to the left will be a plot of the cumulative frequency of node degress (or the cumulative number of coevolving genes per gene) in the network.

### 4) Modify the network
To facilitate further exploration of the network, various functions (i.e., buttons and sliders) are provided on the left side of the screen.

Explanations of each function are available below the submit button.

Changes to the network can be implemented with the <i>Submit</i> button or will be done automatically.

### 5) Download results
To download the pdf of the network and information provided below the network use the <i>Download Plot</i> button. To download coevolution coefficients between each gene pair, use the <i>Download Data Frame</i> button. To download information about the number of degrees (or coevolving genes per gene), the gene identifiers for each gene in the network, the community to which each gene belongs to, use the <i>Download Node Information</i> button.
<br /><br />

## Citation
If you found the <i>Budding Yeast Coevolutionary Network</i> useful in your research, please cite<br />
Steenwyk JL, <i>et al.</i> 2021. ARTICLE NAME. JOURNAL. [Link](https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-019-4577-5)<br />

