# Hierarchical Clustering Transition Index (hcti) - v1.0

### Thanks for your interest in using hcti.

This software was initially presented in the manuscript "Country Transition Index Based on Hierarchical Clustering to Predict Next COVID-19 Waves" (submitted to Scientific Report -- under review)

An online system with our analyses is available at https://covid19-usp.neodados.com/.


### About the manuscript findings

COVID-19 has widely spread around the world, impacting the health systems of several countries in addition to the collateral damage that societies will face in the next years. 

From the COVID-19 dataset by the Johns Hopkins University Center for Systems Science and Engineering, we present a temporal analysis on the number of new cases and deaths among countries using artificial intelligence. Our approach incrementally models the cases using a hierarchical clustering that emphasizes country transitions between infection groups over time.

<img align="right" src="images/cov-clust.png">

### About the transition index

The transition index is suitable for any dataset and is applied on dendogram produced by hierachical clustering. In summary, it is used to extract information from dendrogram clades. When used along with multiple clustering execution (e.g. temporal data), it allows to track element movements among clusters.

<img align="right" src="images/hcti.png">

The organization of the material is:

> - **/dataset** - contains the datasets to reproduce our findings and create the figures.
> - **/src** - contains the source code for the machine learning framework and for other analyses.
> - **/results** - you can find the pre-trained classification models in this folder.
> - **/workdir** - please execute the code when you are inside this directory.

To reproduce all experiments using individual ML classification models on the training dataset, please run the source codes:

```Prolog
Rscript src/ml/prediction-visualization.R 
Rscript src/ml/prediction-visualization-aug.R
```

You can also open R and run:

```Prolog
source("src/ml/prediction-visualization.R")
```

```Prolog
source("src/ml/prediction-visualization-aug.R")
```

To reproduce our figures and results, we recommend to run all codes from the workdir directory. For example:

```Prolog
cd workdir/

Rscript ../src/ml/make_heatmap_predictions.R
```

You can also open R, from the workdir directory, and run:

```Prolog
source("../src/ml/make_heatmap_predictions.R")
```

If you find any issues with the code, please contact us: tiago-jose@ncchd.go.jp, ricardoar@ufba.br, tatiane.nogueira@ufba.br, mello@icmc.usp.br

On the behalf of all of the authors, we appreciate your interest in Hema-Class and hope it is useful to your research.
