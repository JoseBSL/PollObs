This repository contains the R code and processed data required to reproduce all analyses and figures presented in the manuscript:

**Seasonal Variation Mediates the Importance of Species Attributes in Plant–Pollinator Interactions**

## Preprint

https://doi.org/10.64898/2026.07.21.739935

## Authors

Jose B. Lanuza, Alfonso Allen-Perkins, Will Glenny, Anna Traveset, Henriette F. Morgenroth, Raymond Umazekabiri, Marc Hoffmann, Panagiotis Theodorou, Robert J. Paxton, Isabell Hensen, Robert Rauschkolb, Christine Römermann, Oliver Schweiger, and Tiffany Knight.

## Reproducibility

This repository uses the **renv** package to recreate the R environment used in this study:

```r
install.packages("renv")
renv::restore()
```

Running `renv::restore()` installs the package versions recorded in `renv.lock`.

