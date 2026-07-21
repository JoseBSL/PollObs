**Code and data repository**

This repository contains the code and data required to reproduce all analyses and figures presented in the manuscript: **Seasonal Variation Mediates the Importance of Species Attributes in Plant-Pollinator Interactions**.

**Authors:** Jose B. Lanuza, Alfonso Allen-Perkins, Will Glenny, Anna Traveset, Henriette F. Morgenroth, Raymond Umazekabiri, Marc Hoffmann, Panagiotis Theodorou, Robert J. Paxton, Isabell Hensen, Robert Rauschkolb, Christine Römermann, Oliver Schweiger and Tiffany Knight.

**Reproducibility**: This repository uses the **renv** package to ensure a reproducible R environment. To recreate the software environment used in this study, run:

```r
install.packages("renv")
renv::restore()
```

This will install the package versions recorded in the `renv.lock` file.
