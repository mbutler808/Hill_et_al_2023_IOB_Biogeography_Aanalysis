# biogeography
[![DOI](https://zenodo.org/badge/646599963.svg)](https://zenodo.org/badge/latestdoi/646599963)

## This repository contains the analysis for "Testing geology with biology: Plate tectonics and the diversification of microhylid frogs in the Papuan region"

## History

2023-06-01 First release with data, code, results, and etc folders.  

## Software requirements

This repository requires use of R, Quarto, Github and a reference manager for bibtex. A plain text editor is also necessary. 

## Repository structure

The description of the directory structure is as follows (Please see the `README.md` files in each folder for more details):

* All data is in the `data` folder or is referenced as a link to a publicly available file.
* All code is in the `code` folder. An explanation of all of the code is in the quarto file `biogeog.qmd`, and rendered for easy reading in `biogeog.html` (open the .html in a browser).
* All results (figures, tables, computed values) are saved into the `results` folder.
* Accessory files needed for the bibliography and styles are in the `etc` folder.
* The `LaTeX` file for the manuscript and its component parts are in the `manuscript` folder.
* See the various `README.md` files in those folders for some more information.

	
### Reproducing the analysis

1. Run `biogeog.R` from the code folder
2. Run `tree_plotting_DEC.R` from the code folder (Figure 4)
3. Run `genera_thru_time.R` from the code folder (Figure 5)

### Citations

(This study) __Hill E.C., Gao D.F., Polhemus, D.A., Fraser C.J., Iova B., Allison A., and  Butler M.A. (in press)__ Testing geology with biology: Plate tectonics and the diversification of microhylid frogs in the Papuan region.  _Integrative Organismal Biology_.  

__Hill E.C., Fraser C.J., Gao D.F., Jarman M., Henry E.R., Iova B., Allison A., and  Butler M.A. (2022)__ Resolving the deep phylogeny: Implications for Early Adaptive Radiation, Cryptic, and Present-day Ecological Diversity of Papuan Microhylid Frogs.  _Molecular Phylogenetics and Evolution_. <https://doi.org/10.1016/j.ympev.2022.107618>

__Hill E.C., Jarman M., Fraser C.J., Gao D.F., Henry E.R., Fisher, A.R., Iova B., Allison A., and  Butler M.A. (2023)__ Molecular and phylogenetic datasets for the Asterophryinae frogs of New Guinea with additional data on lifestyle, geography, and elevation. _Data in Brief_. <https://doi.org/10.1016/j.dib.2023.108987>

