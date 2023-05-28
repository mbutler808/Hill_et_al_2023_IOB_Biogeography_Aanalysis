# code

To understand the design of the code, open `biogeog.html` in a web browser for a walk-through of all of the code and figures. 

To reproduce all of the analyses and generated figures, run the R script `biogeog.R` first, followed by `tree_plotting_DEC.R` and `genera_thru_time.R`, or you can run the quarto file `biogeog.qmd`, which will run all of the scripts (in the .qmd it skips running the models to save 20 minutes, but you can disable this if you like). 

- `biogeog.qmd` : a quarto document that runs all of the scripts with added commentary. 
- `biogeog.html` : the document produced by running `biogeog.qmd`. Read this for a better walk-through and additional explanations.
- `biogeog.R` : the r script for running the BioGeoBEARS models, outputs to results folder: model fits (as an R object `hyp_fits.rds`), nodal probabilities from the best fit model (pies for plotting on phylogeny), summary statistics for all models.
  - within this script, there is an option for running the sensitivity analysis ***(WARNING this will take ~ 66.6 hours to complete)***. If you choose this option (set flag to TRUE), it will produce:
	- sensitivity_result_with_daic.csv: result summary of the sensitivity analysis containing AIC of best fit model, delta AIC from second best model.
	- out: directory that contains .csv for each of the trees in the analysis; models are ordered based AIC
- `tree_plotting_DEC.R`: plotting the ancestral reconstructions on the phylogeny of range transitions based on the best fit model (figure 4 in results)
- `genera_thru_time.R`:  plot figure 5, dispersal timings of all independent dispersal events across the phylogeny.

The remaining figures (1, 2, 3, 7) were made by hand using other software (ArcGIS, Adobe Illustrator, Keynote). 
