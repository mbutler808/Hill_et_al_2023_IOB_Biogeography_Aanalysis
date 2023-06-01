# results

- models: directory where the outputs of the models are stored; mainly .csv and .rds files of the raw BioGeoBEARS output and 
	- `modelfittable.csv` table containing dAIC, LnL, df, disperal, and extinction rates for all of the models. 
	- `hyp_fits.rds` a list of all of the model-fits from BioGeoBEARS
	- `best_fit.rds` The BioGeoBEARS modelfit output for the best model
	- `prob_pies_13.csv` The georegion probabilities for each node of the phylogeny, for producing pie insets on Fig. 4
	- `modelfit_sensitivity.csv` best models selected in the sensitivity analysis
- Figures: 
	- h_DEC.pdf the pie plot phyogeny output from BioGeoBEARS
	- Fig4_DEC.pdf: plot of phylogeny with ancestral terranes produced by best DEC fit model
	- Fig5_plot.pdf: central data plot of figure 5, put together with maps and annotations in illustrator. This plot shows the timing of the inferred dispersal events with error bars for the nodes at which range transitions occur. 
