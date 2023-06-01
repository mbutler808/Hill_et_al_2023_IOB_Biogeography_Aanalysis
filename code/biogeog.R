################################################################################
# biogeog.R  Runs models of biogeographical evolution on Asterophryinae frogs
# Analyses published in 
#	Hill et al (2023) Testing geology with biology: Plate tectonics 
# 	and the diversification of microhylid frogs in the Papuan region. 
# 	Integrative Organismal Biology. In press
################################################################################

## ---- packagesDEC --------
library(BioGeoBEARS)
	# installed from https://github.com/nmatzke/BioGeoBEARS/tree/master
	# version number 1.1.3
library(treeio)  
library(ape)

## ---- functions --------
### Load functions to run BioGeoBEARS models
source("biogeog_functions.R")

## ---- comment1 --------
##  Contains the Functions:
##   biogeo_obj <- function( trfn, geogfn,  dispersal_multipliers_fn = NA, 
##                         timesfn = NA, areas_allowed_fn = NA )
##   "%w/o%" <- function(x,y) { x[ !(x %in% y) ]}
##   logLik.bgb <- function( x ) 
##   AIC.bgb <- function( x, df =2 ) 
##   hypothesis_list <- function( hyp_dm, hyp_ts, hyp_un, tree, geo=geo, dispersal=dispersal, 
##                              timeperiods=timeperiods, areasallowed=arreasallowed )
##   model_fit_table <- function( fit )

###############################################
## ARGUMENTS all are file names with paths
## name used in script = BioGeoBEARS name: 
# tree = trfn = tree in nexus format
# geo = geogfn = geographic data, in PHYLIP format 
# disperal = dispfn = dispersal_multipliers_fn = dispersal multiplier matrix 
# timeperiods = timesfn = time stratification matrix 
# areasallowed = areasfn = areas_allowed_fn = areas allowed matrix 

## ---- sensitivity --------
### Do we want to run sensitivity analysis on 100 best trees? 
run_sensitivity_analysis = FALSE  # Will take ~30 hours of run time
								  
## ---- inputs ----
#######################
# Gather the inputs
#######################

# The tree comes from Hill et al 2022 and Hill et al 2023
# and is converted to phylo format for use with BioGeoBEARS
treefile = "https://github.com/mbutler808/Asterophryinae_phylogenetic_data/blob/main/BEAST2_analysis/beast_218_tree_asterophryinae.nex?raw=TRUE"
tree <- read.beast(treefile) %>% as.phylo
write.tree(tree, "../data/biogeo.tree")

treefile = "../data/biogeo.tree"
geo = "../data/geobins.txt"
hdat = read.csv("../data/hypotheses.csv")

hypotheses <- hdat$hypothesis
names(hypotheses) <- hdat$id

## ---- model_groups ----
hyp_un <- hdat$id[ hdat$dispersal_multipliers=="" ] # unconstrained
hyp_ts <- hdat$id[ hdat$time_periods!="" ]  # time-stratified
hyp_dm <- hdat$id[ !hdat$id %in% hyp_un ]  # with dispersal matrices (includes ts)

## ---- model_input_vectors ----
# paths to model inputs
dispersal <- paste("../data/dispersal_matrices/", 
                   hdat$dispersal_multipliers, 
                   ".txt", 
                   sep="")
timeperiods <- paste("../data/time_periods/", 
                     hypotheses, 
                     ".txt", 
                     sep="")
areasallowed <- paste("../data/areas_allowed/", 
                      hypotheses, 
                      ".txt", 
                      sep="" )
names(dispersal) <- names(areasallowed) <- names(timeperiods) <- names(hypotheses)

# remove empty filenames, just to be safe
dispersal <- dispersal[hyp_dm]
timeperiods <- timeperiods[hyp_ts]
areasallowed <- areasallowed[hyp_ts]

## ---- create_BioGeoBEARS_objects ----
##############################################
## Create BioGeoBEARS input objects 
##############################################
# inputs are vectors created above

h <- hypothesis_list( hyp_dm,   # names of dispersal multiplier hypotheses
					  hyp_ts,   #		   time-stratified hypotheses
					  hyp_un,   # 		   unconstrained hypotheses
					  treefile, 	# paths to treefile
					  geo, 		# paths to geobins file
					  dispersal, # paths to dispersal matrices files
					  timeperiods, # paths to time periods files
					  areasallowed # paths to areas allowed files
					)

## ---- run_models ----
#######################
# Run the models
#######################

# run all the models
fit <- lapply( h, bears_optim_run )

# run one model at a time
#  fit1 <- bears_optim_run(hyp[["a"]])
#  fit2 <- bears_optim_run(hyp[["b"]])

## ---- model_stats ----
##############################################
# Collect the model fit statistics
##############################################

scores <- model_fit_table(fit)
best_id <- scores$id[1]
best_fit <- fit[[best_id]]
scores

## ---- save_modelfits ----
##############################################
# Save model fit results
##############################################

saveRDS( 
         fit, 
         file="../results/models/hyp_fits.rds"
       ) # model fits as R objects
saveRDS(
         best_fit, 
         file="../results/models/best_fit.rds"
       )
write.csv(
           scores, 
           "../results/models/modelfittable.csv", 
           row.names = F, quote = F
         ) # scores, parameters



## ---- comment1 ----
#######################
# Consolidate Probabilities 
# for pie charts showing range transitions
# We have run models allowing dual-area occupancy,
# the smallest number possible to still run BioGeoBEARS.
# Since these are low-disperal species and 
# occupy only one area at a time, we
# consolidate dual area probabilities to single areas
#######################

## ---- pie_probs ----
statenames <- names(
                best_fit$inputs$list_of_dispersal_multipliers_mats[[1]]
              ) # vector of georegion codes
probs <- as.data.frame(
              best_fit$ML_marginal_prob_each_state_at_branch_top_AT_node
            ) # the probabilities
names(probs) <- c(
                  "0", 
                  statenames, 
                  apply(
                    combn(statenames,2), 
                    2, 
                    paste, 
                    collapse=""
                  )
                ) # the BioGeoBEARS probability matrix has the null prob,
                  # single state probs, followed by multi-state probs

pies <- probs[1:(length(statenames)+1)]  # the first set are single-areas 

for (ii in 1:length(statenames)) { # parse the multi-areas
  tt <- grep(statenames[ii], names(probs)) # pointers to state names
    # for each pie, after the null state, we sum the single-area pie
    # with half of each of the two-area pies 
    # (n = south east asia)  
  pies[ii+1] <- probs[tt[1]] + rowSums(0.5*probs[tt[-1]])   
	
}

write.csv(pies, "../results/models/prob_pies_13.csv", row.names=F)


## ---- comment2 ----
#### EXPLANATION:
# The trick is to find the label rows with each terrane letter, say:
# n <- grep("n", column_labels)

# Then use that index to add the partial probabilities. 
# The first occurance of the state name is the single-area prob
# to that we add the partial prob of each double letter area 
# split 50-50 between the two areas:

# pies[2] <- probs[n[1]] + rowSums(0.5*probs[n[-1]])   # n (south east asia)

# The first statename correspponds to the second column of the probs matrix, as the first column
# is the null probability

## ---- plot_pies ----
##############################################
# Plot BioGeoBEARS pie charts 
##############################################
# out <- readRDS("../../results/models/hyp_fits.rds")
# scores <- read.csv("../../results/models/modelfit.csv")


pdf(file = "../results/h_DEC.pdf", height = 45, width = 20)
  plot_BioGeoBEARS_results( best_fit,
                            plotwhat = "pie",
                            splitcex=.4,
                            statecex=.4,
                            plotlegend = T,
                            plotsplits = F
  											 )
  plot_BioGeoBEARS_results( best_fit,
                            plotwhat = "text",
                            splitcex=.4,
                            statecex=.4,
                            plotlegend = T,
                            plotsplits = F
                          )
dev.off()


##############################################
## ---- Sensitivity_Analysis ---- 
# Checks the effect of phylogenetic uncertainty by
# repeating analysis on each of 100 trees from 
# the end of the BEAST run 
##############################################

if( run_sensitivity_analysis ) { # Will take 30 hours of run time

  # Save the scores from best models, observed + 100 trees
  # First row is the observed result
  best_models <- read.csv("../results/models/modelfittable.csv")[1,]
  best_models <- cbind(best_models, "tree"=treefile)   # Add tree path
  write.table( best_models, 
					 "../results/models/modelfit_sensitivity.csv", 
					 quote=FALSE,
					 sep=",", 
					 row.names=FALSE
				  ) 

  # Top 100 trees from BEAST analysis 
  # All other inputs remain the same 
  trees <- list.files(
					 "../data/sensitivity_trees", 
					 full.names = TRUE
				   ) # vector of tree files

  ### Interate over trees: assemble hypotheses, run models, print scores
  for (tree in trees) {
  	print(paste("Analyzing tree:", tree))

  	h <- hypothesis_list( hyp_dm, hyp_ts, hyp_un, tree, geo, 
  	                      dispersal, timeperiods, areasallowed)
    fit <- lapply( h, bears_optim_run )
  	scores <- cbind( model_fit_table(fit), "tree"=tree )

    write.table( scores[1,], 
    					  "../results/models/modelfit_sensitivity.csv", 
    					  append=TRUE, 
    					  quote=FALSE,
    					  sep=",", 
    					  row.names=FALSE, 
    					  col.names=FALSE
    					 ) 
  }  
}