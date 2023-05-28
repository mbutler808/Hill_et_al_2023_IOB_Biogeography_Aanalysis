## Functions for Biogeography analysis 

###############################################
## ARGUMENTS all are file names with paths:
##
# trfn = tree in nexus format
# geogfn = geographic data, in PHYLIP format 
# dispersal_multipliers_fn = dispersal multiplier matrix 
# timesfn = time stratification matrix 
# areas_allowed_fn = areas allowed matrix 

## ---- make_input --------
##############################################
### Function to assemble BioGeoBEARS input object
### returns BioGeoBEARS obj
##############################################

biogeo_obj <- function(
                 trfn,
                 geogfn, 
                 dispersal_multipliers_fn = NA,
                 timesfn = NA, 
                 areas_allowed_fn = NA
              ){
  x = define_BioGeoBEARS_run(
  	trfn=trfn, 
  	geogfn=geogfn, 
  	dispersal_multipliers_fn = dispersal_multipliers_fn,
  	timesfn = timesfn,
  	areas_allowed_fn = areas_allowed_fn,
  	max_range_size = 2,
    min_branchlength = 0.000001,
  	include_null_range = TRUE,
  	num_cores_to_use = 1,
  	force_sparse = FALSE
  )

  # This function loads inputs (e.g., dispersal multiplier matrix) 
  # into the model object, and runs some checks. Required for models to run.
  x = readfiles_BioGeoBEARS_run(x)

  x$return_condlikes_table = TRUE
  x$calc_TTL_loglike_from_condlikes_table = TRUE
  x$calc_ancprobs = TRUE

  if( !is.na(timesfn) & !is.na(areas_allowed_fn)) x=section_the_tree(inputs=x, make_master_table=TRUE, plot_pieces=FALSE)  # if time stratified

  return(x)
}

## ---- build_hypotheses --------
##############################################
### Function to build list of all hypotheses in an analysis
### returns a list
##############################################

hypothesis_list <- function( hyp_dm, hyp_ts, hyp_un, tree, 
                             geo, dispersal, 
                             timeperiods, 
                             areasallowed ) {
  # models with dispersal multipliers only
  h <- lapply( hyp_dm %w/o% hyp_ts, 
              function(x) biogeo_obj(
                  trfn=tree, 
                  geogfn=geo,
                  dispersal_multipliers_fn = dispersal[x] 
              ) 
            )
  names(h) <- hyp_dm %w/o% hyp_ts

  # no dispersal multipliers
  h[[hyp_un]] <- biogeo_obj(trfn=tree, geogfn=geo)

  # dispersal multipliers and time-stratification
  hts <- lapply( hyp_ts, 
              function(x) biogeo_obj(
                  trfn=tree, 
                  geogfn=geo,
                  dispersal_multipliers_fn = dispersal[x], 
                  timesfn = timeperiods[x], 
                  areas_allowed_fn = areasallowed[x]
                )
              ) 
  names(hts) <- hyp_ts

  # combine the hypotheses into one list 
  h <- c(h, hts)
  return(h)
}

## ---- modelfit_accessors --------
##############################################
### Model Fit statistics accessors
##############################################

logLik.bgb <- function( x ) {
  return( x$total_loglikelihood )
}

AIC.bgb <- function( x, df =2 ) {
  L <- logLik.bgb(x)
  a <- getAIC(LnL = L, df)
  return(a)
}

# getAIC is a BioGeoBears function:
# computes AIC=−2loglikelihood+kn, where k=2 for usual AIC. n=npar
# getAIC <- function (LnL, numparams) 
# {
#     AICval = 2 * numparams - 2 * LnL
#     return(AICval)
# }

## ---- modelfit_table --------
##############################################
### Collect Model fits and parameters from 
### BioGeoBEARS output object
### returns dataframe
##############################################

model_fit_table <- function( fit ) {
	logL <- sapply( fit, logLik.bgb )
	df <- 2 # all models have df=2
	aic <- sapply( fit, AIC.bgb ) 
	disp <- sapply( fit, function(x) return(x$outputs@params_table[1,5]) )
	ext <- sapply( fit, function(x) return(x$outputs@params_table[2,5]) )

# Write AIC, logL, df to dataframe
	scores <- data.frame(id=names(aic), 
                     hypotheses=hypotheses[names(aic)], 
                     delta_aic=aic-min(aic), 
                     m2logL=-2*logL, 
                     df, 
                     dispersal=disp, 
                     extinction=ext
                    )
	scores <- scores[order(scores$delta_aic),]
	return(scores)
}

## ---- without --------
##############################################
# y without y function:
##############################################
"%w/o%" <- function(x,y) { x[ !(x %in% y) ]}


