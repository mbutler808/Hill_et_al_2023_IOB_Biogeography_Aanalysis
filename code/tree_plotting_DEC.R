################################################################################
# tree_plotting_DEC.R  
#  Produces biogeographical evolution tree, a phylogeny with terrane pies (Fig 4)
#  for Asterophryinae frogs published in: 
#
#		Hill et al (2023) Testing geology with biology: Plate tectonics 
# 		and the diversification of microhylid frogs in the Papuan region. 
# 		Integrative Organismal Biology. In press
#
#  Tree branches are painted by majority rule ancestral terrane
#  Pies represent probabilities of ancestral states at nodes
#  Model outputs produced in biogeog.R 
#  Tree and geographical data is from Hill et al (2023)
#  The tree comes from Hill et al 2022 and Hill et al 2023
#  and is converted to phylo format for use with BioGeoBEARS
#  "https://github.com/mbutler808/Asterophryinae_phylogenetic_data/blob/main/BEAST2_analysis/beast_218_tree_asterophryinae.nex?raw=TRUE"
#
#  inputs:
# 	   tree in phylo format 					"../data/biogeo.tree" 
# 	   georegion metadata 				"https://github.com/mbutler808/Asterophryinae_phylogenetic_data/blob/main/Table1.csv?raw=TRUE" 
#     statenames, codes, colors 		"../data/georegion_colors.csv"
#     ancestral state probabilities 		"../results/models/prob_pies_13.csv"
#  outputs:
#     figure  "../results/manuscript_tree_DEC.pdf"
#################################################
## ---- packages_treeplot ----
# Required Packages
library(dplyr)
library(ggplot2)
require(ggtree)
library(treeio)

# x without y function:
"%w/o%" <- function(x, y) x[!x %in% y ]


#################################################
# Read and arrange inputs														#
#################################################

## ---- tree_inputs ----
#################################################
# Load the tree

# Tree in phylo format is converted to a dataframe 
tree <- read.tree("../data/biogeo.tree")
tree.data <- tree %>% as_tibble %>% as.data.frame
istip <- !is.na(tree.data$label)

#################################################
# Load terrane data for tips

# edits terrane names, subsets by tips in the tree
dat <- read.csv("https://github.com/mbutler808/Asterophryinae_phylogenetic_data/blob/main/Table1.csv?raw=TRUE") %>% 
         select(label, terrane, locality, site) %>% 
         mutate(terrane = ifelse(terrane=="dentrecasteaux", locality, terrane)) %>%  
         mutate(terrane = gsub("epct", "EPCT", terrane)) %>% 
         mutate(terrane = gsub("accreted", "Accreted Terranes", terrane)) %>% 
         mutate(terrane = gsub("fold", "Fold Belt", terrane)) %>% 
         mutate(terrane = gsub("craton", "Australian Craton", terrane)) %>%
         mutate(terrane = gsub("vogelkop", "Vogelkop Peninsula", terrane)) %>%
         mutate(terrane = gsub("misima", "Misima Island", terrane)) %>%
         mutate(terrane = gsub("sudest", "Sudest Island", terrane)) %>%
         mutate(terrane = gsub("rossel", "Rossel Island", terrane)) %>%
         mutate(terrane = gsub("woodlark", "Woodlark Island", terrane)) %>%
         mutate(terrane = gsub("newbritain", "New Britain", terrane)) %>%
         mutate(terrane = gsub("notpng", "Southeast Asia", terrane)) %>%
         filter( label %in% as_tibble(tree.data)$label) %>%  		# drop any not in tree
		 mutate(labels = paste(label, locality, site, sep="-")) %>%
		 mutate(labels = gsub("_", " ", labels)) %>%
         select(label, labels, terrane)  # keep only label, terrane

## ---- merge_tree_data ----
####################################################
# Merge tree.data with dat to add terranes to tip labels and nodes
# 218 tips
tree.data.tips <- full_join(tree.data[istip,], dat, by = "label") %>% 
               as_tibble %>% 
               as.data.frame %>%
               mutate(label2 = gsub("[-_]", " ", label)) %>% # clean version
               select(node, label, terrane, label2, labels)

## ---- names_colors ----
####################################################
# Load state names, state codes, and colors
sa <- read.csv("../data/georegion_colors.csv")
oo <- order(sa$states)  # sort by state names Accreted, Australian, etc.
sa <- sa[oo,]  # includes ambiguous
sn <- sa[sa$states!="Ambiguous",]  # excludes ambiguous

## ---- node_probs ----
####################################################
# Load state probabilities at nodes 
prob <- read.csv("../results/models/prob_pies_13.csv")[-1]
statenames <- names(prob)
prob$node <- as.integer(rownames(prob))
prob <- prob %>% relocate(node)

# Reassign statenames in prob with full names for georegions
oo <- sapply( names(prob)[-1], grep, sn$code ) # match codes, get order
names(prob)[-1] <- sn$states[oo]  # assign full names in order

# Add branchcol = ancestral terranes, used for painting the branches
# Add showpie, plot pie if max prob is < 95% 
prob <- prob %>% # the prob of each state
  mutate( branchcol = 
            apply( 
              prob[sn$states], 
              1, 
              function(x) {
                ifelse ( 
                  max(x) < 0.5,
                  "Ambiguous",  # if no region is >50%, leave ambiguous 
                  names(prob[sn$states])[x == max(x)]
                )
              }
            )
        ) %>% # for branch color
  mutate( showpie = 
            apply( 
              prob[sn$states],    # no need to plot pies for single states (>95% prob)
              1, 
              function(x) max(x)<.95
            )
        ) # plot if prob<.95               



## ---- node_pies ----
####################################################
# Create objects for use with ggtree plots									  #
####################################################

# Generates the terrane pies for the nodes for use with ggplot
pies <- nodepie(
            prob[!istip & prob$showpie,], 
            cols = (sn$states),  
            alpha = .7
          )
pies <- lapply(
            pies, 
            function(g) g+scale_fill_manual(values = sn$cols)
          )

td <- full_join(
            prob, 
            tree.data.tips, 
            by = "node"
          ) 

## ---- plot_ggtree ----
####################################################
##  Plot biogeographical evolution tree with terrane pies (Fig 4)
##
##  Use theme_get() to see theme options
####################################################

p <- ggtree(tree, aes(color=branchcol), size=.5) %<+% td + 
  geom_tiplab( aes(label=label2),  # tip labels
  			   color = "black", 
  			   size = 1, 
  			   offset = .3, 
  			   fontface = 3) +
  geom_tippoint( aes(color = terrane, x=x+.22), # georegions on tip points
  				 shape=15, 
  				 size = 1, 
  				 show.legend = F) +
  geom_vline( xintercept = c(4.71, 9.71, 14.71), # vertical lines for time periods
  			  linetype = "dotted", 
  			  linewidth = .5,
  			  color = "grey40") + 
  scale_color_manual( values = sa$cols ) + # specify georegion colors 
  scale_x_continuous( limits = c(-1, 23),   # x-axis tick placements
  					  breaks = c(0, 4.71, 9.71, 14.71, 19.71), 
  					  labels = c(20, 15, 10, 5, 0)) +
  theme( legend.position=c(0.1,.88), # customize the legend placement, text, title size
         legend.text=element_text(size=8),   # size of georegion categories
         legend.title=element_text(size=12), # title size
         legend.key.size = unit(.8, 'lines'), # key size
         legend.key.width = unit(.25, 'cm'),  # key width
         legend.spacing.x = unit(2, "mm"),  # space between key and legend
         legend.spacing.y = unit(0, "mm"),  # space between lines
         legend.background=element_blank(), 
         axis.ticks.x = element_line(linewidth = .4), 
         axis.text.x = element_text(size = 8),
         axis.line.x = element_line(linewidth = .4),
         axis.title.x = element_text(size = 12),
         axis.ticks.length=unit(.2, "cm") 
       ) +
  guides( color = 
            guide_legend( override.aes=list(linewidth=3), # width of key line 
          title = "Terrane")
        ) + # customize legend line size  
  xlab("Time (MYA)") +
  geom_inset( pies, 
  			  width = .07, 
  			  height = .07, 
  			  hjust = .08, 
  			  vjust = .4)   # plot with multiple georegion probabilities

pdf(file = "../results/Fig4_DEC.pdf", height = 10, width = 7)
  print(p)
dev.off()  


## ---- plot_ggtree_sites ----
q <- ggtree(tree, aes(color=branchcol), size=.5) %<+% td + 
  geom_tiplab( aes(label=labels),  # tip labels
  			   color = "black", 
  			   size = 1, 
  			   offset = .3, 
  			   fontface = 3) +
  geom_tippoint( aes(color = terrane, x=x+.22), # georegions on tip points
  				 shape=15, 
  				 size = 1, 
  				 show.legend = F) +
  geom_vline( xintercept = c(4.71, 9.71, 14.71), # vertical lines for time periods
  			  linetype = "dotted", 
  			  linewidth = .5,
  			  color = "grey40") + 
  scale_color_manual( values = sa$cols ) + # specify georegion colors 
  scale_x_continuous( limits = c(-1, 27),   # x-axis tick placements
  					  breaks = c(0, 4.71, 9.71, 14.71, 19.71), 
  					  labels = c(20, 15, 10, 5, 0)) +
  theme( legend.position=c(0.1,.88), # customize the legend placement, text, title size
         legend.text=element_text(size=8),   # size of georegion categories
         legend.title=element_text(size=12), # title size
         legend.key.size = unit(.8, 'lines'), # key size
         legend.key.width = unit(.25, 'cm'),  # key width
         legend.spacing.x = unit(2, "mm"),  # space between key and legend
         legend.spacing.y = unit(0, "mm"),  # space between lines
         legend.background=element_blank(), 
         axis.ticks.x = element_line(linewidth = .4), 
         axis.text.x = element_text(size = 8),
         axis.line.x = element_line(linewidth = .4),
         axis.title.x = element_text(size = 12),
         axis.ticks.length=unit(.2, "cm") 
       ) +
  guides(color = guide_legend( override.aes=list(linewidth=3), # width of key line 
  							   title = "Terrane")) + # customize legend line size  
  xlab("Time (MYA)") +
  geom_inset( pies, 
  			  width = .07, 
  			  height = .07, 
  			  hjust = .08, 
  			  vjust = .4)   # plot with multiple georegion probabilities
  
pdf(file = "../results/Fig4_DEC_sites.pdf", height = 10, width = 7)
  print(q)
dev.off()  
  