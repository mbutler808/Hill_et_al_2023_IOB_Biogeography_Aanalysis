################################################################################
# genera_thru_time.R  
#  Produces plot to visualize the direction and timing of dispersal events 
#  between geographic regions across New Guinea (Fig 5): 
#
#		Hill et al (2023) Testing geology with biology: Plate tectonics 
# 		and the diversification of microhylid frogs in the Papuan region. 
# 		Integrative Organismal Biology. In press
#
#  inputs:
# 	   .csv containing timing and direction of 
#       state transitions "../data/genera_thru_time2.csv" 
#  outputs:
#     figure  "../results/Fig5_plot.pdf"
#################################################
## ---- packages_genusplot ----
# Required Packages

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(forcats)

## ---- read_plot_data ----
# Reads in the data frame and orders the transition states in plotting order
datall <- read.csv("../data/genera_thru_time2.csv")
datall$Direction <- factor( datall$Direction, 
                      c( "E/A", "E/F", "E/R", "E/W", 
                         "E/S", "E/M", "E/G", "E/N", 
                         "E/B", "A/F", "A/C", "A/B", 
                         "F/V", "F/A", "F/C", "F/E", 
                         "V/C","V/F"
										   )
									   )

## ---- sort_plot_data ----
# Sorts the groupings of transitions by time of dispersal events
temp <- reorder(datall$Group, datall$Time)
datall$Group <- factor(datall$Group, levels(temp))
datall$Genus_unique <- datall$Genus
datall$Genus <- sub("[0-9.]+", "", datall$Genus)

# Generates the plotting order for genera
plotlab <- datall$Genus[seq(1, length(datall$Genus), 3)]

## ---- plot_dispersals ----
# Generates the plot of dispersal events
# Note: The unique factor order had to be manually converted to clade names to 
#       represent the custom plotting order based on terrane specific dispersal
#       events on the x-axis 
fig <- datall %>%
  ggplot( aes( x = reorder( Group, desc(Group) ), y = Time ) ) + 
  geom_hline( yintercept = c(20, 15, 10, 3), linetype = "dotted", color = "black") +
  geom_line( aes(group = Group, colour = Direction ), linewidth = .8, lineend = "round") +
  geom_point( aes( colour = Direction ), data=subset( datall[ seq( 3, nrow(datall), 3), ] ), size = 4) + 
  scale_x_discrete(labels = c(
  				   "1" = "Oreophryne A",
                   "2" = "Oreophryne A",
                   "3" = "Oreophryne A",
                   "4" = "Cophixalus",
                   "5" = "Cophixalus",
                   "6" = "Choerophryne",
                   "7" = "Choerophryne",
                   "8" = "Austrochaperina B",
                   "9" = "Austrochaperina B",
                   "10" = "Austrochaperina A",
                   "11" = "Liophryne",
                   "12" = "Sphenophryne",
                   "13" = "Asterophrys",
                   "14" = "Asterophrys",
                   "15" = "Asterophrys",
                   "16" = "Asterophrys",
                   "17" = "Asterophrys",
                   "18" = "Xenorhina",
                   "19" = "Xenorhina",
                   "20" = "Xenorhina",
                   "21" = "Xenorhina",
                   "22" = "Xenorhina",
                   "23" = "Callulops",
                   "24" = "Callulops",
                   "25" = "Mantophryne",
                   "26" = "Hylophorbus",
                   "27" = "Hylophorbus",
                   "28" = "Hylophorbus",
                   "29" = "Hylophorbus",
                   "30" = "Oreophryne A",
                   "31" = "Oreophryne A",
                   "32" = "Oreophryne A",
                   "33" = "Oreophryne A",
                   "34" = "Paedophryne",
                   "35" = "Cophixalus",
                   "36" = "Cophixalus",
                   "37" = "Cophixalus",
                   "38" = "Cophixalus",
                   "39" = "Cophixalus",
                   "40" = "Cophixalus",
                   "41" = "Oreophryne B",
                   "42" = "Oreophryne B",
                   "43" = "Oreophryne B",
                   "44" = "Oreophryne B",
                   "45" = "Oreophryne B",
                   "46" = "Oreophryne B",
                   "47" = "Austrochaperina C",
                   "48" = "Barygenys",
                   "49" = "Barygenys",
                   "50" = "Barygenys",
                   "51" = "Austrochaperina A",
                   "52" = "Austrochaperina A",
                   "53" = "Copiula",
                   "54" = "Copiula",
                   "55" = "Copiula",
                   "56" = "Copiula",
                   "57" = "Liophryne",
                   "58" = "Genyophryne",
                   "59" = "Genyophryne",
                   "60" = "Callulops",
                   "61" = "Callulops",
                   "62" = "Mantophryne",
                   "63" = "Mantophryne",
                   "64" = "Mantophryne",
                   "65" = "Mantophryne",
                   "66" = "Hylophorbus",
                   "67" = "Hylophorbus",
                   "68" = "Hylophorbus",
                   "69" = "Hylophorbus",
                   "70" = "Hylophorbus"
                   )
             ) +
  theme( axis.line.x = element_line(), 
  			  axis.line.y.left=element_line(), 				
  			  panel.background=element_blank(),
  			  panel.border=element_blank(),
  			  panel.grid.major=element_blank(), 
  			  panel.grid.minor=element_blank(),
  			  plot.background=element_blank(), 
  			  axis.title.x = element_blank(), 
  			  legend.position="none", 
  			  axis.text.x = element_text( angle = 90, size = 9 ), 
  			  panel.spacing.x = unit( .25, "lines"), 
  			  strip.background.x = element_rect( linewidth = .5, color = "black" ), 
  			  axis.text.y = element_text( size = 25 ), 
  			  axis.title.y = element_text( size = 25 ), 
  			  axis.text.x.bottom = element_text( vjust = .5, hjust = 1 )
  			) + 
  scale_color_manual( values=c( "orange", "cyan", "violet", "maroon", 
                                "magenta", "purple", "#F89880", "pink",
                                "red", "cyan", "blue", "red", 
                                "yellow", "orange", "blue", "green", 
                                "blue", "cyan"
                               )
                    ) +
  labs( y = ("Time (Ma)") ) +
  facet_grid( cols = vars( Direction ), 
  					scales = "free", 
  					space = "free"
  				  ) +
  theme( strip.text = 
            element_text( color = NA, 
                          margin = margin( .5,0,.5,0, "cm" )
                        )
       )

# Writes the plot to pdf
pdf( file = "../results/Fig5_plot.pdf", height = 16, width = 12)
  print(fig)
dev.off()


