# PigeonExploreR
See Your Data and Visualize Your Plots: 
Scaffolding for Data Science.

Due to a restructuring of Pigeontools, the mote PigeonExploreR is finding new life as it's own package.
It was far too disparate from the other functions in pigeontools. This however, is a great opportunity to
burn it to the ground and start fresh, using what I've learned and discovered to resurrect it to raise it
to reach loftier goals.

PigeonExploreR is mainly about helping researchers explore and modify their data visually. There are three
main features/goals being worked toward:
1. {Main} A Shiny interface for ggplot2 (and extensions)
  * Main goal ~ Allows for more complex and granular plot creation for people not expertised with R/ggplot2
  * Will allow for download of images and/or R code to create the plot itself
2. {Side} A Shiny interface for common data tidying techniques
  * Similar to 1 but for live datatable updates
3. {Offshoot} Various extensions for ggplot2 either in functionality or theme
  * Color palettes and aes themes (especially colorblind safe modes)
  * Theme creator
  * Custom plots if I think of any
