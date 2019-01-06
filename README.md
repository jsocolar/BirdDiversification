# BirdDiversification
Code for analyses of bird diversification and species richness

A guide to the codebase:

### Diversity1.R
Extract species lists for 34K equally spaced points across the terrestrial mainland regions of the globe, and harmonize the names in those species lists with the BirdTree names.

Save objects ‘continents’ and ‘world.conts2’ as .Rdata
* These objects contain shapefiles for the continents, including large land-bridge islands.

Save object ‘mem.shapes’ as ‘mem_shapes_new.Rdata’
* This object contains aggregated shapefiles for the range of each species from Birdlife.

Save object ‘points’ as .Rdata
* This object contains a spatialPointsDataFrame of the 34K points.

Save object ‘breeding_point_data’ as ‘breeding_point_data_new.Rdata’
* This contains a presence-absence matrix for the species across the points, using Birdlife taxonomy.

Save object ‘PTrees’ as .Rdata
* This is a list of 30 equiprobable trees from Pulido & Weir’s update to BirdTree

Save object ‘breeding_point_continental’ as .Rdata and as .csv
* This object contains a matrix of ones and zeros giving species presence-absence at all 34K points, using BirdTree taxonomy.

### Diversity2.R
Compute the species richness and historical richness for each point

Save object ‘cs2’ as .Rdata
* This object gives the species richness of each point.

Save object ‘xy.prop.meta.2’ as ‘xy_prop_meta_2_new.Rdata’
* This object gives the coordinates of a lineages-through-time plot (LTT plot). Its structure is as follows: xy.prop.meta.2 is a 30-element list, with each element corresponding to a different phylogenetic posterior. The elements are 1-element lists (originally the were 5-element lists because I was analyzing different diet groups separately). Each of these elements is a 34184-element list, where each element corresponds to a different point on Earth, and gives a 2-column data-frame.  The first column is the coordinates (in MYA) of the LTT plot.  The second column gives R_h/R_c for each of those points.
	
### Diversity3.R
Get R_h as of 5 MYA. Make a few rasters of species richness and of historical richness as of 5MYA, and mask out large lakes. Also mask out Greenland and Ellesmerle Island, which we shouldn’t have included originally.

Save object all.lakes as ‘all_lakes.Rdata’
* Contains a shapefile for all lakes larger than 1 million hectares

Save objects of the form msr2NEW_i.grd
* Each is a raster of the diversification contribution over the last 5 MY (diversification contribution at this stage in the code is R_h/R_c)

For several different trees, save object points.df as paste("points_df", tree_i, ".Rdata", sep="")
* Contains a dataframe of the masked rasters
	
### Diversity4.R
Associate each point with a continent and biome.

Save object points_cont as ‘points_cont.Rdata’
* This contains a named list where each element gives the points belonging to a given biome-continent combination.

### Diversity5_plotting.R
Generate Figures 1-4 by plotting diversification contributions

**ALSO**, generate the full suite of metrics {R_c/R_h – 1; R_t/R_h – 1; (R_c – R_h)/(R_t – R_h} across all points, and make a bunch of relevant plots

**NOTE**: currently we don’t save these metrics, we just generate them and plot them.  This obviously needs to change.

### Diversity6_plotting_variable_time.R
Generate plots similar to the panels of Figure 3, for a variety of mowing depths (50 MYA to 3 MYA)

### Shiny_plotting.R
Manipulate the plots from Diversity6_plotting_variable_time to make them pretty for our shiny app

### Shiny_app.R
Create shiny app

### Ricklefs_2_0.R
Ricklefs 2.0 analysis of diversity per lineage, comparing tropical to temperate lineages.  Also implements a version where instead of tropical vs. temperate, we divide the world into species-rich versus species-poor areas.
