##### Header #####
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO)
# Group:        Quantitative Assessment Methods Section
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: Matthew.Grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      csas-style-sr
# Code name:    example.R
# Version:      1.0
# Date started: 2018-11-13
# Date edited:  2018-11-13
#
# Overview:
#
#
# Requirements:
#
#
# Notes:
#
#
# References:
#
#

##### Housekeeping #####

# General options
# rm( list=ls( ) )      # Clear the workspace
sTime <- Sys.time( )  # Start the timer
graphics.off( )       # Turn graphics off

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, locn="https://cran.rstudio.com/" ) {
  # Reverse the list
  rPkgs <- rev( pkgs )
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(rPkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("suppressPackageStartupMessages(library(", rPkgs[i],
      "))", sep="")) )
  }  # End i loop over package names
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("knitr", "tidyverse", "xtable", "here") )

##### Controls #####

# Make english figures (if they're not specified in 'example.rnw')
if( !exists("plotEng") ) plotEng <- TRUE

# Plot resolution (dpi)
pDPI <- 600

# Directories
rootd <- here::here()
rootd.data <- file.path( rootd, "data" )
rootd.doc <- file.path( rootd, "maindoc" )
rootd.figs <- file.path( "knitr-figs" )
rootd.tex <- file.path( "knitr-cache-tex" )
rootd.static.figs <- file.path( rootd, "maindoc", "figures" )

##### Parameters #####

# Years
yrs <- 1960:2010

# Dive survey start
firstYrDive <- 1988

# Cross-walk table for SAR to region and region name (and french)
regions <- read_csv( file=
    "Region, RegionName, RegionFR, RegionNameFR
     HG, Haida Gwaii, HG, Haida Gwaii
  PRD, Prince Rupert District, DPR, District de Prince Rupert
  CC, Central Coast, CC, C\u{00F4}te centrale",
  col_types=cols() )

# Region names by type
allRegionNames <- list(
  major=c("Haida Gwaii (HG)", "Prince Rupert District (PRD)",
    "Central Coast (CC)", "Strait of Georgia (SoG)",
    "West Coast of Vancouver Island (WCVI)"),
  minor=c("Area 27 (A27)", "Area 2 West (A2W)") )

# Figure width
figWidth <- 6.5

##### Data #####

# Set the seed
set.seed( 1 )

# Generate some fake spawn index data
spIndex <- tibble(
  Region=rep(c("HG", "PRD", "CC"), each=length(yrs)),
  Year=c(yrs, yrs, yrs),
  Index=c( rnorm(n=length(yrs), mean=10, sd=4),
    rnorm(n=length(yrs), mean=20, sd=8),
    rnorm(n=length(yrs), mean=15, sd=6)),
  Survey=ifelse(Year<firstYrDive, "Surface", "Dive") ) %>%
  mutate( Index=ifelse(Survey=="Dive", Index*1.5, Index),
    Index=ifelse(Index<0, 0, Index) )

# Append region names (and french names)
spIndex <- spIndex %>%
  left_join( y=regions, by="Region" )

# Wrangle the data
spIndex <- spIndex %>%
  mutate(
    RegionName=factor(RegionName, levels=regions$RegionName),
    RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR),
    Survey=factor(Survey, levels=c("Surface", "Dive")),
    Releve=ifelse(Year < firstYrDive, "Surface", "Plong\u{00E9}e"),
    Releve=factor(Releve, levels=c("Surface", "Plong\u{00E9}e")) )

# Create the required directories
dir.create(rootd.figs, showWarnings = FALSE)
dir.create(rootd.tex, showWarnings = FALSE)

##### Main #####

# Mean spawn index by region and survey type
meanSpawn <- spIndex %>%
  group_by( RegionName, Survey ) %>%
  summarise( MeanIndex=mean(Index) ) %>%
  ungroup( ) %>%
  spread( Survey, MeanIndex )

##### Figures #####

# Plot the spawn index
PlotSpawnIndex <- function( dat, fn ) {
  # Get the plot height
  SARs <- length( unique(dat$Region) )
  # Make the base plot
  plotBase <- ggplot( data=dat, aes(x=Year, y=Index) ) +
    scale_shape_manual( values=c(1, 2) ) +
    geom_vline( xintercept=firstYrDive-0.5, linetype="dashed", size=0.25 ) +
    expand_limits( y=0, x=yrs ) +
    theme_bw( ) +
    theme( legend.position="top" )
  # If making the english plot
  if( plotEng ) {
    plotSpawn <- plotBase +
      geom_point( aes(shape=Survey) ) +
      geom_line( aes(group=Survey) ) +
      labs( shape="Survey period",
        y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
      facet_wrap( ~ RegionName, ncol=2, dir="v", scales="free_y" ) +
      ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
        dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*3.5+0.75 )
  } else {  # End if english, otherwise french
    plotSpawn <- plotBase +
      geom_point( aes(shape=Releve) ) +
      geom_line( aes(group=Releve) ) +
      labs( shape="P\u{00E9}riode du relev\u{00E9}", x="Ann\u{00E9}e",
        y=expression(paste("Indice du frai (t"%*%10^3, ")", sep="")) )  +
      facet_wrap( ~ RegionNameFR, ncol=2, dir="v", scales="free_y" ) +
      ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
        dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*3.5+0.75 )
  }  # End if french plot
}  # End PlotSpawnIndex function

# Plot the spawn index
PlotSpawnIndex( dat=spIndex, fn="SpawnIndex")

##### Latex #####

# Bold and latex symbols
Bold2 <- function(x) {paste('\\textbf{',sanitize(x, type="latex"),'}', sep ='')}

# Paste strings nicely
PasteNicely <- function( x, intChars=", ", nChar="and " ) {
  # Get the length of the vector
  n <- length( x )
  # If there are more than two
  if( n > 2 ) {
    # Make a print-friendly vector
    x[n] <- paste( nChar, x[n], sep="" )
    # Get print friendly values
    res <- paste( x, collapse=intChars )
  } else {  # End if more than two, otherwise
    # Add a space
    nCharSp <- paste( " ", nChar, sep="" )
    # Get print friendly values
    res <- paste( x, collapse=nCharSp )
  }  # End if not more than two
  # Return the results
  return( res )
}  # End PasteNicely function

# Formatted year ranges for q1 (surface) and q2 (dive)
qYrs <- list(
  q1=paste(range(yrs[yrs<firstYrDive]), collapse=" to "),
  q2=paste(range(yrs[yrs>=firstYrDive]), collapse=" to ") )

##### Tables #####

# Print mean spawn for latex
xMeanSpawn <- meanSpawn %>%
  rename( Region=RegionName ) %>%
  xtable( align=c("l", "l", "r", "r"), digits=c(0, 0, 1, 1) ) %>%
  print( file=file.path(rootd.tex, "MeanSpawn.tex"), include.rownames=FALSE,
    booktabs=TRUE, NA.string=NA, floating=FALSE,
    sanitize.colnames.function=Bold2 )
