% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineage_assign.R
\name{lineage_assign}
\alias{lineage_assign}
\title{Assign Lineages based on proportional membership to categories}
\usage{
lineage_assign(lineages, species_prop = NA, presence_matrix = NULL,
  cutoff_1, cutoff_2, cutoff_3, cutoff_4, cutoff_5 = 0.5)
}
\arguments{
\item{lineages}{output from \code{\link{get_lineages}}}

\item{species_prop}{vector of the proportions of the range that is in species-rich areas for each species, in the order of lineages$species}

\item{cutoff_1}{fraction of the range in species-rich areas that qualifies a species as rich-associated.}

\item{cutoff_2}{fraction of rich-associated species that qualifies a lineage as rich-associated}

\item{cutoff_3}{fraction of the range in species-poor areas that qualifies a species as poor-associated}

\item{cutoff_4}{fraction of species-poor associated species that qualifies a lineage as poor-associated.}

\item{cutoff_5}{richness value (if >1) or quantile (if <1) at which to call a site species-rich vs. species poor, for use only with presence_matrix (and thus species_prop=NA)}
}
\description{
Assign Lineages based on proportional membership to categories
}
\examples{
library(phylosympatry)
tree <- ape::rtree(100)
lineages <- get_lineages(tree,1)
species_prop <- runif(nrow(lineages))
lineage_assign(lineages,species_prop,.7,.1,.3,.2)

}
