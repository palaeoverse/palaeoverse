#' Geological Time Scale 2020
#'
#' A dataset of the Geological Time Scale 2020. Age data from:
#'  \url{https://stratigraphy.org/timescale/}.
#' Supplementary information is also included in the dataset for plotting functionality (e.g. GTS2020 colour scheme).
#'
#' @format A data frame with 189 rows and 12 variables:
#' \describe{
#'   \item{index}{Index number for the temporal order of all intervals present in the dataset}
#'   \item{stage_number}{Index number for stages}
#'   \item{series_number}{Index number for series}
#'   \item{system_number}{Index number for system}
#'   \item{interval_name}{Names of intervals in the dataset}
#'   \item{rank}{The temporal rank of intervals in the dataset}
#'   \item{max_ma}{The maximum age of the interval in millions of years before present}
#'   \item{mid_ma}{The midpoint age of the interval in millions of years before present}
#'   \item{min_ma}{The minimum age of the interval in millions of years before present}
#'   \item{duration_myr}{The duration of the interval in millions of years}
#'   \item{font}{Colour of font to use for plotting in conjunction with the colour column.}
#'   \item{colour}{Colours of stages based on the ICS timescale \url{https://stratigraphy.org/timescale/}.}
#' }
#' @section References:
#' Gradstein, F.M., Ogg, J.G., Schmitz, M.D. and Ogg, G.M. eds. (2020). Geologic time scale 2020. Elsevier.
#' \cr
#' @source Compiled by Lewis A. Jones. See item descriptions for details.
"GTS2020"

#' Geological Time Scale 2012
#'
#' A dataset of the Geological Time Scale 2012. Age data from:
#'  \url{https://stratigraphy.org/timescale/}.
#' Supplementary information is also included in the dataset for plotting functionality (e.g. GTS2012r colour scheme).
#'
#' @format A data frame with 189 rows and 12 variables:
#' \describe{
#'   \item{index}{Index number for the temporal order of all intervals present in the dataset}
#'   \item{stage_number}{Index number for stages}
#'   \item{series_number}{Index number for series}
#'   \item{system_number}{Index number for system}
#'   \item{interval_name}{Names of intervals in the dataset}
#'   \item{rank}{The temporal rank of intervals in the dataset}
#'   \item{max_ma}{The maximum age of the interval in millions of years before present}
#'   \item{mid_ma}{The midpoint age of the interval in millions of years before present}
#'   \item{min_ma}{The minimum age of the interval in millions of years before present}
#'   \item{duration_myr}{The duration of the interval in millions of years}
#'   \item{font}{Colour of font to use for plotting in conjunction with the colour column.}
#'   \item{colour}{Colours of stages based on the ICS timescale \url{https://stratigraphy.org/timescale/}.}
#' }
#' @section References:
#' Gradstein, F.M., Ogg, J.G., Schmitz, M.D. and Ogg, G.M. eds. (2012). Geologic time scale 2012 Elsevier.
#' \cr
#' @source Compiled by Alessandro Chiarenza. See item descriptions for details.
"GTS2012"

#' Example dataset: Early tetrapod data from the Paleobiology Database
#'
#' A dataset of tetrapod occurrences ranging from the Carboniferous through to
#' the Early Triassic. Data from: \url{https://paleobiodb.org/}.
#' Dataset includes a range of variables relating to identification, geography,
#' environmental context, traits and more. Those relevant to common paleobiological
#' analyses are described below, but more information (and information on other
#' variables) can be found at \url{https://paleobiodb.org/data1.2/}.
#' The downloaded data is unaltered, with the exception of removing some
#' superfluous variables, and can be used to demonstrate how the functions
#' in the palaeoverse package might be applied.
#'
#' @format A data frame with 5270 rows and 57 variables:
#' \describe{
#'   \item{occurrence_no}{Reference number given to the particular occurrence in
#'   the Paleobiology Database}
#'   \item{collection_no}{Reference number given to the Paleobiology Database
#'   collection (locality) that the occurrence belongs to}
#'   \item{identified_name}{Taxon name as it appears in the original publication,
#'   which may include expressions of uncertainty (e.g. "cf.", "aff.", "?") or
#'   novelty (e.g. "n. gen.", "n. sp.")}
#'   \item{identified_rank}{The taxonomic rank, or resolution, of the identified
#'   name}
#'   \item{accepted_name}{Taxon name once the identified name has passed through
#'   the Paleobiology Database's internal taxonomy, which collapses synonyms,
#'   amends binomials which have been altered (e.g. species moving to another
#'   genus) and updates taxa which are no longer valid (e.g. _nomina dubia_)}
#'   \item{accepted_rank}{The taxonomic rank, or resolution, of the accepted
#'   name}
#'   \item{early_interval}{The oldest (or only) time interval within which the
#'   occurrence is thought to have been deposited}
#'   \item{late_interval}{The youngest time interval within which the occurrence
#'   is thought to have been deposited}
#'   \item{max_ma, min_ma}{The age range given to the occurrence}
#'   \item{phylum, class, order, family, genus}{The taxa (of decreasing taxonomic
#'   level) which the occurrence is identified as belonging to}
#'   \item{abund_value, abund_unit}{The number (and units) of fossils attributed
#'   to the occurrence}
#'   \item{lng, lat}{The modern-day longitude and latitude of the fossil locality}
#'   \item{collection_name}{The name of the Paleobiology Database collection
#'   which the occurrence belongs to, typically a spatio-temporally restricted
#'   locality}
#'   \item{cc}{The country (code) where the fossils were discovered}
#'   \item{formation, stratgroup, member}{The geological units from which the
#'   fossils were collected}
#'   \item{zone}{The biozone which the occurrence is attributed to}
#'   \item{lithology1}{The main lithology of the beds in the section where the
#'   fossils were collected}
#'   \item{environment}{The inferred environmental conditions in the place of
#'   deposition}
#'   \item{pres_mode}{The mode of preservation of the fossils found in the
#'   collection (not necessarily of that specific occurrence), which will include
#'   information on whether they are body or trace fossils}
#'   \item{taxon_environment}{The environment within which the taxon is thought
#'   to have lived, collated within the Paleobiology Database}
#'   \item{motility, life_habit, diet}{Various types of trait data for the taxon,
#'   collated within the Paleobiology Database}

#' }
#' @section References:
#' Uhen MD et al. (in prep). Paleobiology Database User Guide.
#' \cr
#' @source Compiled by Bethany Allen, current version downloaded on 14th July
#' 2022. See item descriptions for details.
"tetrapods"
