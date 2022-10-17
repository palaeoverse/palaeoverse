#' Geological Timescale 2020
#'
#' A dataframe of the Geological Timescale 2020. Age data from the
#' [International Commission on Stratigraphy](
#' https://stratigraphy.org/ICSchart/ChronostratChart2020-03.pdf).
#' Supplementary information is included in the dataset for plotting
#' functionality (e.g. GTS2020 colour scheme).
#'
#' @format A data frame with 189 rows and 9 variables:
#' \describe{
#'   \item{interval_number}{Index number for the temporal order of all
#'   intervals present in the dataset.}
#'   \item{interval_name}{Names of intervals in the dataset.}
#'   \item{rank}{The temporal rank of intervals in the dataset.}
#'   \item{max_ma}{The maximum age of the interval in millions of years before
#'   present.}
#'   \item{mid_ma}{The midpoint age of the interval in millions of years before
#'   present.}
#'   \item{min_ma}{The minimum age of the interval in millions of years before
#'   present.}
#'   \item{duration_myr}{The duration of the interval in millions of years.}
#'   \item{font}{Colour of font to use for plotting in conjunction with the
#'   colour column.}
#'   \item{colour}{Colours of stages based on the [ICS timescale](
#'   https://stratigraphy.org/timescale/).}
#' }
#' @section References:
#' Gradstein, F.M., Ogg, J.G., Schmitz, M.D. and Ogg, G.M. eds. (2020).
#' Geologic Timescale 2020. Elsevier.
#' \cr
#' @source Compiled by Lewis A. Jones (2022-07-02) from the [ICS](
#' https://stratigraphy.org/ICSchart/ChronostratChart2020-03.pdf).
"GTS2020"

#' Geological Timescale 2012
#'
#' A dataframe of the Geological Timescale 2012. Age data from the
#' [International Commission on Stratigraphy](
#' https://stratigraphy.org/ICSchart/ChronostratChart2012.pdf).
#' Supplementary information is also included in the dataset for plotting
#' functionality (e.g. GTS2012 colour scheme).
#'
#' @format A data frame with 186 rows and 9 variables:
#' \describe{
#'   \item{interval_number}{Index number for the temporal order of all
#'   intervals present in the dataset.}
#'   \item{interval_name}{Names of intervals in the dataset.}
#'   \item{rank}{The temporal rank of intervals in the dataset.}
#'   \item{max_ma}{The maximum age of the interval in millions of years before
#'   present.}
#'   \item{mid_ma}{The midpoint age of the interval in millions of years before
#'   present.}
#'   \item{min_ma}{The minimum age of the interval in millions of years before
#'   present.}
#'   \item{duration_myr}{The duration of the interval in millions of years.}
#'   \item{font}{Colour of font to use for plotting in conjunction with the
#'   colour column.}
#'   \item{colour}{Colours of stages based on the [ICS timescale](
#'   https://stratigraphy.org/timescale/).}
#' }
#' @section References:
#' Gradstein, F.M., Ogg, J.G., Schmitz, M.D. and Ogg, G.M. eds. (2012).
#' Geologic Timescale 2012. Elsevier.
#' \cr
#' @source Compiled by Lewis A. Jones (2022-07-02) from the [ICS](
#' https://stratigraphy.org/ICSchart/ChronostratChart2012.pdf).
"GTS2012"

#' Example dataset: Early tetrapod data from the Paleobiology Database
#'
#' A dataset of tetrapod occurrences ranging from the Carboniferous through to
#' the Early Triassic, from the
#' [Palaeobiology Database](https://paleobiodb.org/). Dataset includes a
#' range of variables relevant to common palaeobiological analyses, relating to
#' identification, geography, environmental context, traits and more. Additional
#' information can be found [here](https://paleobiodb.org/data1.2/).
#' The downloaded data is unaltered, with the exception of removing some
#' superfluous variables, and can be used to demonstrate how the functions
#' in the palaeoverse package might be applied.
#'
#' @format A data frame with 5270 rows and 32 variables:
#' \describe{
#'   \item{occurrence_no}{Reference number given to the particular occurrence in
#'   the Paleobiology Database}
#'   \item{collection_no}{Reference number given to the Paleobiology Database
#'   collection (locality) that the occurrence belongs to}
#'   \item{identified_name}{Taxon name as it appears in the original
#'   publication, which may include expressions of uncertainty (e.g. "cf.",
#'   "aff.", "?") or novelty (e.g. "n. gen.", "n. sp.")}
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
#'   \item{phylum, class, order, family, genus}{The taxa (of decreasing
#'   taxonomic level) which the occurrence is identified as belonging to}
#'   \item{abund_value, abund_unit}{The number (and units) of fossils attributed
#'   to the occurrence}
#'   \item{lng, lat}{The modern-day longitude and latitude of the fossil
#'   locality}
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
#'   collection (not necessarily of that specific occurrence), which will
#'   include information on whether they are body or trace fossils}
#'   \item{taxon_environment}{The environment within which the taxon is thought
#'   to have lived, collated within the Paleobiology Database}
#'   \item{motility, life_habit, diet}{Various types of trait data for the
#'   taxon, collated within the Paleobiology Database}
#' }
#' @section References:
#' Uhen MD et al. (in prep). Paleobiology Database User Guide.
#' \cr
#' @source Compiled by Bethany Allen, current version downloaded on 14th July
#' 2022. See item descriptions for details.
"tetrapods"

#' Example dataset: Phanerozoic reefs from the PaleoReefs Database
#'
#' A dataset of Phanerozoic reef occurrences from the
#' [PaleoReefs Database](https://www.paleo-reefs.pal.uni-erlangen.de) (PARED).
#' This example dataset includes a subset of the available data from PARED,
#' but can be used to demonstrate how the functions in the `palaeoverse`
#' package might be applied.
#'
#' @format A data frame with 4363 rows and 14 variables:
#' \describe{
#'   \item{r_number}{Reference number given to the particular fossil
#'   reef in PARED}
#'   \item{name}{Reference name given to the particular fossil
#'   reef in PARED}
#'   \item{formation}{The geological formation to which the fossil
#'   reef belongs}
#'   \item{system}{The stratigraphic system to which the fossil
#'   reef belongs}
#'   \item{series}{The stratigraphic series to which the fossil
#'   reef belongs}
#'   \item{interval}{The stratigraphic interval to which the fossil
#'   reef belongs}
#'   \item{biota_main}{The main biota present within the fossil
#'   reef}
#'   \item{biota_sec}{The secondary biota present within the fossil
#'   reef}
#'   \item{lng}{The modern-day longitude of the fossil reef}
#'   \item{lat}{The modern-day latitude of the fossil reef}
#'   \item{country}{The country or ocean the fossil reef is located in}
#'   \item{authors}{The authors of the publication documenting the fossil reef}
#'   \item{title}{The title of the publication documenting the fossil reef}
#'   \item{year}{The year of the publication documenting the fossil reef}
#' }
#' @section References:
#' Kiessling, W. & Krause, M. C. (2022). PaleoReefs Database (PARED)
#' (1.0) Data set. \doi{10.5281/zenodo.6037852}
#' \cr
#' @source Compiled by Lewis A. Jones. Downloaded on the 25th July 2022.
#' \doi{10.5281/zenodo.6037852}
"reefs"

#' Example dataset: Interval key for the look_up function
#'
#' A table of geological intervals and the earliest and latest
#' corresponding international geological stages from the International
#' Commission on Stratigraphy (ICS). The table was compiled using
#' regional stratigraphies, the [GeoWhen Database](
#' https://timescalefoundation.org/resources/geowhen/), temporal
#' information from the [Paleobiology Database](
#' https://paleobiodb.org/classic/displaySearchStrataForm) and the
#' [Geological Timescale 2016](
#' https://stratigraphy.org/ICSchart/ChronostratChart2016-10.pdf).
#' Some assignments were made with incomplete information on
#' the stratigraphic provenance of intervals. The assignments in this
#' table should be verified before research use.
#'
#' @format A data frame with 783 rows and 3 variables:
#' \describe{
#'   \item{interval_name}{Stratigraphic interval}
#'   \item{early_stage}{Earliest (oldest) geological stage which overlaps with
#'   the interval}
#'   \item{late_stage}{Latest (youngest) geological stage which overlaps with
#'   the interval}
#' }
#' @source Compiled by K. Eichenseer for assigning geological stages to
#' occurrences from the [Paleobiology Database](https://paleobiodb.org)
#' and the [PaleoReefs Database](https://www.paleo-reefs.pal.uni-erlangen.de/).
"interval_key"
