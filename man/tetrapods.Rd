% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{tetrapods}
\alias{tetrapods}
\title{Example dataset: Early tetrapod data from the Paleobiology Database}
\format{
A data frame with 5270 rows and 32 variables:
\describe{
\item{occurrence_no}{Reference number given to the particular occurrence in
the Paleobiology Database}
\item{collection_no}{Reference number given to the Paleobiology Database
collection (locality) that the occurrence belongs to}
\item{identified_name}{Taxon name as it appears in the original
publication, which may include expressions of uncertainty (e.g. "cf.",
"aff.", "?") or novelty (e.g. "n. gen.", "n. sp.")}
\item{identified_rank}{The taxonomic rank, or resolution, of the identified
name}
\item{accepted_name}{Taxon name once the identified name has passed through
the Paleobiology Database's internal taxonomy, which collapses synonyms,
amends binomials which have been altered (e.g. species moving to another
genus) and updates taxa which are no longer valid (e.g. \emph{nomina dubia})}
\item{accepted_rank}{The taxonomic rank, or resolution, of the accepted
name}
\item{early_interval}{The oldest (or only) time interval within which the
occurrence is thought to have been deposited}
\item{late_interval}{The youngest time interval within which the occurrence
is thought to have been deposited}
\item{max_ma, min_ma}{The age range given to the occurrence}
\item{phylum, class, order, family, genus}{The taxa (of decreasing
taxonomic level) which the occurrence is identified as belonging to}
\item{abund_value, abund_unit}{The number (and units) of fossils attributed
to the occurrence}
\item{lng, lat}{The modern-day longitude and latitude of the fossil
locality}
\item{collection_name}{The name of the Paleobiology Database collection
which the occurrence belongs to, typically a spatio-temporally restricted
locality}
\item{cc}{The country (code) where the fossils were discovered}
\item{formation, stratgroup, member}{The geological units from which the
fossils were collected}
\item{zone}{The biozone which the occurrence is attributed to}
\item{lithology1}{The main lithology of the beds in the section where the
fossils were collected}
\item{environment}{The inferred environmental conditions in the place of
deposition}
\item{pres_mode}{The mode of preservation of the fossils found in the
collection (not necessarily of that specific occurrence), which will
include information on whether they are body or trace fossils}
\item{taxon_environment}{The environment within which the taxon is thought
to have lived, collated within the Paleobiology Database}
\item{motility, life_habit, diet}{Various types of trait data for the
taxon, collated within the Paleobiology Database}
}
}
\source{
Compiled by Bethany Allen, current version downloaded on 14th July
2022. See item descriptions for details.
}
\usage{
tetrapods
}
\description{
A dataset of tetrapod occurrences ranging from the Carboniferous through to
the Early Triassic, from the
\href{https://paleobiodb.org/}{Palaeobiology Database}. Dataset includes a
range of variables relevant to common palaeobiological analyses, relating to
identification, geography, environmental context, traits and more. Additional
information can be found \href{https://paleobiodb.org/data1.2/}{here}.
The downloaded data is unaltered, with the exception of removing some
superfluous variables, and can be used to demonstrate how the functions
in the palaeoverse package might be applied.
}
\section{References}{

Uhen MD et al. (2023). Paleobiology Database User Guide Version 1.0.
PaleoBios, 40 (11). \doi{10.5070/P9401160531}.
\cr
}

\keyword{datasets}
