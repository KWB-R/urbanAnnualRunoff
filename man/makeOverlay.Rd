% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abimo.R
\name{makeOverlay}
\alias{makeOverlay}
\title{spatial overlay of subcatchments and raster holding information required by ABIMO}
\usage{
makeOverlay(
  rawdir,
  rasterData,
  subcatchmSPobject,
  overlayName,
  subcatchmNamesCol
)
}
\arguments{
\item{rawdir}{rawdir}

\item{rasterData}{rasterData}

\item{subcatchmSPobject}{subcatchmSPobject}

\item{overlayName}{overlayName}

\item{subcatchmNamesCol}{subcatchmNamesCol}
}
\value{
save overlay as .Rdata in directory "rawdir" with filename defined in
parameter "overlayName"
}
\description{
spatial overlay of subcatchments and raster holding information required by ABIMO
}
