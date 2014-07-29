\name{mtiDB}
\alias{mtiDB}
\alias{getMtiCon}
\title{
  Connect to the miRTarBase (MTI) database.
}
\description{
These functions connect to the database containing the information from miRTarBase.
}
\usage{
mtiDB(x, v = TRUE )
getMtiCon( x, v=FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Optional. The file name of the SQLite3 database file.
  }
  \item{v}{
    Verbosity. If TRUE prints additional informations.
  }
}
\details{
This function creates a new database connection each time it is
called. Thus, the function \code{getMtiCon}, that only creates a
new connection if none is already available, should be used instead.
}
\value{
  A connection object to the database.
}
\author{
Johannes Rainer
}
\keyword{ data }
