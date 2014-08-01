\name{mtiDB}
\alias{mtiDB}
\alias{getMtiCon}
\alias{getMirbaseCon}
\title{
  Connect to the miRTarBase (MTI) or mirbase database.
}
\description{
These functions connect to the database containing the information from
miRTarBase or to the mirbase database (provided by mirbase.db package).
}
\usage{
mtiDB(x, v = TRUE )
getMtiCon( x, v=FALSE )
getMirbaseCon()
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
\code{mtiDB} creates a new database connection each time it is
called. Thus, the function \code{getMtiCon}, that only creates a
new connection if none is available, should be used instead.
}
\value{
  A connection object to the database.
}
\author{
Johannes Rainer
}
\keyword{ data }
