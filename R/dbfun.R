## x... optional, the database file name.
## v... verbosity
mtiDB <- function( x, v=TRUE ){
    if( missing( x ) ){
        #x <- system.file( "extdata/db/mirtarbase.db", package="mirtarbase.db" )
        x <- "inst/extdata/db/mirtarbase.db"
    }
    lite <- dbDriver("SQLite")
    con <- dbConnect(lite, dbname = x)
    if( v ){
        tables <- dbListTables( con )
        cat( "I've got", length( tables ), "table(s):", paste( tables, collapse=", " ), ".\n" )
    }
    return( con )
}


## internal function to return the connection to the database.
## it will create a new connection if none is found in the global environment (saved as .meal.con).
getMtiCon <- function( x, v=FALSE ){
    Vars <- ls( pos=1, all.names=TRUE )
    if( any( Vars ==".mti.con" ) ){
        return( get( ".mti.con" ) )
    }else{
        cat( "Connecting to database.\n" )
        con <- mtiDB( x, v )
        assign( ".mti.con", con, pos=1 )
        return( con )
    }
}
