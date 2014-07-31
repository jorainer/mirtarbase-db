## Package initialization:
.onLoad <- function( libname, pkgname ){
    ## setting useFancyQuotes to FALSE!
    .mti.fq <- options( "useFancyQuotes" )[[ 1 ]]
    options( useFancyQuotes=FALSE )
}

.onAttach <- function( libname, pkgname ){
    packageStartupMessage( paste( "Note: global option \"useFancyQuotes\" was set to FALSE.\n" ) )
}

.onUnload <- function( libpath ){
    ## disconnecting from the database
    Vars <- ls( pos=1, all.names=TRUE )
    ## restoring the useFancyQuotes to the pre-load state.
    if( any( Vars == ".mti.fq" ) ){
        options( useFancyQuotes=get( ".mti.fq" ) )
    }
    if( any( Vars == ".mti.con" ) ){
        con <- get( ".mti.con" )
        dbDisconnect( con )
    }
}
