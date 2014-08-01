
#################
##     CLASSES
##
## A report (i.e. publication) that reported the MTI.
setClass( "Report",
         representation( pmid="numeric",
                        experiments="character",
                        support_type="character"
                        ),
         prototype=list( pmid=0,
             experiments="",
             support_type=""
                        )
         )
newReport <- function( pmid=0, support_type="", experiments="" ){
    if( is.na( pmid ) ){
        return( NA )
    }
    rep <- new( "Report",
               pmid=pmid,
               support_type=support_type,
               experiments=experiments )
    return( rep )
}
## the main MTI class representing a miRNA target gene interaction.
setClass( "MTI",
         representation(id = "character",
                        mature_mirna="character",
                        species_mirna="character",
                        query="character",
                        target_gene="character",
                        target_gene_entrezid="numeric",
                        species_target_gene="character",
                        report="list"),
         prototype=list( id="",
             mature_mirna="",
             species_mirna="",
             query="",
             target_gene="",
             target_gene_entrezid=0,
             species_target_gene="",
             report=list( newReport() ))
         )

## constructors.
newMTI <- function( id="", mature_mirna="", species_mirna="", query="", target_gene="", target_gene_entrezid=0, species_target_gene="", report=list( ) ){
    ## a MTI has to have an ID!
    if( is.na( id ) ){
        return( NA )
    }
    mti <- new( "MTI",
               id=id,
               mature_mirna=mature_mirna,
               species_mirna=species_mirna,
               query=query,
               target_gene=target_gene,
               target_gene_entrezid=target_gene_entrezid,
               species_target_gene=species_target_gene,
               report=report
               )
    return( mti )
}


#################
##     METHODS
##
## MTI
##
## check validity of MTI instances.
validateMTI <- function( object ){
    ## check if the objects in the slot report are of the type Report
    if( length( object@report ) > 0 ){
        Classes <- unique( unlist( lapply( object@report, class ) ) )
        if( any( Classes != "Report" ) ){
            return( paste0( "Slot \"report\" should contain only \"Report\" objects! I found ", paste( Classes, collapse="," ), "!" ) )
        }
    }
    return( TRUE )
}
setValidity( "MTI", validateMTI )
setMethod( "initialize", "MTI", function( .Object,... ){
    OK <- validateMTI( .Object )
    if( class( OK )=="character" ){
        stop( OK )
    }
    callNextMethod( .Object, ... )
})



##### Generic method definitions:
##
if( !isGeneric( "pmid" ) )
    setGeneric( "pmid", function( object, ... )
               standardGeneric( "pmid" ))
if( !isGeneric( "experiments" ) )
    setGeneric( "experiments", function( object, ... )
        standardGeneric( "experiments" ))
if( !isGeneric( "reports" ) )
    setGeneric( "reports", function( x, ... )
        standardGeneric( "reports" ))
if( !isGeneric( "supportedBy" ) )
    setGeneric( "supportedBy", function( object, ... )
        standardGeneric( "supportedBy" ))
if( !isGeneric( "matureMirna" ) )
    setGeneric( "matureMirna", function( object, ... )
        standardGeneric( "matureMirna" ))
if( !isGeneric( "mirnaSpecies" ) )
    setGeneric( "mirnaSpecies", function( object, ... )
        standardGeneric( "mirnaSpecies" ))
if( !isGeneric( "targetGene" ) )
    setGeneric( "targetGene" , function( object, ... )
               standardGeneric( "targetGene" ))
if( !isGeneric( "targetGeneSpecies" ) )
    setGeneric( "targetGeneSpecies" , function( object, ... )
               standardGeneric( "targetGeneSpecies" ))
if( !isGeneric( "targetGeneEntrezid" ) )
    setGeneric( "targetGeneEntrezid" , function( object, ... )
               standardGeneric( "targetGeneEntrezid" ))
if( !isGeneric( "id" ) )
    setGeneric( "id" , function( object, ... )
               standardGeneric( "id" ))
if( !isGeneric( "reportCount" ) )
    setGeneric( "reportCount" , function( object, ... )
               standardGeneric( "reportCount" ))
if( !isGeneric( "mirna" ) )
    setGeneric( "mirna" , function( object, ... )
               standardGeneric( "mirna" ))
if( !isGeneric( "matureAccession" ) )
    setGeneric( "matureAccession" , function( object )
               standardGeneric( "matureAccession" ))
if( !isGeneric( "matureSequence" ) )
    setGeneric( "matureSequence" , function( object, ... )
               standardGeneric( "matureSequence" ))
if( !isGeneric( "mirnaAccession" ) )
    setGeneric( "mirnaAccession" , function( object )
               standardGeneric( "mirnaAccession" ))
if( !isGeneric( "mirnaFamily" ) )
    setGeneric( "mirnaFamily" , function( object, ... )
               standardGeneric( "mirnaFamily" ))
#if( !isGeneric( "" ) )
#    setGeneric( "" , function( object, ... )
#               standardGeneric( "" ))
##
#####



##### Methods for Report
##
#if( !isGeneric("data.frame") )
# 	setGeneric("data.frame", function( ..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  stringsAsFactors = default.stringsAsFactors() )
# 	standardGeneric("data.frame"))
setMethod( "show", "Report", function( object ){
    cat( paste0( "PMID: ", pmid( object ), "\n" ) )
    cat( paste0( "Support type: ", supportedBy( object ), "\n" ) )
    cat( paste0( "Experiments: ", paste( experiments( object ), collapse=", " ), "\n" ) )
} )
## these are the getter methods:
## slot: pmid
setMethod( "pmid", "Report", function( object, ... ){
    return( object@pmid )
} )
## slot experiments
setMethod( "experiments", "Report", function( object, ... ){
    return( object@experiments )
} )
## slot support_type
setMethod( "supportedBy", "Report", function( object, ... ){
    return( object@support_type )
} )

##### Methods for MTI
##
setMethod( "show", "MTI", function( object ){
    cat( paste0( "ID: ", id( object ), "\n" ) )
    cat( paste0( "mature miRNA: ", matureMirna( object ), "\n" ) )
    cat( paste0( "miRNA species: ", mirnaSpecies( object ), "\n" ) )
    cat( paste0( "target gene: ", targetGene( object ), "\n" ) )
    cat( paste0( "target gene entrezid: ", targetGeneEntrezid( object ), "\n" ) )
    cat( paste0( "target gene species: ", targetGeneSpecies( object ), "\n" ) )
    cat( paste0( "Number of supporting reports: ", reportCount( object ), "\n" ) )
    cat( "Reports:\n" )
    lapply( reports( object ), show )
} )
## getter methods:
## slot report:
## this method will always return a list with at least one (eventually empty) Report!
setMethod( "reports", "MTI",
          function( x, ... ){
              if( length( x@report )==0 ){
                  return( list( newReport() ) )
              }
              return( x@report )
          }
          )
## slot id
setMethod( "id", "MTI",
          function( object, ... ){
              return( object@id )
          })
## slot mature_mirna
setMethod( "matureMirna", "MTI",
          function( object, ... ){
              return( object@mature_mirna )
          })
## slot species_mirna
setMethod( "mirnaSpecies", "MTI",
          function( object, ... ){
              return( object@species_mirna )
          })
## slot target_gene
setMethod( "targetGene", "MTI",
          function( object, ... ){
              return( object@target_gene )
          })
## slot target_gene_entrezid
setMethod( "targetGeneEntrezid", "MTI",
          function( object, ... ){
              return( object@target_gene_entrezid )
          })
## slot species_target_gene
setMethod( "targetGeneSpecies", "MTI",
          function( object, ... ){
              return( object@species_target_gene )
          })

## getters for Report inside MTI:
## pmid
setMethod( "pmid", "MTI",
          function( object, ... ){
              return( unlist( lapply( reports( object ), pmid ) ) )
          })
## experiments
setMethod( "experiments", "MTI",
          function( object, ... ){
              return( unlist( lapply( reports( object ), experiments ) ) )
          })
## supportedBy
setMethod( "supportedBy", "MTI",
          function( object, ... ){
              return( unlist( lapply( reports( object ), supportedBy ) ) )
          })
## other methods.
setMethod( "reportCount", "MTI",
          function( object, ... ){
              reps <- reports( object )
              return( length( reps ) )
          })

## get the miRNA ID for the mature miRNA of a MTI.
setMethod( "mirna", "MTI",
          function( object, ... ){
              return( getMirbaseForMature( matureMirna( object ) )[ , "mirna_id" ] )
          })
setMethod( "mirnaFamily", "MTI",
          function( object, ... ){
              return( unique( getMirbaseForMature( matureMirna( object ), tables=c( "mirna", "mirna_prefam" ) )[ , "prefam_id" ] ) )
          })
setMethod( "mirnaAccession", "MTI",
          function( object ){
              return( getMirbaseForMature( matureMirna( object ) )[ , "mirna_acc" ] )
          })
setMethod( "matureAccession", "MTI",
          function( object ){
              return( unique( getMirbaseForMature( matureMirna( object ) )[ , "mature_acc" ] ) )
          })
setMethod( "matureSequence", "MTI",
          function( object, ... ){
              tmp <- getMirbaseForMature( matureMirna( object ) )
              matseq <- apply( tmp, MARGIN=1, function( z ){
                  return( substr( z[ "sequence" ], start=as.numeric( z[ "mature_from" ] ), stop=as.numeric( z[ "mature_to" ] )) )
              } )
              return( unique( matseq ) )
          })


