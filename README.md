<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. miRTarBase R package</a>
<ul>
<li><a href="#sec-1-1">1.1. Create the database file</a></li>
<li><a href="#sec-1-2">1.2. Development</a>
<ul>
<li><a href="#sec-1-2-1">1.2.1. The MTI class</a></li>
</ul>
</li>
<li><a href="#sec-1-3">1.3. Changelog:</a></li>
<li><a href="#sec-1-4">1.4. TODOs</a>
<ul>
<li><a href="#sec-1-4-1">1.4.1. <span class="done DONE">DONE</span> Create the sqlite database file from the tab delimited txt file.</a></li>
<li><a href="#sec-1-4-2">1.4.2. <span class="done DONE">DONE</span> Create a class representing an interaction (?).</a></li>
<li><a href="#sec-1-4-3">1.4.3. <span class="done DONE">DONE</span> Create some helper functions to retrieve information from the DB.</a></li>
<li><a href="#sec-1-4-4">1.4.4. <span class="done DONE">DONE</span> Create a txt file with all informations (version, date etc) from the miRTarBase and read it when calling mirtarbase()</a></li>
<li><a href="#sec-1-4-5">1.4.5. <span class="done DONE">DONE</span> Implement the function to get MTIs for a specified gene.</a></li>
<li><a href="#sec-1-4-6">1.4.6. <span class="todo TODO">TODO</span> Create a vignette for this package.</a></li>
<li><a href="#sec-1-4-7">1.4.7. <span class="done CANCELED">CANCELED</span> Include also the phenomiR database?</a></li>
<li><a href="#sec-1-4-8">1.4.8. <span class="done DONE">DONE</span> implement a central <code>getMti</code> function.</a></li>
<li><a href="#sec-1-4-9">1.4.9. <span class="done DONE">DONE</span> Implement functions to create MTI objects from a data.frame (and <i>vice versa</i>). <code>[4/4]</code></a></li>
<li><a href="#sec-1-4-10">1.4.10. <span class="done DONE">DONE</span> Implement all methods for the <code>Report</code> class <code>[4/4]</code>.</a></li>
<li><a href="#sec-1-4-11">1.4.11. <span class="done DONE">DONE</span> Implement all methods for the <code>MTI</code> class <code>[12/12]</code>.</a></li>
<li><a href="#sec-1-4-12">1.4.12. <span class="todo TODO">TODO</span> Implement a function that retrieves additional miRNA annotations for a MTI.</a></li>
<li><a href="#sec-1-4-13">1.4.13. <span class="done DONE">DONE</span> Implent additional functions <code>[2/2]</code></a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>



---

# miRTarBase R package<a id="sec-1" name="sec-1"></a>

This packages provides the miRNA-target gene interactions (MTI) from the miRTarBase.

## Create the database file<a id="sec-1-1" name="sec-1-1"></a>

Using the code in this section we create the `SQLite` database for the package. The workflow is the following: first we have to download the XLS spreadsheet from the miRTarBase web site, which we have to open in Excel and export as tabulator delimited text file. We will then read this file into R in order format it for our requirements (i.e. rename the column names) and we store this file than into a `SQLite` database.

Assuming that the Excel file has been downloaded from <http://mirtarbase.mbc.nctu.edu.tw/php/download.php> and has been exported as a tabulator delimited txt file to `orig_files/txt`. Next we read this file, rename the columns and export it again as a tab delimited text file (over-writing the original one).

It is a little more complicated to process this file due to the following problems:

-   Experiments column contains 5' RACE and 5" RACE and these will be recognized by `R` as quotes. Thus we have to read the file without specifying quotes.
-   Experiments column contains *Weak* but also *WeaK*, will have to replace these.
-   Some Entrezgene IDs are NA.

We could also create the database in a cleaner way, i.e. as a *real* relational database with a table for the miRNA target gene interaction, a table with the evidence (Report) and one relating each other. The question remains whether that would improve it's performance.
Note that each publication can have more than one report.

    ## want to know whether the "report" is unique (i.e. pubmed id, experiments and support type) across all miRNAs are specific for a miRNA.
    Report <- data.frame( matrix( ncol=4, nrow=0 ), stringsAsFactors=FALSE )
    colnames( Report ) <- c( "report_id", "experiments", "support_type", "references_pmid" )
    mti2report <- matrix( ncol=2, nrow=0 )
    colnames( mti2report ) <- c( "mti_id", "report_id" )
    for( i in 1:nrow( MTI ) ){
        current.rep <- MTI[ i, c( "experiments", "support_type", "references_pmid" ) ]
        ## check if it's in Report
        idx <- which( rownames( Report )==paste0( current.rep[ 1, ], collapse="-" ) )
        if( length( idx ) > 0 ){
            ## have it already
            #cat( "have already the report\n" )
            report.id <- Report[ idx, "report_id" ]
        }else{
            rownames( current.rep ) <- paste0( current.rep[ 1, ], collapse="-" )
            report.id <- paste0( "MTIR", sprintf( "%04d", ( nrow( Report )+1 ) ) )
            current.rep <- cbind( report_id=report.id, current.rep, stringsAsFactors=FALSE )
            Report <- rbind( Report, current.rep )
        }
        ## add it to mti2report.
        mti2report <- rbind( mti2report, c( MTI[ i, 1 ], report.id ) )
    }
    ## at last define the MTI table.
    MTI.table <- unique( MTI[ , !( colnames( MTI ) %in% colnames( Report ) ) ] )

    ## ok, now we do have all tables.
    con <- dbConnect( dbDriver( "SQLite" ), dbname="inst/extdata/db/mirtarbase_rel.db" )
    if( dbExistsTable( con, "mti" ) ){
        dbRemoveTable( con, "mti" )
    }
    if( dbExistsTable( con, "report" ) ){
        dbRemoveTable( con, "report" )
    }
    if( dbExistsTable( con, "mti2report" ) ){
        dbRemoveTable( con, "mti2report" )
    }
    ## mti
    dbWriteTable( con, name="mti", MTI.table, row.names=FALSE )
    dbGetQuery( con, "create index mirna_idx on mti (mirna);" )
    dbGetQuery( con, "create index target_gene_idx on mti (target_gene);" )
    dbGetQuery( con, "create index target_gene_entrez_idx on mti (target_gene_entrez_gene_id);" )
    ## report
    dbWriteTable( con, name="report", Report, row.names=FALSE )
    dbGetQuery( con, "create index report_id_idx on report (report_id);" )
    ## mti2report
    dbWriteTable( con, name="mti2report", data.frame( mti2report, stringsAsFactors=FALSE ), row.names=FALSE )
    dbGetQuery( con, "create index mti2report_report_idx on mti2report (report_id);" )
    dbGetQuery( con, "create index mti2report_mti_idx on mti2report (mti_id);" )

    dbDisconnect( con )

Next we create the database for the package. This requires a working installation of `SQLite`.

    con <- dbConnect( dbDriver( "SQLite" ), dbname="inst/extdata/db/mirtarbase.db" )
    if( dbExistsTable( con, "mirtarbase" ) ){
        dbRemoveTable( con, "mirtarbase" )
    }
    dbWriteTable( con, name="mirtarbase", MTI, row.names=FALSE )
    dbGetQuery( con, "create index mirna_idx on mirtarbase (mirna);" )
    dbGetQuery( con, "create index target_gene_idx on mirtarbase (target_gene);" )
    dbGetQuery( con, "create index target_gene_entrez_idx on mirtarbase (target_gene_entrez_gene_id);" )
    dbDisconnect( con )

## Development<a id="sec-1-2" name="sec-1-2"></a>

At present (<span class="timestamp-wrapper"><span class="timestamp">&lt;2014-07-28&gt;</span></span>), the miRTarBase can only be exported as a XLS spread sheet. This file contains one line per MTI and publication:
-   MTI MIRT000140: online: 2 references, XLS sheet: one (one reference missing in XLS sheet).
-   MTI MIRT001206: online: 8 references, XLS sheet: 3 rows (several rows missing).
-   MTI MIRT003413: online: 4 references, XLS sheet: 2 rows (2 rows missing):
    -   PMID: 18328430: experiments: Luciferase reporter assay//Microarray//Western blot; evidence: Functional MTI. Online: also Other listed as experiment.
    -   PMID: 19422085: experiments: Luciferase reporter assay//Microarray//qRT-PCR//Western blot; evidence: Functional MTI. Online: also Other listed as experiment.

Thus, an MTI between the same miRNA and target gene (for the same species!) is stored in several rows in the XLS sheet. Each row seems however be related to one publication, and the field *experiments* seems to list all experiments performed in that publication.

One question is whether the XLS sheet should be stored as-is to the database, or whether a clean relational database should be created. The benefit from the latter approach would be to allow more specific queries, e.g. all MTIs based on a certain support type.

It would be possible to create a relational database with 3 tables, one describing the MTI, one listing the experiments performed in a publication to validate this MTI and one allowing for possible n:m relations (although it's not clear whether these really exist&#x2026; yet).

### The MTI class<a id="sec-1-2-1" name="sec-1-2-1"></a>

The MTI class represents a miRNA target gene interaction. There should only be one MTI for a miRNA target gene combination, with multiple evidences and eventually multiple publications. The unique identifier for a MTI is the identifier used in the miRTarBase (e.g. *MIRT001206*).

class MTI
       L\_ class Report
                 L\_ experiments: lists all experiments that have been performed.
                 L\_ pmid: returns the (PMID) of the report.
                 L\_ supportedBy: list support type(s) (evidences).
       L\_ reports: returns the Report(s) of the MTI.
       L\_ experiments: returns all experiments (of the Report(s)).
       L\_ pmids: returns the PMIDs of the Report(s).
       L\_ supportedBy

## Changelog:<a id="sec-1-3" name="sec-1-3"></a>

-   version 0.2.0 (2014-08-01):
    -   Fixed some series problems in the XLS sheet from the miRTarBase. Missed some MTIs in the previous database versions.
    -   Removed functions `getMtiForGene` and `getMtiForMiRNA` and replaced both with `getMti`.
-   version 0.1.0 (2014-07-29): it's done: the first *release*. Basic functionality, not Vignette, no S4 objects (yet).
-   version 0.0.1 (2014-07-28): added database.

## TODOs<a id="sec-1-4" name="sec-1-4"></a>

### DONE Create the sqlite database file from the tab delimited txt file.<a id="sec-1-4-1" name="sec-1-4-1"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-28 Mon 13:59]</span></span>

### DONE Create a class representing an interaction (?).<a id="sec-1-4-2" name="sec-1-4-2"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-31 Thu 11:00]</span></span>

What would be the benefits from this?

-   Object oriented approach (which would be helpful if not the only function of the package would be to fetch data).

Did that. That's the `MTI` object.

### DONE Create some helper functions to retrieve information from the DB.<a id="sec-1-4-3" name="sec-1-4-3"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-28 Mon 15:20]</span></span>

### DONE Create a txt file with all informations (version, date etc) from the miRTarBase and read it when calling mirtarbase()<a id="sec-1-4-4" name="sec-1-4-4"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-28 Mon 15:20]</span></span>

### DONE Implement the function to get MTIs for a specified gene.<a id="sec-1-4-5" name="sec-1-4-5"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-29 Tue 10:52]</span></span>

### TODO Create a vignette for this package.<a id="sec-1-4-6" name="sec-1-4-6"></a>

### CANCELED Include also the phenomiR database?<a id="sec-1-4-7" name="sec-1-4-7"></a>

-   State "CANCELED"   from "DONE"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-31 Thu 10:59]</span></span>
          miRTarBase contains also other databases and will (hopefully) be updated regularily.
-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-31 Thu 10:59]</span></span>

This makes only sense, if the phenomiR does provide additional information.
Will not do that, since the guys from miRTarBase claim that they provide the most data and include also other databases.

### DONE implement a central `getMti` function.<a id="sec-1-4-8" name="sec-1-4-8"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-08-01 Fri 11:06]</span></span>

This function should take either one or more gene or miRNA IDs as input and return a list of MTI objects.
Split the data.frame by miRTarBase IDs, make one MTI for each data.frame and nrow Report classes.

### DONE Implement functions to create MTI objects from a data.frame (and *vice versa*). <code>[4/4]</code><a id="sec-1-4-9" name="sec-1-4-9"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-31 Thu 10:58]</span></span>

These should be put into *convertfunctions.R*.

-   [X] data.frame2report
-   [X] report2data.frame
-   [X] data.frame2mti
-   [X] mti2data.frame

### DONE Implement all methods for the `Report` class <code>[4/4]</code>.<a id="sec-1-4-10" name="sec-1-4-10"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-31 Thu 11:46]</span></span>

-   [X] show
-   [X] experiments
-   [X] supportedBy
-   [X] pmid

### DONE Implement all methods for the `MTI` class <code>[12/12]</code>.<a id="sec-1-4-11" name="sec-1-4-11"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-31 Thu 11:46]</span></span>

-   [X] show
-   [X] id
-   [X] reports
-   [X] experiments
-   [X] supportedBy
-   [X] pmid
-   [X] matureMirna
-   [X] mirnaSpecies
-   [X] targetGene
-   [X] targetGeneSpecies
-   [X] targetGeneEntrezid
-   [X] countReports

### TODO Implement a function that retrieves additional miRNA annotations for a MTI.<a id="sec-1-4-12" name="sec-1-4-12"></a>

The idea is to get the pre-miRNA or miRNA family for a given mature miRNA.
This should then also be exported if MTIs are exported as a `data.frame`.

### DONE Implent additional functions <code>[2/2]</code><a id="sec-1-4-13" name="sec-1-4-13"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-08-01 Fri 12:03]</span></span>

-   [X] getAvailableExperiments: retrieves a unique list of experiments.
-   [X] getPmids: retrieves a unique list of PubMed IDs.