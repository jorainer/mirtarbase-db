<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. miRTarBase R package</a>
<ul>
<li><a href="#sec-1-1">1.1. Create the database file</a></li>
<li><a href="#sec-1-2">1.2. Changelog:</a></li>
<li><a href="#sec-1-3">1.3. TODOs</a>
<ul>
<li><a href="#sec-1-3-1">1.3.1. <span class="done DONE">DONE</span> Create the sqlite database file from the tab delimited txt file.</a></li>
<li><a href="#sec-1-3-2">1.3.2. <span class="todo TODO">TODO</span> Create a class representing an interaction (?).</a></li>
<li><a href="#sec-1-3-3">1.3.3. <span class="done DONE">DONE</span> Create some helper functions to retrieve information from the DB.</a></li>
<li><a href="#sec-1-3-4">1.3.4. <span class="done DONE">DONE</span> Create a txt file with all informations (version, date etc) from the miRTarBase and read it when calling mirtarbase()</a></li>
<li><a href="#sec-1-3-5">1.3.5. <span class="done DONE">DONE</span> Implement the function to get MTIs for a specified gene.</a></li>
<li><a href="#sec-1-3-6">1.3.6. <span class="todo TODO">TODO</span> Create a vignette for this package.</a></li>
<li><a href="#sec-1-3-7">1.3.7. <span class="todo TODO">TODO</span> Include also the phenomiR database?</a></li>
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

## Changelog:<a id="sec-1-2" name="sec-1-2"></a>

-   version 0.1.0 (2014-07-29): it's done: the first *release*. Basic functionality, not Vignette, no S4 objects (yet).
-   version 0.0.1 (2014-07-28): added database.

## TODOs<a id="sec-1-3" name="sec-1-3"></a>

### DONE Create the sqlite database file from the tab delimited txt file.<a id="sec-1-3-1" name="sec-1-3-1"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-28 Mon 13:59]</span></span>

### TODO Create a class representing an interaction (?).<a id="sec-1-3-2" name="sec-1-3-2"></a>

What would be the benefits from this?
-   Object oriented approach (which would be helpful if not the only function of the package would be to fetch data).

### DONE Create some helper functions to retrieve information from the DB.<a id="sec-1-3-3" name="sec-1-3-3"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-28 Mon 15:20]</span></span>

### DONE Create a txt file with all informations (version, date etc) from the miRTarBase and read it when calling mirtarbase()<a id="sec-1-3-4" name="sec-1-3-4"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-28 Mon 15:20]</span></span>

### DONE Implement the function to get MTIs for a specified gene.<a id="sec-1-3-5" name="sec-1-3-5"></a>

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2014-07-29 Tue 10:52]</span></span>

### TODO Create a vignette for this package.<a id="sec-1-3-6" name="sec-1-3-6"></a>

### TODO Include also the phenomiR database?<a id="sec-1-3-7" name="sec-1-3-7"></a>

This makes only sense, if the phenomiR does provide additional information.