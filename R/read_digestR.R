##' Reads the 3 necessary files for digestR analysis
##'
##' @description digestR requires a dataset, a key, and a map. This function reads all of these either from user-supplied files or default examples.
##' @param data.fn
##' @param key.fn
##' @param fxn.map.fn
##' @export
read_digestR <- function(data.fn=NULL,
                         key.fn=NULL,
                         fxn.map.fn=NULL,
                         normalizer="sum.reads") {

  ## Read files
  #d <- read.csv(data.fn)
  #key <- read.csv(key.fn)
  #map <- read.csv(map.fn)

  # Read pfam abundance
  if(is.null(data.fn)) {
    d <- ex_data_wide
  } else {
    d <- read.csv(data.fn)
  }

  # Read sample key
  if(is.null(key.fn)) {
    key <- ex_sample_key
    if(!is.null(data.fn)) {
      warning("You are using your own data but the default sample key. Are you sure you mean to do that?")
    }
  } else {
    key <- read.csv(key.fn)
  }

  # Read the map
  if(is.null(fxn.map.fn)) {
    fxn_map <- pfam_fxn_map
  } else {
    fxn_map <- read.csv(fxn.map.fn)
  }
browser()
  # Test each data frame for necessary features

  # Trim map to only necessary columns
  min.map <- fxn_map[ , c("pfam", "fxn")]

  # Melt data file, which is assumed to be wide
  dm <- melt(d, id.vars=c("pfam", "name"),
             value.name="abs.reads",
             variable.name="sample")

  # Merge data file with key
  dm_merge <- merge(dm, key, by="sample", all=TRUE)

  # Merge merged-data-and-key with functions for each pfam
  dm_mapped <- merge(dm_merge, min.map, all=TRUE)

  # Remove na cases
  dm_mapped <- dm_mapped[!is.na(dm_mapped$abs.reads), ]

  # Calculate relative reads
  #dm_mapped$frac.reads <- dm_mapped$abs.reads/dm_mapped$sum.reads
  dm_mapped$frac.reads <- dm_mapped$abs.reads/dm_mapped[ , normalizer]

  dm_mapped
}
