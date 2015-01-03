##' Reads the 3 necessary files for digestR analysis
##'
##' @description digestR requires a dataset, a key, and a map. This function reads all of these either from user-supplied files or default examples.
##' @param data.fn
##' @param key.fn
##' @param map.fn
##'
read_digestR <- function(data.fn="data/ex_data_wide.csv",
                         key.fn="data/ex_sample_key.csv",
                         map.fn="data/pfam_fxn_map.csv") {

  # Read files
  d <- read.csv(data.fn)
  key <- read.csv(key.fn)
  map <- read.csv(map.fn)

  # Test each data frame for necessary features

  # Trim map to only necessary columns
  min.map <- map[ , c("pfam", "fxn")]

  # Melt data file, which is assumed to be wide
  dm <- melt(d, id.vars=c("pfam", "name"),
             value.name="abs.reads",
             variable.name="sample")

  # Merge data file with key
  dm_merge <- merge(dm, key, by="sample", all=TRUE)

  # Merge merged-data-and-key with functions for each pfam
  dm_mapped <- merge(dm_merge, min.map, all=TRUE)

  dm_mapped
}
