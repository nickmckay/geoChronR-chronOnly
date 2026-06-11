getExports <- function(path){
  ns <- readLines(path)
  ex <- grep("^export\\(", ns, value = TRUE)
  gsub("^export\\(|\\)$", "", ex)
}
ensExports <- getExports("/Users/nicholas/GitHub/ens/NAMESPACE")
vizExports <- getExports("/Users/nicholas/GitHub/lipdViz/NAMESPACE")
header <- c(
  "# Re-exports: geoChronR 2.x is a metapackage for age modeling that re-exports",
  "# the full APIs of the ens (ensemble methods) and lipdViz (visualization)",
  "# packages, so code written against geoChronR 1.x keeps working unchanged.",
  "# This file is generated from the NAMESPACE files of those packages.", "")
block <- function(fun, pkg){
  name <- if (grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", fun)) fun else sprintf("`%s`", fun)
  c("#'", "#' @export", sprintf("%s::%s", pkg, name), "")
}
out <- c(header,
         unlist(lapply(ensExports, block, pkg = "ens")),
         unlist(lapply(vizExports, block, pkg = "lipdViz")))
writeLines(out, "R/reexports.R")
cat("ens:", length(ensExports), " lipdViz:", length(vizExports), "re-exported\n")
