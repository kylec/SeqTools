
#' Load RSEM gene expression
#' 
#' Load RSEM gene expression
#' @param path to rsem data 
#' @return a data.frame of rsem expected counts (no transformations whatsoever). rownames are genes, colnames are samples
#' @keywords rsem
#' @export
#' @examples 
#' exprs = load_rsem("/path/rsem/")

load_rsem <- function(rsem_path) {
  # return raw counts matrix
  files = list.files(path=rsem_path, pattern="genes.results")
  
  samples = gsub(".genes.results", "", files)
  samples = gsub("Sample_", "", samples)
  
  # counts matrix
  df_count = NULL
  
  # read rsem files
  for (f in files) {
    f = read.table(paste0(path,"rsem/",f), header=T, sep="\t")
    df_count = cbind(df_count,f$expected_count)
  }
  
  colnames(df_count) = samples
  rownames(df_count) = f$gene_id
  
  # return expression dataframe  
  return(as.data.frame(df_count))
}
