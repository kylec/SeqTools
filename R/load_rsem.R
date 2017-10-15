
#' Load RSEM gene expression
#' 
#' Load RSEM gene expression
#' @param path to rsem data 
#' @param count type: "expected_count"(default), "tmp", "fpkm"
#' @return a data.frame of rsem expected counts (no transformations whatsoever). rownames are genes, colnames are samples
#' @keywords rsem
#' @export
#' @examples 
#' exprs = load_rsem("/path/rsem/")

load_rsem <- function(rsem_path, count_type=NULL) {
  # return raw counts matrix
  files = list.files(path=rsem_path, pattern="genes.results")
  
  samples = gsub(".genes.results", "", files)
  samples = gsub("Sample_", "", samples)
  
  # counts matrix
  df_count = NULL
  
  # read rsem files
  for (f in files) {
    f = read.table(paste0(rsem_path,"/",f), header=T, sep="\t")
    # default count type is expected count
    if (is.null(count_type)) {
      df_count = cbind(df_count,f$expected_count)
    } else if (count_type == "tpm") {
      df_count = cbind(df_count,f$TPM)
    } else if (count_type == "fpkm") {
      df_count = cbind(df_count,f$FPKM)
    } else {
      stop(paste0("Incorrect count_type:", count_type))
    }
  }
  
  colnames(df_count) = samples
  rownames(df_count) = f$gene_id
  
  # return expression dataframe  
  return(as.data.frame(df_count))
}
