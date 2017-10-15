#' Convert gene names
#' 
#' convert gene symbol between hg19, mm10, rn6
#' @param a list of genes symbols 
#' @param species from
#' @param species to 
#' @return a data frame of genes symbol lookup
#' @keywords genes 
#' @export
#' @examples 
#' genes_df = convertGeneSymbols(genes, "mm10", "hg19")

convertGeneSymbols <- function(genes, species1, species2) {
  # make connection
  connectEnsemble <- function(species) {
    if (species == "hg19") {
      human = useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", dataset = "hsapiens_gene_ensembl")
      return(list(name="hgnc_symbol", mart=human))
    } else if (species == "mm10") {
      mouse = useMart(biomart="ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset = "mmusculus_gene_ensembl")
      return(list(name="mgi_symbol", mart=mouse))  
    } else if (species == "rn6") {
      rat = useMart(biomart="ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset = "rnorvegicus_gene_ensembl")
      return(list(name="rgd_symbol", mart=rat))  
    }
  }
  #species1 = "rn6"; species2="hg19"
  species_mart1 = connectEnsemble(species1)
  species_mart2 = connectEnsemble(species2)
  genes_df = getLDS(attributes = c(species_mart1$name), filters = species_mart1$name, values = genes ,mart = species_mart1$mart, attributesL = c(species_mart2$name), martL = species_mart2$mart, uniqueRows=T)
  
  colnames(genes_df) = c("name", "name2")
  return(genes_df)
}
