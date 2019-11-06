#' Translate DNA or RNA to amino acide
#'
#' @param x a text string containing a DNA or RNA sequence
#' @return A text string that represents the translated amino acid sequence.
#' @examples Translate(x)
#' x <- "ATGTCCTAG"
#' Translate(x)
#' @export
#'
Genseq <- function(x) {

x <- "ATGtacacagctaaaGAacggauuuctacacatGAAGgtagagaggccacagagagcacguag"
if(nchar(x)%%3 != 0){
  warning("DNA sequence is not a multiple of 3 and the translation is assumed to start from the first base.")}
x <- toupper(x)
dna <- gsub("U", "T", x)
ctable <- matrix(c("GCT", "GCC", "GCA", "GCG", "CGT", "CGC", "CGA", "CGG", "AGA", "AGG", "AAT", "AAC", "GAT", "GAC", "TGT", "TGC", "CAA", "CAG", "GAA", "GAG", "GGT", "GGC", "GGA", "GGG",
                   "CAT", "CAC", "ATT", "ATC", "ATA", "ATG", "TTA", "TTG", "CTT", "CTC", "CTA", "CTG", "AAA", "AAG", "TTT", "TTC", "CCT", "CCC", "CCA", "CCG", "TCT", "TCC", "TCA", "TCG",
                   "AGT", "AGC", "ACT", "ACC", "ACA", "ACG", "TGG", "TAT", "TAC", "GTT", "GTC", "GTA", "GTG", "TAA", "TGA", "TAG", "A", "A", "A", "A", "R", "R", "R", "R", "R", "R", "N",
                   "N", "D", "D", "C", "C", "Q", "Q", "E", "E", "G", "G", "G", "G", "H", "H", "I", "I", "I", "M", "L", "L", "L", "L", "L", "L", "K", "K", "F", "F", "P", "P", "P", "P", "S",
                   "S", "S", "S", "S", "S", "T", "T", "T", "T", "W", "Y", "Y", "V", "V", "V", "V", "*", "*", "*"), ncol = 2, nrow = 64)
codtable <- data.frame(ctable)
rownames(codtable) <- codtable[,1]
codtable[,1] <- NULL
output_c <- vector('character')
for(i in seq(from=1, to=nchar(x), by=3))
  output_c = c(output_c, as.character(codtable[substring(x,i,i+2),]))
AA <- output_c[!is.na(output_c)]
paste(AA, sep = "", collapse = "")

}
