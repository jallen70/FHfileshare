

costs <- function(){
  
  source("functions/inputs.R")
  inputs <- inputs()
  
  # total cost of sequenom testing
  
  cSeq_tot = inputs$cSeq*n_seq
  cMiSeq_tot = inputs$cMiSeq*n_miseq
  
  return(list(cSeq_tot = cSeq_tot, cMiSeq_tot = cMiSeq_tot))
  
  
}