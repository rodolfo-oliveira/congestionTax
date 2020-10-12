#calculating individual share of family income


ind_share_family_income <- function(database){

  database$ren_fam_ind <- 0 
  for(k in 1:length(unique(database$ID_FAM))){
    database[database$ID_FAM == unique(database$ID_FAM)[k],]$ren_fam_ind <- database$RENDA_FA[database$ID_FAM == unique(database$ID_FAM)[k]] / length(database[database$ID_FAM == unique(database$ID_FAM)[k],]$ZONA)
    print(paste0("Andamento: ", round(100*k/length(unique(database$ID_FAM)),digits = 3),"%"))
    }
  return(database)
}