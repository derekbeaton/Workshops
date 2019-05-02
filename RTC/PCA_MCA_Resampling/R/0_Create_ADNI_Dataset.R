library(tidyverse)
library(ADNIMERGE)

adnimerge %>%
  dplyr::select(RID, VISCODE, DX, AGE, PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, APOE4, FDG, AV45, CDRSB, ADAS13, MOCA, WholeBrain, Hippocampus, MidTemp, mPACCtrailsB) %>%
  filter(VISCODE == "bl") %>%
  filter(MOCA >= 16) %>%
  drop_na() -> amerge_subset

## bring in modified hachinksi 
amerge_subset <- inner_join(amerge_subset, modhach[,c("RID","HMSCORE")])

## clean some of this up...
amerge_subset$PTEDUCAT <- ifelse(amerge_subset$PTEDUCAT<=12, 12, amerge_subset$PTEDUCAT)
amerge_subset$PTETHCAT <- recode(amerge_subset$PTETHCAT, `Hisp/Latino`= "Hisp/Latino",  .default = "Not Hisp/Latino")
amerge_subset$PTRACCAT <- recode(amerge_subset$PTRACCAT, White="White", Black="Black", Asian="Asian", .default = "Other")
amerge_subset$CDRSB <- ifelse(amerge_subset$CDRSB>=5.5, 5.5, amerge_subset$CDRSB)
amerge_subset$HMSCORE <- ifelse(amerge_subset$HMSCORE>=3, 3, amerge_subset$HMSCORE)

## take a quick look at what these originally were:
unlist(lapply(amerge_subset,function(x){y<-class(x); y[length(y)]}))

## manually change types for now
amerge_subset$RID <- as.character(amerge_subset$RID)
amerge_subset$VISCODE <- as.character(amerge_subset$VISCODE)
amerge_subset$DX <- as.character(amerge_subset$DX)
amerge_subset$AGE <- as.numeric(amerge_subset$AGE)
amerge_subset$PTGENDER <- as.character(amerge_subset$PTGENDER)
amerge_subset$PTEDUCAT <- as.numeric(amerge_subset$PTEDUCAT)
amerge_subset$PTETHCAT <- as.character(amerge_subset$PTETHCAT)
amerge_subset$PTRACCAT <- as.character(amerge_subset$PTRACCAT)
amerge_subset$APOE4 <- as.numeric(amerge_subset$APOE4)
amerge_subset$FDG <- as.numeric(amerge_subset$FDG)
amerge_subset$AV45 <- as.numeric(amerge_subset$AV45)
amerge_subset$CDRSB <- as.numeric(amerge_subset$CDRSB)
amerge_subset$ADAS13 <- as.numeric(amerge_subset$ADAS13)
amerge_subset$MOCA <- as.numeric(amerge_subset$MOCA)
amerge_subset$WholeBrain <- as.numeric(amerge_subset$WholeBrain)
amerge_subset$Hippocampus <- as.numeric(amerge_subset$Hippocampus)
amerge_subset$MidTemp <- as.numeric(amerge_subset$MidTemp)
amerge_subset$mPACCtrailsB <- as.numeric(amerge_subset$mPACCtrailsB)
amerge_subset$HMSCORE <- as.numeric(amerge_subset$HMSCORE)


rownames(amerge_subset) <- amerge_subset$RID
amerge_subset <- amerge_subset[,-c(1:2)]

## make a map of variables to types
variable_type_map <- matrix(0, nrow=ncol(amerge_subset), ncol = 3)
colnames(variable_type_map) <- c("Continuous","Categorical","Ordinal")
rownames(variable_type_map) <- colnames(amerge_subset)


  ## some *very* good arguments can (should) be made that these are not necessarily continuous.
    ## some are counts or count-like and most are by definition strictly non-negative.
likely_continuous_items <- c("AGE","FDG","AV45","WholeBrain","Hippocampus","MidTemp","mPACCtrailsB")
strictly_categorical_items <- c("DX","PTGENDER","PTETHCAT","PTRACCAT")
strictly_ordinal_items <- c("PTEDUCAT","MOCA","ADAS13","CDRSB")
categorical_or_ordinal_items <- c("APOE4","HMSCORE")
  
variable_type_map[likely_continuous_items,"Continuous"] <- 1
variable_type_map[strictly_categorical_items,"Categorical"] <- 1
variable_type_map[strictly_ordinal_items,"Ordinal"] <- 1
variable_type_map[categorical_or_ordinal_items,"Categorical"] <- 1
variable_type_map[categorical_or_ordinal_items,"Ordinal"] <- 1

save(amerge_subset, file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
save(variable_type_map, file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))
