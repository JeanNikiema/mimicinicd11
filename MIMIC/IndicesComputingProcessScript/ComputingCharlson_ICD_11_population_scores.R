popC$death <- ifelse(is.na(popC$dod), 0, 1)

popC1 <- popC %>% select(icd11Code, death, id_uniq)
popC1 <- popC1 %>%
  mutate(Mi = 0, Chf = 0, Pvd = 0, Cevd = 0, Dementia = 0, 
         Cpd = 0, Rheumd = 0, Pud = 0, Mld = 0, Diab = 0, 
         Diabwc = 0, Hp = 0, Rend = 0, Canc = 0, Msld = 0, 
         Metacanc = 0, Aids = 0)
check_value <- function(dfvalue) {
  dfvalue <- dfvalue %>%
    mutate(
      Mi = ifelse(icd11Code %in% c("BA41.Z", "BA42.Z", "BA50"), 1, Mi),
      
      Chf = ifelse(icd11Code %in% c("BC20.Z", "BA01", "BD1Z", "BA01", "BA02",
                                    "BC43.Z", "BC43.01", "BC43.4", "BC20.1", "BA01",
                                    "BD1Z", "BA02", "BA51.Z", "BC43.0Z",
                                    "BC43.Z", "BC43.01", "BC43.4", "BC43.Z", "KB40.Z"), 1, Chf),
      
      Pvd = ifelse(icd11Code %in% c("EG00", "BD5Z", "8B22.5", "4A44.8", "BD4Z", "BD52.2", "DD31.Z",
                                    "BD52.2", "DD31.Z", "DA97.Z", "DB34.Z", "DA52.Z", "DD3Z",
                                    "QB50.Z", "BD40", "BD50.Z", "BD51.0", "BD51.1", "BD51.5",
                                    "BD51.4", "BD51.Z", "BD40.Z", "BD50.Z", "4A44.8",
                                    "EG00", "MB40.7", "BD5Z", "BD4Z", "BD52.2", "BD53.Z", "BD53.Z",
                                    "DD31.Z", "DA97.Z", "DB34.Z", "DA52.Z", "DD3Z",
                                    "QB50.Z", "QB50.Z"), 1, Pvd),
      
      Cevd = ifelse(icd11Code %in% c("9B74.0","9B74.0", "8B01","8B00.Z",
                                     "8B03","BD55","BD55","8B10.Z","8B1Z",
                                     "8B22","BD55", "8B25.Z", "8B10.Z",
                                     "8B26.Z","9B74.0","8B01","8B00.Z","8B0Z",
                                     "8B11","8B20","BD55","BD55","8B2Z","8B22",
                                     "8B23","8B25.Z"), 1, Cevd),
      
      Dementia = ifelse(icd11Code %in% c("6D8Z", "6D85.Y", "MB21.0","6D80.Z",
                                         "6D8Z","6D81","6D8Z","6D85","6D82","6D83",
                                         "6D70.Z","8A20","MB21.0"), 1, Dementia),
      
      Cpd = ifelse(icd11Code %in% c("BB01.1", "BB01.2","BB01.4", "BB01.0",
                                    "BB0Z" ,"BB0Z" ,  "CA20.Z" ,  "CA27.Z","CA22.Z",
                                    "CA60.1" ,"CA60.2" , "CA60.0Z" ,"CA82.1" ,"CA82.Z", 
                                    "CA23", "CA24", "CA70.Z" , "8A40.Z", "CA21.Z" ,"CA60.Z", 
                                    "CA81.0","BB0Z","CA20.Z","CA27.Z","CA20.1Z","CA21.Z",
                                    "CA22.Z","CA23","CA23.31","CA23.11","CA23.01","CA23",
                                    "CA24","CA60.1","CA60.2","CA60.0Z","CA60.Z",
                                    "CA60.Z","CA60.3","CA80.Z","CA70.Z","CA81.Z",
                                    "CA82.1","CA82.3"), 1, Cpd),
      
      Pud = ifelse(icd11Code %in% c("DA60.Z", "DA63.Z", "DA61", "DA62.Z","DA60.Z",
                                    "DA63.Z","DA61","DA62.Z"), 1, Pud),
      
      Rheumd = ifelse(icd11Code %in% c("4A44.2", "4A40.0Z","4A40.00","4A42.1" ,
                                       "4A42.2" ,"4A42.Z" ,"4A42.0" ,"4A43.Z" , 
                                       "4A43.22" ,"4A43.2","4A43.20" , "4A43.21" ,
                                       "4A41.Z","4A41.11" , "4A41.1Z"  ,"4A41.10","FA20.Z" , 
                                       "FA20.0" ,"FA20.Z" ,"FA20.0","FA23" ,"FA22" , 
                                       "4A44.0" , "CB05.1","FA20","FA20.0","FA20.Z" ,
                                       "4A44.2" ,"FA22","4A40.0Z","4A41.Z","4A42.Z",
                                       "4A42.0","4A43.Z","4A43.3","FA22",
                                       "4A44.0","4A41.00"), 1, Rheumd),
      
      Mld = ifelse(icd11Code %in% c("1E51.0Z" , "1E51.2"  ,"1E51.1"  ,
                                    "1E51.1","1E5Z"  ,"DB91.Z"  ,"DB9Z" ,
                                    "DB95.Z","DB97.Z", "DB98.0" , "DB98.8" ,
                                    "DB98.A","5C58.03" ,"DB98.2" , "DB98.B" ,"DB9Z",
                                    "DB98.8"  , "DB99"   ,"DB9Z" ,"QB63.3","1E51.Z",
                                    "DB94.0","DB94.1Z","DB94.2","DB94.3",
                                    "DB94.Z","DB95.1Z", "DB95.1Z","DB95.1Z",
                                    "DB95.5","DB97.2","DB93.Z","DB92.Z",
                                    "DB92.0", "DB92.Y","DB9Z","DB98.0","DB98.1",
                                    "DB98.A","5C58.03","DB98.2","DB98.B",
                                    "DB9Z","DB98.8","DB99","DB9Z","QB63.3"), 1, Mld),
      
      Diab = ifelse(icd11Code %in% c("5A11", "5A10", "5A24", "EB90.0",
                                     "5A10","5A23","5A20.Z","5A22.3","5A11",
                                     "5A24","EB90.0","5A12","5A13","5A23",
                                     "5A20.Z","5A22.3","5A13","5A20.Z","5A14",
                                     "5A22.Z","5A22.2","5A22.0","5A22.1","5A24"), 1, Diab),
      
      
      Diabwc = ifelse(icd11Code %in% c("9B71.0", "5A11","5A24","5A10",  "5A12",
                                       "5A13","5A14"), 1, Diabwc),
      
      Hp = ifelse(icd11Code %in% c("8B44.0Z","MB53.Z","MB53.0" ,
                                   "8D20.11", "MB5Z","MB56", "MB51.Z",
                                   "MB55.Z","MB54.Z", "MB54.0", "MB54.1" , "8B40",
                                   "8A45.00","8B44.0Z","8D20.11","8D20.0",
                                   "MB53.Z","MB53.0","MB56","MB50.Z",
                                   "MB5Z","MB51.Z","MB55.Z","MB54.Z",
                                   "MB54.0","MB54.1","MB5Z","8B40"), 1, Hp),
      
      Rend = ifelse(icd11Code %in% c("BA02", "GB40","MF8Z", "GB60.Z",
                                     "GB61.Z","GB6Z", "QB63.0", "QB42","QB94.Z",
                                     "BA02","MF8Z","GB40","GB61.Z","GB6Z",
                                     "QB94.0","QB94.1","QB94.2","QB63.0","QB42"), 1, Rend),
      
      
      Canc = ifelse(icd11Code %in% c("2B60.Z","2B62.Z","2B68.Z","2B63.Z",
                                     "2B64.Z","2B64.Z","2B66.Z","2B6A.Z","2B6B.Z",
                                     "2B6D.Z","2B6E.Z","2B70.Z","2B72.Z","2B80.0Z",
                                     "2B90.Z","2B90.Z","2B91.Z","2C12.Z","2C13.Z",
                                     "2C10.Z","2C50.Z","2C51.Z","2C5Z","2C11.Z",
                                     "2C20.Z","2C23.Z","2C23.Z","2C20.Z","2C26.Z",
                                     "2C27.Z","2C29.Z","2B5Z","2B5J","2B5Z","2B5K",
                                     "2C30.Z","2C30.Z&XH4846", "2C30.Z","2B57.Z",
                                     "2B57.Z","2C6Z","2C78","2C77.Z","2C77.Z",
                                     "2C75.Z","2C76.Z","2C73.0","2C7Z","2C82.Z",
                                     "2C80.Z","2C80.Z","2C80.Z","2C94.Z","2C94.Z",
                                     "2C90.Z","2D0Z","2A00.11","2A00.00","2A00.5","2A00.5",
                                     "2A02.1Z","2A02","2D10.Z","2D12.Z","2D42","2A81.Z",
                                     "2B30.Z","2B30.Z","2A83.1","2B33.3",
                                     "2B33.3","2B33.1","2A60.3Z","2A60.3Z","2A60.0","2A61",
                                     "2B3Z","2B33.4","2B60.Z","2B61.Z","2B62.Z","2B63.Z",
                                     "2B64.Z","2B65.Z","2B66.Z","2B67.Z","2B68.Z",
                                     "2B69.Z","2B6A.Z","2B6B.Z","2B6C.Z","2B6D.Z",
                                     "2B6E.Z","2B70.Z","2B72.Z","2B80.0Z","2B80.Z",
                                     "2B90.Z","2B91.Z","2B92.Z","2C00.Z","2C12.Z",
                                     "2C13.Z","2C17.Z","2C10.Z","2C11.Z","2C20.Z","2C21.Z",
                                     "2C2Z","2C22.Z","2C23.Z","2C24.Z","2C25.Z",
                                     "2C27.Z","2C28.Z","2C29.Z","2B5Z","2B5Z",
                                     "2C30.Z","2C26.0","2C53.1","2C51.2Z","2D3Z","2B57.Z",
                                     "2C4Z","2C50.Z","2C51.Z","2C5Z","2B5K","2C6Z","2C65",
                                     "2C70.Z","2C71.Z","2C77.Z","2C76.Z","2C78","2C73.Z",
                                     "2C7Z","2C75.Z","2C81.Z","2C82.Z","2C80.Z",
                                     "2C84","2C90.Z","2C91.Z","2C92.Z","2C94.Z",
                                     "2C9Z","2D0Z","2A01.00","2A01.1","2A00.11",
                                     "2A00.00","2A00.5","2A02","2A0Z","2A02.00",
                                     "2D10.Z","2D11.Z","2D12.Z","2D4Z","2D42","2B30.Z",
                                     "2A80.Z","2A8Z","2B2Z","2A8Z","2A84.Z","2A83.Z",
                                     "2B33.3","2B33.1","2B33.1","2A61","2B3Z","2B33.4",
                                     "2B3Z","2D43"), 1, Canc),                                                                                               
      
      Msld = ifelse(icd11Code %in% c("DA26.00", "DA26.0Z","DA26.00","DB99.7","DB99.7",
                                     "DB91.Z","DB99.8","DB9Z","DB98.7Z","DB99.2",
                                     "DB99.7","DA26.00","DA26.01", "DA43.0",
                                     "DA26.01","DB94.Z","DB95.0","DB99.8","DB99.7",
                                     "DB91.Z","DB98.6","DB98.7Z","DB99.2"), 1, Msld),
      
      
      Metacanc = ifelse(icd11Code %in% c("2D6Z", "2D6Z", "2D7Z","2E2Z", "2E0Y",
                                         "2D4Z","DA26.00","DA26.01","DA43.0",
                                         "2D6Z","2D7Z","2E2Z","2D4Z"), 1, Metacanc), 
      
      Aids = ifelse(icd11Code %in% c("2D6Z", "2D6Z", "2D7Z","2E2Z", "2E0Y",
                                     "2D4Z","DA26.00","DA26.01","DA43.0",
                                     "2D6Z","2D7Z","2E2Z","2D4Z"), 1, Aids)
      
      
      
    )
  return(dfvalue)
}


popC_1 <- check_value(popC1)

popC2 <- popC_1[ , !(names(popC_1) %in% "icd11Code")]

popC2 <- aggregate(. ~ id_uniq, popC2, sum)
popC2[-1] <- lapply(popC2[-1], function(x) ifelse(x != 0, 1, 0))

# Save popC2 to a CSV file
write.csv(popC2, file = "popCCharlson.csv", row.names = FALSE)

# Add the "comorbidity" class to the popC2 object
class(popC2) <- c("comorbidity", class(popC2))


# Rename columns
popC_charlson <- popC2 %>%
  select(-death) %>%
  rename("mi" = "Mi", "chf" = "Chf", "pvd" = "Pvd",
         "cevd" = "Cevd", "dementia" = "Dementia", "cpd" = "Cpd",
         "rheumd" = "Rheumd", "pud" = "Pud", "mld" = "Mld", "diab" = "Diab",
         "diabwc" = "Diabwc", "hp" = "Hp", "rend" = "Rend", "canc" = "Canc",
         "msld" = "Msld", "metacanc" = "Metacanc", "aids" = "Aids")

# Add a "map" attribute with value "charlson_icd10_quan"
attr(x = popC_charlson, which = "map") <- "charlson_icd10_quan"



# Check for the "comorbidity" class
class(popC_charlson)

# Calculate Charlson scores
popC_charlson_score <- score(x = popC_charlson, weights = "quan", assign0 = FALSE)
length(popC_charlson_score)

print(popC_charlson_score)

# Create a summary table
resume_scorepopC <- table(popC_charlson_score) %>% as.data.frame()
print(resume_scorepopC)

# Add the scores to the IDs
scorepopC <- popC_charlson %>% mutate(score_popC = popC_charlson_score)


