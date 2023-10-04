popC$death <- ifelse(is.na(popC$dod), 0, 1)

popCElix1 <- popC %>% select(icd11Code, death, id_uniq)
popCElix1 <- popCElix1 %>%
  mutate(Chf = 0, Carit = 0, Valv = 0, Pcd = 0, Pvd = 0, 
         Hypunc = 0, Hypc = 0, Para = 0, Ond = 0, Cpd = 0, 
         Diabunc = 0, Diabc = 0, Hypohthy = 0, Rf = 0, Ld = 0,
         Pud = 0, Aids = 0, Lymph = 0, Metacanc = 0, Solidtum = 0,
         Rheumd = 0, Coag = 0, Obes = 0, Wloss = 0, Fed = 0,
         Blane = 0, Dane = 0, Alcohol = 0, Drug = 0, Psycho = 0,
         Depre = 0)
check_value <- function(dfvalue) {
  dfvalue <- dfvalue %>%
    mutate(
      Chf = ifelse(icd11Code %in% c("BC20.1","BA01","BD1Z",  "BA02",
                                    "BA51.Z","BC43.0Z", "BC43.Z", "BC43.01", "BC43.4",
                                    "BC43.Z", "KB40.Z", "BC20.Z" ,"BA01","BA01/BD1Z","BA01",
                                    "BA01/BD1Z/BA02" , "BA02", "BC43.Z","BC43.Z","BC43.01","BC43.Z",
                                    "BC43.4","BD1Z"), 1, Chf),
      
      Carit = ifelse(icd11Code %in% c("BC63.1Z","BC63.2Z","BC63.Z","BC81.4","BC63.Z","BC63.Z","MC81.3",
                                      "BC81.Z","BC71.1","BC60","BC61","BC70","BC80.20","BC81.Z","BC65.3",
                                      "BC65.Z","BC65.1","BC65.4","BC62","BC65.0", "BC71.Z","BC81.6", 
                                      "BC65.2","BC8Z","BC90","BC64","BE2Z","MC81.Z","MC81.0","MC81.1",
                                      "BC80.1","MC81.Z","NE82.00", "NE82.02","NE82.12" ,"NE82.20","NE82.0Z",
                                      "NE82.21" ,"NE82.22","NE82.Z","NE82.3","NE82.11" ,"NE82.10","NE82.01",
                                      "NE82.03","NE82.1Z","QB30.2Z","BC91","QB50.0Z","QB50.00","BC81.0",
                                      "BC81.7Z","BC81.2Z","BC81.8","BC81.21","BC81.20","BC81.1",
                                      "BC81.5","BC71.0Z","BC65.5","MC81.3","MC81.3","BC81.Z","BC71.1",
                                      "BE2Z","BE2Z","BC65.3","BC65.Z","BC65.1","BC65.4","BC62","BC65.0",
                                      "BC71.Z","BC81.6","BC65.2","BC8Z","BC90","BE2Z","BC64", 
                                      "BE2Z","MC81.Z","MC81.0","NE82.02","NE82.12","NE82.0Z",
                                      "NE82.21","NE82.22" ,"NE82.Z","NE82.3" ,"NE82.11","NE82.10",
                                      "NE82.01","NE82.03","NE82.1Z","NE82.02","NE82.12","NE82.20","NE82.21",
                                      "NE82.22" ,"NE82.Z","NE82.3","NE82.11","NE82.10","NE82.01","NE82.03"  ,
                                      "NE82.1Z", "NE82.00","NE82.02",  "NE82.12", "NE82.20" ,"NE82.0Z",
                                      "NE82.21","NE82.22","NE82.Z","NE82.11","NE82.10","NE82.01","NE82.03",
                                      "NE82.1Z","NE82.00"  ,"NE82.12" ,"NE82.20"  ,"NE82.21","NE82.22",
                                      "NE82.Z","NE82.3","NE82.11","NE82.10"   ,"NE82.01"  ,"NE82.03",
                                      "NE82.1Z"  ,"NE82.00","NE82.02"  ,"NE82.12","NE82.20","NE82.0Z" ,
                                      "NE82.21","NE82.22","NE82.Z","NE82.11","NE82.10","NE82.01",
                                      "NE82.03","NE82.1Z","NE82.00","NE82.02","NE82.12","NE82.20","NE82.0Z"
                                      ,"NE82.21","NE82.22","NE82.3","NE82.11","NE82.10","NE82.01","NE82.03",
                                      "NE82.1Z","QB50.Z","QB30.2Z","BC91"), 1, Carit),
      
      
      Valv = ifelse(icd11Code %in% c("1A62.1","BB6Z","BB7Z","BC0Z","BB8Z",
                                     "BC20.0","BC20.Z","BC00","BB6Z","BC00",
                                     "BC0Z","BB7","BB8Z","BC00","BB9Z","BC0Z","BB4Z",
                                     "LA8A.20","LA8A.21","LA87.11","LA89.2","LA87.10",
                                     "QB50.2","QB50.3","QB50.Z","BC01","BB70.0" ,"BC00",
                                     "BB8Z","BB61.Z","BB63.1","BC01","BB63.Z"), 1, Valv),
      
      Pcd = ifelse(icd11Code %in% c("BB00.Z","BB01.Z","BB01.0","BE2Z","BB01.1","BB01.2" ,
                                    "BB01.4","BB0Z","BB02.0","4A44.Z",
                                    "QB50.3","QB50.2","BB00.Z","BB01.0","BB00.Z","BB0Z",
                                    "BB02.0","4A44.Z","BB02"), 1, Pcd),
      
      Pvd = ifelse(icd11Code %in% c("BD40.Z","BD50.Z","4A44.8","EG00","MB40.7",
                                    "BD5Z","BD4Z","BD52.2", "BD53.Z","BD53.Z",
                                    "DD31.Z","DA97.Z","DB34.Z","DA52.Z","DD3Z",
                                    "DD3Z","QB50.Z","QB50.Z","1A62.1", "8B22.5",
                                    "BA52.Z","4A44.8","EG00","MB40.7","BD5Z",
                                    "EG00","MB40.7","BD5Z","BD4Z","BD52.2",
                                    "DD31.Z","DA97.Z","DB34.Z"), 1, Pvd),
      
      Hypunc = ifelse(icd11Code %in% c("BA00.Z"), 1, Hypunc),
      
      
      Para = ifelse(icd11Code %in% c("8A45.00","8B44.0Z","8D20.11","8D20.0","MB53.Z","MB53.0",
                                     "MB56","MB50.Z","MB5Z","MB51.Z","MB55.Z","MB54.Z","MB54.0",
                                     "MB54.1","MB5Z","8B40","MB5Z","8B44.0Z",
                                     "MB53.Z","MB53.0","8D20.11","MB5Z",
                                     "MB56","MB56","MB51.Z","MB55.Z","MB54.Z",
                                     "MB54.0","MB54.1","MB54.1","MB5Z","8B40"), 1, Para),
      
      
      Ond = ifelse(icd11Code %in% c("8A01.10","8A03.1Z" ,"8B61.Z","8D87.0Z",
                                    "8E4A.1","8E4A.3","8A00.0Z","LD90.1","8A0Z","8A20",
                                    "8A00.2Z","8A00.2Z","8A01.Z","8A01.2Z","8A01.0",
                                    "8A0Z", "8D44.Z","8B44.0Z","8A21.0","5C53.24","8E4A.0",
                                    "8A2Z","8A2Z","8B44.Z","8A40.Z","8A42.Z","8A41.Z",
                                    "8A4Z","8A4Z","8A6Z", "8A66.Z","8B24.Z","8B24.0","8E47",
                                    "8E4A.0","8E63","MA80.0","MA80.1","MA80.Z","8A68.Z","8A2Z",
                                    "8A00.0Z","LD90.1","8A0Z","8A20","8A00.24","8A00.24","8A00.2Z",
                                    "8A00.2Z","8A00.2Z","8A01.10","8A01.Z","8A01.2Z","8A01.0","8A0Z" ,
                                    "8A0Z","8A00.2Z","8D87.01","8B61.0","8B44.Z","8A40.Z",
                                    "8A4Z","8A6Z","8B24.Z","8B24.0","8E47","8A68.Z","MA80.0"), 1, Ond),
      
      
      Cpd = ifelse(icd11Code %in% c("BB0Z","BB0Z","CA20.Z","CA27.Z","CA20.1Z",
                                    "CA21.Z","CA22.Z","CA23","CA23.31","CA23.11","CA23.01",
                                    "CA23","CA24","CA60.1","CA60.2","CA60.0Z","CA60.Z","CA60.3",
                                    "CA80.Z","CA70.Z","CA81.Z","CA82.1","CA82.3","BB01.1",
                                    "BB01.2","BB01.4","BB01.0","CA20.Z","CA27.Z","CA20.1Z",
                                    "CA21.Z","CA21.Z","CA23","CA23.32","CA24","CA24","CA70.Z",
                                    "CA22.Z","CA60.1","CA60.2","CA60.0Z","CA60.0Z","CA60.4",
                                    "CA60.5","CA60.6","CA60.7","CA60.8","CA60.9","CA60.Z","CA60.Z",
                                    "CA80.0","CA80.1","CA80.2","CA80.Z","CA60.Z",
                                    "CA81.Z","CA82.1","CA82.Z"), 1, Cpd),
      
      Diabunc = ifelse(icd11Code %in% c("5A10","5A23","5A20.Z","5A22.3","5A10","5A10",
                                        "5A11","5A12","5A12","5A12","5A13","5A23","5A20.Z",
                                        "5A22.3","5A13","5A13","5A14","5A23","5A20.Z",
                                        "5A22.3","5A14","5A22.Z","5A22.2","5A22.0","5A22.1","5A14",
                                        "EB90.0","5A24"), 1, Diabunc),
      
      Diabc = ifelse(icd11Code %in% c("5A10","EB90.0","5A24","5A10",  "EB90.0","5A24",
                                      "5A24","5A12","5A13" , "5A14","5A00.1Z","5A01.Z"), 1, Diabc),
      
      Hypohthy = ifelse(icd11Code %in% c("5A00.04","5A00.1Z","5A00.22","5A00.Z","5D40.Z" ,
                                         "5A00.01","5D40.Z","5A00.00","5A0Z"), 1, Hypohthy),
      
      Rf = ifelse(icd11Code %in% c("BA02","BA02","GB61.Z","GB6Z",
                                   "GB61.Z","QB94.0","QB94.1","QB94.2","QB63.0","QB42"), 1, Rf),
      
      Ld = ifelse(icd11Code %in% c("1E51.Z","DA26.0Z","DA43.0","DA26.01",
                                   "DB94.Z","DB95.0","DB95.1Z","DB95.1Z","DB95.1Z",
                                   "DB95.5","DB91.Z","DB99.8","DB99.7","DB9Z","DB97.2",
                                   "DB93.Z","DB92.Z","DB92.0","DB92.Y","DB9Z","DB98.0",
                                   "DB98.1","DB98.6","DB98.7Z","DB99.2","DB98.A","5C58.03",
                                   "DB98.2","DB98.B","DB9Z","DB98.8","DB99","DB9Z","QB63.3",
                                   "BA02"   ,        "BA02"    ,      "GB61.Z"  ,       "GB61.Z"     ,
                                   "GB6Z","GB61.Z","QB63.0","QB42","QB94.Z","1E51.0Z","1E51.2",
                                   "1E51.1","1E5Z","DA26.00","DA26.00","DB94.Z","DB99.7","DB99.7","DB91.Z" ,
                                   "DB99.8","DB99.7","DB9Z","DB98.7Z","DB99.2"), 1, Ld),
      
      Pud = ifelse(icd11Code %in% c("DA60.Z"   ,"DA60.Z"  ,"DA63.Z"  ,  
                                    "DA63.Z"  ,"DA61", "DA61",  
                                    "DA62.Z" , "DA62.Z"), 1, Pud),
      
      Aids = ifelse(icd11Code %in% c("1C62.Z","1C62.1"), 1, Aids),
      
      Lymph = ifelse(icd11Code %in% c("2B30.Z",    "2A80.Z",
                                      "2A8Z"     ,"2B2Z",
                                      "2A8Z" ,"2A84.Z", 
                                      "2B3Z" ,"2A83.1","2A83.3"), 1, Lymph),
      
      Metacanc = ifelse(icd11Code %in% c("2D6Z" ,"2D7Z",
                                         "2E2Z","2D4Z"), 1, Metacanc),
      
      Solidtum = ifelse(icd11Code %in% c("2B60.Z"  ,       "2B60.Z"    ,    "2B62.Z"  ,
                                         "2B68.Z"   ,      "2B63.Z"   ,      "2B63.Z"  ,
                                         "2B64.Z"      ,   "2B64.Z"  ,       "2B66.Z"  ,
                                         "2B6A.Z"    ,     "2B6A.Z"   ,      "2B6B.Z"   ,
                                         "2B6B.Z"   ,      "2B6D.Z"   ,      "2B6D.Z"  ,
                                         "2B6E.Z"    ,    "2B70.Z"   ,      "2B70.Z"  ,
                                         "2B72.Z"     ,    "2B72.Z"   ,      "2B80.0Z"    ,    "2B90.Z"  ,
                                         "2B90.Z"    ,    "2B91.Z"   ,      "2C12.Z"   ,      "2C13.Z"   ,
                                         "2C10.Z"     ,    "2C10.Z"    ,     "2C50.Z"   ,      "2C51.Z"  ,
                                         "2C5Z"     ,      "2C11.Z"   ,      "2C20.Z"     ,    "2C23.Z"     ,
                                         "2C23.Z"       ,  "2C26.Z"    ,     "2C27.Z"  ,
                                         "2C29.Z"     ,    "2B5Z"       ,    "2B5J" ,
                                         "2B5Z"        ,   "2B5K" ,          "2C30.Z" ,
                                         "2C30.Z&XH4846", "2C30.Z"  ,       "2C6Z"    ,
                                         "2C6Z"     ,      "2B57.Z"   ,      "2B57.Z"   ,
                                         "2C78"     ,      "2C77.Z"  ,  "2C77.Z"     ,
                                         "2C75.Z"     ,    "2C76.Z"    ,     "2C76.Z"      ,
                                         "2C73.0"      ,   "2C7Z"     ,      "2C82.Z"  ,
                                         "2C80.Z"      ,   "2C80.Z"    ,     "2C80.Z"    ,
                                         "2C81.Z"    ,     "2C94.Z"    ,     "2C94.Z"   ,
                                         "2C90.Z"      ,  "2D0Z"    ,       "2A00.11"   ,     "2A00.00"   ,
                                         "2A00.5"       ,  "2A00.5"     ,    "2A02.1Z"   ,
                                         "2A02"       ,   "2D10.Z"     ,    "2D12.Z"      ,   "2D42"   ,
                                         "2D4Z"     ,      "2D42"), 1, Solidtum),
      
      Rheumd = ifelse(icd11Code %in% c("4A44.Z", "EB60"  ,"EB61.0",  
                                       "EB61"  , "EB61.0", "EB61", 
                                       "EB61.1","EB90.40","EB90.4",
                                       "4A41.0Z","EK91.1", "EM0Z", 
                                       "4A40.0Z","4A40.00", "4A40.0Z",
                                       "4A40.0Z","4A42.1","4A42.2","4A42.Z","4A42.0" ,  
                                       "4A43.Z","4A43.22","4A43.2", "4A43.20", 
                                       "4A43.21", "4A43.Z","4A43.22","4A43.2"   ,  
                                       "4A43.20" , "4A43.21"  ,"4A41.Z","4A41.Z",  
                                       "4A41.Z",  
                                       "4A41.11"   ,"4A41.1Z"  ,"4A41.10","4A41.11" ,
                                       "4A41.1Z" , "4A41.10" , "FB51.Z","4A4Z" , 
                                       "4A62" , "FA20.Z" , "FA27.2" ,
                                       "FA27.2" ,  "FA92.0Z" ,  "FA22" ,  "4A44.0" , "LD28.1" , 
                                       "FB32"    , "EF00.Z"   ,"EB90.41",
                                       "KC22.1"   , "EF00.0" , "5C5A"), 1, Rheumd),
      
      Coag = ifelse(icd11Code %in% c("3B20","3B10.Z","3B11.Z","3B61.Z", "3B6Z"  ,
                                     "3B62.Z","3B64.10", "3B64.Z" ,  
                                     "3B64.11","4A85.02","3B64.1"  ,
                                     "3B64.13" ,"3B64.12", "3B64.Z"), 1, Coag),
      
      Obes = ifelse(icd11Code %in% c("5B81.Z"), 1, Obes),
      
      Wloss = ifelse(icd11Code %in% c("5B52","5B7Z","5B51","5B7Z",
                                      "5B71","5B71","5B53","5B71","5B54","5B51","5B50",
                                      "5B53","5B52","MG43.5","MG20.Z" ), 1, Wloss),
      
      Fed = ifelse(icd11Code %in% c("5A60.2Z"    ,    "5C70.Z"    ,     "5C7Z", 
                                    "5A60.2Z"    ,    "5C71"), 1, Fed),
      
      Blane = ifelse(icd11Code %in% c("3A00.0Z"), 1, Blane),
      
      
      Dane = ifelse(icd11Code %in% c("3A00.0Z","3A00.Z" ,
                                     "3A00.Z","3A01.Z", "3A02.Z", "3A03"), 1, Dane),
      
      Alcohol = ifelse(icd11Code %in% c("5B5C" ,"6C40.Z","6C40.2Z",
                                        "8D44.Z","6C40.2Z" , "8D44.Z" ,
                                        "6C40.2Z" ,   "8D44.Z","6C40.1Z","8D44.0",
                                        "DA42.80" ,   "DB94.0", "DB94.1Z",
                                        "DB94.3" ,   "DB94.3", "DB94.Z",
                                        "NE61" , "NE61"  , "QE8Z",
                                        "QE4Z"  ,"6C40.Z" , "5B5C" , "8D44.0" ,
                                        "DA42.80"  , "DB94.0" , "DB94.3",
                                        "DB94.Z" ,"NE61" , "QB95.2",
                                        "QA11" , "QE10"), 1, Alcohol),
      
      Drug = ifelse(icd11Code %in% c("6C43.Z",  "6C41.Z"   ,      "6C44.Z"   ,      "6C45.Z" ,
                                     "6C48.Z"      ,   "6C4C.Z"  ,       "6C49.Z"  ,      "6C4B.Z"  ,
                                     "6C4E.Z"      ,   "6C4G.Z"   ,      "6C4H.Z"  ,       "6C4D.Z"  ,
                                     "6C4Z"       ,    "QA12" ,  "QE11.Z", "6C4E.Z" , 
                                     "6C43.2Z"   ,     "6C41.1Z"   ,     "6C42.1Z" ,  "6C42.11" ,
                                     "6C42.10"       , "6C42.0"  ,  "6C49.1Z"   ,     "6C43.1Z"   ,
                                     "6C45.1Z"     ,   "6C48.1Z"   ,     "6C4C.1Z"    ,    "6C46.1Z"    ,
                                     "6C48.Z"     ,   "6C4D.1Z"   ,     "6C47.0"  ,       "6C47.11"  ,
                                     "6C47.1Z"    ,    "6C4G.1Z"   ,     "6C47.10"   ,     "6C4E.1Z" ,
                                     "6C4Z"       ,    "6C4B.1Z"   ,     "QA11"), 1, Drug),
      
      Psycho = ifelse(icd11Code %in% c("6E61.1" , "6E6Z", "6A20.Z",
                                       "6A20.Z",  "6A60.1" , "6A8Z",
                                       "6A60.1", "6A60.7" , "6A24.Z"  ,
                                       "6A70.4", "6A24.Z", "6A23.Z","6A24.Z",
                                       "6A21.Z","6A2Z" ,"6A2Z","6A60.1",
                                       "6A8Z","6A60.1","6A60.7"), 1, Psycho),
      
      Depre = ifelse(icd11Code %in% c("6A20.Z", "6A25.2"  ,  "6A2Z",
                                      "6A60.3", "6A60.6"  ,"6A70.Z" ,
                                      "6A71.Z", "6A72"  ,"6A73" ,         
                                      "6C9Z", "6B43","6A70.Z","6A7Z" ,
                                      "6A70.3", "6A71.Z","6A60.3",
                                      "6A72", "6B43"), 1, Depre)
      
      
      
    )
  return(dfvalue)
}


popCElix1 <- check_value(popCElix1)

popCElix2 <- popCElix1[ , !(names(popCElix1) %in% "icd11Code")]

popCElix2 <- aggregate(. ~ id_uniq, popCElix2, sum)
popCElix2[-1] <- lapply(popCElix2[-1], function(x) ifelse(x != 0, 1, 0))

# Save popCElix2 to a CSV file
write.csv(popCElix2, file = "popC2_EliX.csv", row.names = FALSE)


# Rename columns
popC_elixhauser_category <- popCElix2 %>%
  select(-death) %>%
  rename("chf" = "Chf", "carit" = "Carit", "valv" = "Valv", "pcd" = "Pcd", "pvd" = "Pvd",
         "cpd" = "Cpd", "ond" = "Ond", "para" = "Para", "rf" = "Rf", "obes" = "Obes", "wloss" = "Wloss",
         "rheumd" = "Rheumd", "pud" = "Pud", "ld" = "Ld", "diabc" = "Diabc", "fed" = "Fed",
         "diabunc" = "Diabunc", "hypc" = "Hypc", "hypunc" = "Hypunc", "coag" = "Coag", "solidtum" = "Solidtum",
         "drug" = "Drug", "alcohol" = "Alcohol", "aids" = "Aids", "blane" = "Blane", "dane" = "Dane", "metacanc" = "Metacanc",
         "psycho" = "Psycho", "depre" = "Depre", "lymph" = "Lymph", "hypothy" = "Hypohthy")


# Add the "comorbidity" class to the popC_elixhauser_category object
class(popC_elixhauser_category) <- c("comorbidity", class(popC_elixhauser_category))

# Add the "elixhauser_icd10_quan" attribute
attr(x = popC_elixhauser_category, which = "map") <- "elixhauser_icd10_quan"

# Check for the presence of attributes
str(popC_elixhauser_category)

# Check for the "comorbidity" class
class(popC_elixhauser_category)

# Calculate Elixhauser scores
elixscore_popC <- score(x = popC_elixhauser_category, weights = "vw", assign0 = FALSE)
print(elixscore_popC)
class(elixscore_popC)

# Add the scores to the IDs
elix_popC <- popC_elixhauser_category %>% mutate(elixscoreC = elixscore_popC)


