install.packages("comorbidity")
library(comorbidity)


# elixhauser Score for the original population of MIMIC4 coded in ICD-9-CM and ICD-10-CM

elixhauser_popMIMIC9 <- comorbidity(x = popICD9MIMIC, id = "id_uniq", code = "icd_code", map = "elixhauser_icd9_quan", assign0 = FALSE)
elixhauser_popMIMIC10 <- comorbidity(x = popICD10MIMIC, id = "id_uniq", code = "icd_code", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_popA <- rbind(elixhauser_popMIMIC9, elixhauser_popMIMIC10)
score_popA_Elixh <- score(x = elixhauser_popA, weights = "vw", assign0 = FALSE)

nrow(elixhauser_popA)
resume_score_popA_Elixh <- table(score_popA_Elixh) %>%
  as.data.frame()
print(score_popA_Elixh)

# Add scores to identifiers
Elixhauser_scorepopA <- elixhauser_popA %>%
  mutate(elixhauser_score_A = score_popA_Elixh)


# elixhauser Score for the original population of MIMIC4 coded in ICD-9-CM and ICD-10-CM

# Remove dots for ICD-10 codes in population B
popB <- popB %>% mutate(icd10cm = gsub("\\.", "", icd10cm)) %>% distinct()

# Calculate elixhauser score for Mimic 4 population with all ICD-10-CM codes
charlson_popB <- comorbidity(x = popB, id = "id_uniq", code = "icd10cm", map = "charlson_icd10_quan", assign0 = FALSE)
nrow(charlson_popB)
score_populationB <- score(x = charlson_popB, weights = "quan", assign0 = FALSE)
print(score_populationB)


