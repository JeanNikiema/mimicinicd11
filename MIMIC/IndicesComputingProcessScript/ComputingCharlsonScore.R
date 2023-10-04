install.packages("comorbidity")
library(comorbidity)


# Charlson Score for the original population of MIMIC4 coded in ICD-9-CM and ICD-10-CM

charlson_popMIMIC9 <- comorbidity(x = popICD9MIMIC, id = "id_uniq", code = "icd_code", map = "charlson_icd9_quan", assign0 = FALSE)
charlson_popMIMIC10 <- comorbidity(x = popICD10MIMIC, id = "id_uniq", code = "icd_code", map = "charlson_icd10_quan", assign0 = FALSE)

charlson_popA <- rbind(charlson_popMIMIC9, charlson_popMIMIC10)
score_popA <- score(x = charlson_popA, weights = "quan", assign0 = FALSE)

nrow(charlson_popA)
resume_score_popA <- table(score_popA) %>%
  as.data.frame()
print(resume_score_popA)

# Add scores to identifiers
charlson_scorepopA <- charlson_popA %>%
  mutate(charlson_score_A = score_popA)


# Charlson Score for the original population of MIMIC4 coded in ICD-9-CM and ICD-10-CM

# Remove dots for ICD-10 codes in population B
popB <- popB %>% mutate(icd10cm = gsub("\\.", "", icd10cm)) %>% distinct()

# Calculate Charlson score for Mimic 4 population with all ICD-10-CM codes
charlson_popB <- comorbidity(x = popB, id = "id_uniq", code = "icd10cm", map = "charlson_icd10_quan", assign0 = FALSE)
nrow(charlson_popB)
score_populationB <- score(x = charlson_popB, weights = "quan", assign0 = FALSE)
print(score_populationB)


