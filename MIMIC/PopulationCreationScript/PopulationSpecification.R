# Remove intersected IDs from population A
popA_filter <- popA[!(popA$id_uniq %in% n_intersect_BD), ]
nA_f <- popA_filter %>% select(id_uniq) %>% distinct() # 430842

# Retrieve IDs with ICD-9 codes without correspondence to ICD-11
pop9_todelete <- dplyr::select(popA_filter, subject_id, id_uniq, icd_code) %>%
  inner_join(dplyr::select(icd9_no_retrouv, icd9cm),
             by = c("icd_code" = "icd9cm"),
             relationship = "many-to-many") %>%
  distinct()

id_todelete1 <- pop9_todelete %>% select(id_uniq) %>% distinct()
subject_id_todelete1 <- pop9_todelete %>% select(subject_id) %>% distinct() # 30952

# Retrieve IDs with ICD-10 codes without correspondence to ICD-11
pop10_todlete <- dplyr::select(popA_filter, subject_id, id_uniq, icd_code) %>%
  inner_join(dplyr::select(no_10cm_mimic_retrouv, icd10cm),
             by = c("icd_code" = "icd10cm"),
             relationship = "many-to-many") %>%
  distinct()

id_todelete2 <- pop10_todlete %>% select(id_uniq) %>% distinct()
subject_id_to_delete2 <- pop10_todlete %>% select(subject_id) %>% distinct() # 36653
subject_id_tot <- rbind(subject_id_to_delete2, subject_id_todelete1) %>% distinct() # 63273

# Gather all IDs with codes not corresponding to ICD-11
all_id_to_delete <- rbind(id_todelete1, id_todelete2) %>% distinct()

id_todelete <- all_id_to_delete$id_uniq

# Filter population A from all codes without correspondence
popA_final <- popA_filter[!(popA_filter$id_uniq %in% id_todelete), ]
n_popA_final <- popA_final %>% select(id_uniq) %>% distinct() # 337165
n_popA_ID_Final <- popA_final %>% select(subject_id) %>% distinct() # 337368

# Extract the population with ICD-9 codes
popICD9MIMIC <- popA_final %>% filter(icd_version == 9)
nICD9MIMIC <- popICD9MIMIC %>% select(id_uniq) %>% distinct() # 236008

# Extract the population with ICD-10 codes
popICD10MIMIC <- popA_final %>% filter(icd_version == 10)
npICD10MIMIC <- popICD10MIMIC %>% select(id_uniq) %>% distinct() # 101360

# Create population ICD-10 from ICD-9-CM mapping
View(final_map) # CIM-9-CM to CIM-10-CM mapping table

# Remove punctuation from all ICD-10-CM codes
final_map <- final_map %>% mutate(icd10cm = gsub("\\.", "", icd10cm))
popICD9InICD10MIMIC <- dplyr::select(popICD9MIMIC, subject_id, id_uniq, dod, icd_code) %>%
  left_join(dplyr::select(final_map, icd9cm, icd10cm), by = c("icd_code" = "icd9cm"), relationship = "many-to-many")

# Check if there are missing ICD-10 codes in population C
na_pICD9InICD10MIMIC<- popICD9InICD10MIMIC %>% filter(is.na(icd10cm))

# Create population B with all Mimic 4 codes in ICD-10-CM
popICD9InICD10MIMIC_subset <- popICD9InICD10MIMIC %>% select(subject_id, id_uniq, dod, icd10cm)
popICD10MIMIC_subset <- popICD10MIMIC %>% select(subject_id, id_uniq, dod, icd_code)%>% rename("icd10cm" = "icd_code") %>% distinct()
popB <- rbind(popICD9InICD10MIMIC_subset, popICD10MIMIC_subset) %>% distinct()

# Create population C using CIM-10-CM to CIM-11 mapping table
popC <- popB %>% left_join(dplyr::select(map_tab_plus6, icd10Code, icd11Code), by = c("icd10cm" = "icd10Code"), relationship = "many-to-many") %>% distinct()
PopCListHosp <- popC %>% select(id_uniq) %>% distinct() # 
write.csv(popC, "./MIMIC/ICD11/population_B.csv", row.names = FALSE)


# see the script ComputingCharlsonScore ComputingElixhauserScore to continue

