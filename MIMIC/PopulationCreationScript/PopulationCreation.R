# Load the 'dplyr' library
library(dplyr)

# Load patient and diagnosis tables from MIMIC
patient <- read.csv("./MIMIC/mimic-iv-2.2/hosp/patients.csv")
diagnoses <- read.csv("./MIMIC/mimic-iv-2.2/hosp/diagnoses_icd.csv")

# Create the total MIMIC population with their diagnoses per hospitalization
mimic <- select(patient, subject_id, dod) %>%
  inner_join(dplyr::select(diagnoses, subject_id, hadm_id, icd_code, icd_version),
             by = c("subject_id" = "subject_id"))

# Find the size (N) of population A
MIMICPatients <- mimic %>%
  select(subject_id) %>%
  distinct()

# Load your population
View(mimic)

# Create unique stay identifiers (subject_id - hadm_id) for the MIMIC total 
#population (population A)
popA <- mimic %>%
  mutate(id_uniq = paste0(subject_id, "_", hadm_id))

# Display the updated data
View(popA)
nrow(popA)

# Find the size (N) of population A
nA <- popA %>%
  select(id_uniq) %>%
  distinct()

# first round of data, the population A is around 430.852 unique hospital 
# stay and subject
View(nA)


# Identification of the MIMIC4 population with ICD-9-CM diagnostic codes
pop_cim9 <- popA %>%
  filter(icd_version == 9)

View(pop_cim9)

# Find the size (N) of pop_cim9
n_pop9 <- pop_cim9 %>%
  select(id_uniq) %>%
  distinct()

# Identification of the MIMIC4 population with ICD-10-CM diagnostic codes
pop_cim10 <- popA %>%
  filter(icd_version == 10)

# Find the size (N) of pop_cim10
n_pop10 <- pop_cim10 %>%
  select(id_uniq) %>%
  distinct()

# Retrieve the IDs of patients with both ICD-10 and ICD-9 diagnoses
n_intersect_BD <- intersect(n_pop9$id_uniq, n_pop10$id_uniq)
print(n_intersect_BD)

# Remove intersected IDs from population A
popA_filter <- popA[!(popA$id_uniq %in% n_intersect_BD), ]

# Find the size (N) of the new population A

nA_f <- popA_filter %>% select(id_uniq) %>% distinct() # 430842


# to continue the script run the mapping between ICD-9-CM, ICD-10-CM and ICD-11 
# mappingProcessICD9, mappingProcessICD10



