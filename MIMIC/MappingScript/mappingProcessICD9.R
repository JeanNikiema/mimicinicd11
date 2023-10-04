library(dplyr)

# Load the table of diagnoses with their labels from MIMIC-IV
icd_diagnoses <- read.csv("./MIMIC/mimic-iv-2.2/hosp/d_icd_diagnoses.csv", header = TRUE)



# Load the ICD-9 to ICD-10 mapping using the GEM
map_gem <- read.csv("./MIMIC/GEM/icd9toicd10cmgem.csv")
map_gem_filter <- map_gem %>% filter(!icd10cm == "NoDx") %>% select(icd9cm, icd10cm)


# Find labels for the codes in the General Equivalence Mapping
data_gem <- dplyr::select(map_gem, icd9cm, icd10cm) %>%
  left_join(dplyr::select(icd_diagnoses, icd_code, long_title), by = c("icd9cm" = "icd_code"), relationship ='many-to-many') %>%
  rename("icd9_title" = "long_title")
data_gem <- data_gem %>% 
  mutate(Maptype = "general equivalence mapping(medicare)")
data_gem <- data_gem %>% select(icd9cm, icd9_title, icd10cm, Maptype)

View(data_gem)


# Load manually matched codes for "NoDx"
nodx_match <- read.csv("./MIMIC/GEM/nodx_code.csv")
new_column <- rep("manual matching", nrow(nodx_match))

# Add the new column "Maptype" to the DataFrame
nodx_match <- nodx_match %>%
  mutate(Maptype = new_column)

nodx_match <- nodx_match %>% select(icd9cm, icd9_title, icd10cm, Maptype)
View(nodx_match)

# combining map_gem and nodx_match
gem<-  rbind(data_gem, nodx_match) 

# Add labels to each patient's diagnoses
patients <- dplyr::select(mimic, subject_id, hadm_id, icd_code, icd_version) %>%
  left_join(dplyr::select(icd_diagnoses, icd_code, long_title),
            by = c("icd_code" = "icd_code"),
            relationship = 'many-to-many') %>%
  distinct()


# Get the list of patients with ICD-9-CM diagnosis codes
pts_icd9 <- patients %>% dplyr::filter(icd_version == 9) %>% 
  rename("icd9_title" = "long_title")
View(pts_icd9)

# Get the list of patients with ICD-10-CM diagnosis codes
pts_icd10 <- patients %>% dplyr::filter(icd_version == 10) %>%
  rename("icd10_title" = "long_title") %>%
  rename("icd10cm" = "icd_code") %>%
  distinct()
View(pts_icd10)




# Perform the mapping while keeping only matched codes (Method 1)
data9_map_gm <- dplyr::select(pts_icd9, icd_code, icd9_title) %>%
  left_join(dplyr::select(gem, icd9cm, icd10cm, Maptype), by = c("icd_code" = "icd9cm"), relationship = 'many-to-many') %>%
  rename("icd9cm" = "icd_code") %>%
  distinct()

# ICD data with mapping and without mapping 
map_code1 <- data9_map_gm  %>% filter(!is.na(icd10cm)) %>% distinct()
no_map_code1 <- data9_map_gm  %>% filter(is.na(icd10cm)) %>% select(icd9cm) %>% distinct()

# Now add UMLS mappings between ICD-9-CM and ICD-10-CM
# MRCONSO query to obtain the table icd9to10_mr " SELECT tab1.cui, tab1.code as ICD9CM,
# tab2.code as ICD10CM
# FROM MRCONSO AS tab1 inner join
# MRCONSO AS tab2 on (tab1.cui=tab2.cui)
# WHERE tab1.sab like 'ICD9CM' and
# tab2.sab like 'ICD10CM' group by tab1.cui,
# ICD9CM, ICD10CM"

# Load the mrconso mapping table
cuimap <- read.csv("./MIMIC/UMLS/icd9to10_mr.csv")

# Create a new column "Maptype" and set it to "UMLS" for all rows
nouvelle_colonne <- rep("UMLS", nrow(cuimap))
cuimap <- cuimap %>%
  mutate(Maptype = nouvelle_colonne)

#remove the dot in the ICD ID

cuimap <- cuimap %>%
  mutate(
    icd9code = gsub("\\.", "", icd9code),
    icd10code = gsub("\\.", "", icd10code)
  )

# Perform mapping for the remaining 75 codes using mrconso
data9_map_cui <- dplyr::select(no_map_code1, icd9cm) %>%
  left_join(dplyr::select(cuimap, icd9code, icd9lib, icd10code, icd10lib, Maptype),
            by = c("icd9cm" = "icd9code"), relationship = 'many-to-many') %>%
  rename("icd10cm" = "icd10code") %>%
  rename("icd9_title" = "icd9lib", "icd10_title" = "icd10lib") %>%
  distinct()

# Filter out rows with missing icd10cm values
map_code2 <- data9_map_cui %>% filter(!is.na(icd10cm)) %>% distinct()
no_map_code2 <- data9_map_cui %>% filter(is.na(icd10cm)) %>% select(icd9cm)

# Load the manual mapping table
data9_map_hand <- read.csv("./MIMIC/ManualMapping/hand_map.csv")

# Add a new column "Maptype" and set it to "manual matching" for all rows
nouvelle_colonne <- rep("manual matching", nrow(data9_map_hand))
data9_map_hand <- data9_map_hand %>%
  mutate(Maptype = nouvelle_colonne)


data9_map_gm <- data9_map_gm %>% select(icd9cm, icd9_title, icd10cm, Maptype)
data9_map_cui <- data9_map_cui %>% select(icd9cm, icd9_title, icd10cm, Maptype)
data9_map_hand <- data9_map_hand %>% select(icd9cm, icd9_title, icd10cm, Maptype)
nodx_match <- nodx_match %>% select(icd9cm, icd9_title, icd10cm, Maptype)

# Combine the mapping data from different sources into a single DataFrame
final_map <- rbind(
  data9_map_gm %>% filter(!is.na(icd10cm)),
  data9_map_cui %>% filter(!is.na(icd10cm)),
  data9_map_hand %>% filter(!is.na(icd10cm)),
  nodx_match
)

# View the final_map DataFrame
View(final_map)

# Install the openxlsx package if not already installed
if (!require(openxlsx)) {
  install.packages("openxlsx")
}

# Load the openxlsx library
library(openxlsx)
# Convert all columns in final_map to character
final_map[] <- lapply(final_map, as.character)

# Write the final_map DataFrame to an Excel file
write.xlsx(final_map, "./MIMIC/MappingsFile/icd9cm_to_icd10cm_mimic4.xlsx")

# Display the final_map DataFrame
final_map





















