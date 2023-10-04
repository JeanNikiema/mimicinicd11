# Load necessary libraries
library(dplyr)
library(readxl)

# WHO Map
whomap1 <- read_xlsx("./MIMIC/WHO/10To11MapToMultipleCategories.xlsx")
whomap1.subset <- whomap1[, c("icd10Code", "icd10Title", "icd11Code", "icd11Title")]
whomap2 <- read_xlsx("./MIMIC/WHO/10To11MapToOneCategory.xlsx")
whomap2.subset <- whomap2[, c("icd10Code", "icd10Title", "icd11Code", "icd11Title")]
whomap <- rbind(whomap1.subset, whomap2.subset) %>% filter(!is.na(icd11Code))

new_column <- rep("WHO map", nrow(whomap))

# Add a new column "Maptype" to the DataFrame
whomap <- whomap %>% mutate(Maptype = new_column)

# KW Map
kw <- read.table("./MIMIC/KWMap/ocab156_Supplementary_Data.txt", header = TRUE, sep = "\t")

kw_filter1 <- kw %>% select(I10CODE, FinalPostcoor, FinalPostcoDesc, FinalMaptype) %>%
  filter(FinalMaptype == "2. full map by postcoordination") %>%
  rename("icd10Code" = "I10CODE", "icd11Code" = "FinalPostcoor", "icd11Title" = "FinalPostcoDesc")

kw_filter2 <- kw %>% select(I10CODE, FinalPostcoor, FinalPostcoDesc, FinalMaptype) %>%
  filter(!FinalMaptype == "2. full map by postcoordination") %>%
  rename("icd10Code" = "I10CODE", "icd11Code" = "FinalPostcoor", "icd11Title" = "FinalPostcoDesc") %>%
  filter(!icd11Code == "NULL")

kw_filter3 <- kw %>% select(I10CODE, FinalI11Code, FinalI11Name, FinalMaptype) %>%
  filter(!FinalMaptype == "2. full map by postcoordination") %>%
  rename("icd10Code" = "I10CODE", "icd11Code" = "FinalI11Code", "icd11Title" = "FinalI11Name")

kw.final <- rbind(kw_filter1, kw_filter2, kw_filter3)
kw.subset <- kw.final %>% select(icd10Code, icd11Code, icd11Title)

new_column <- rep("KINWAH table", nrow(kw.subset))

# Add a new column "Maptype" to the DataFrame
kw.subset <- kw.subset %>% mutate(Maptype = new_column)

# Combine the mapping tables
map_tab <- rbind(whomap %>% select(-icd10Title), kw.subset) %>%
  mutate(icd10Code = gsub("\\.", "", icd10Code)) %>%
  distinct()

# Assemble ICD-10-CM codes from MIMIC-4
mimic_cim10 <- pts_icd10[, c("icd10cm", "icd10_title")] %>% distinct()


#final_map <- read_ods("/Users/papeace/Documents/finalmap.ods")
final_map <- final_map %>%
  filter(!icd10cm == "NoDx") %>%
  filter(!Maptype == "Quan Map") %>%
  distinct()


map9to10 <- final_map %>% select(icd9cm, icd10cm) %>% mutate(icd10cm = gsub("\\.", "", icd10cm))

# Select two columns: the code and the title
map9to10_subset <- map9to10 %>% select(icd10cm)
mimic_cim10 <- mimic_cim10 %>% select( icd10cm)
# Combine all ICD-10 codes
icd10_tot <- rbind(mimic_cim10, map9to10_subset) %>% distinct()

# Remove dots from all ICD-10 codes
icd10_tot <- icd10_tot %>% mutate(icd10cm = gsub("\\.", "", icd10cm))

# Create sub-columns for ICD-10 codes by progressively removing characters
icd10_sub <- icd10_tot %>% select(icd10cm) %>%
  mutate(icd10_sub3 = substr(icd10cm, 1, 3),
         icd10_sub4 = substr(icd10cm, 1, 4),
         icd10_sub5 = substr(icd10cm, 1, 5),
         icd10_sub6 = substr(icd10cm, 1, 6)) %>%
  filter(!icd10cm == "NoDx") %>%
  distinct()
View(icd10_sub)

# Join based on the icd10cm = icd10code condition
result_icd11cm <- left_join(icd10_sub, map_tab, by = c("icd10cm" = "icd10Code")) %>%
  mutate(ncar = 7)
nrow(result_icd11cm %>% filter(is.na(icd11Code))) # 21651

# Add the codes found during the first mapping to the used mapping table
map_tab_plus <- rbind(
  result_icd11cm %>% filter(!is.na(icd11Code)) %>%
    rename(icd10Code = icd10cm) %>%
    select(icd10Code, icd11Code, icd11Title, Maptype) %>%
    distinct(),
  map_tab
) %>% distinct()

# Check the number of rows in map_tab_plus
nrow(map_tab_plus) # 17169

# Join for the condition icd10_sub6 = icd10code
result_icd11_sub6 <- left_join(result_icd11cm %>% filter(is.na(icd11Code)) %>% select(-icd11Code, -icd11Title, -ncar), map_tab_plus, by = c("icd10_sub6" = "icd10Code"), relationship = 'many-to-many') %>% distinct() %>% mutate(ncar = 6)

# Check the number of rows with missing icd11Code after the first mapping
nrow(result_icd11_sub6 %>% filter(is.na(icd11Code))) # 26151

# Add the codes found during the first mapping to the mapping table used
map_tab_plus2 <- rbind(
  result_icd11_sub6 %>% filter(!is.na(icd11Code)) %>% rename(icd10Code = icd10cm) %>% select(icd10Code, icd11Code, icd11Title) %>% distinct(),
  map_tab_plus
) %>% distinct()

# Check the number of rows in map_tab_plus2
nrow(map_tab_plus2) # 17169

# Join for the condition icd10_sub5 = icd10code
result_icd11_sub5 <- left_join(result_icd11_sub6 %>% filter(is.na(icd11Code)) %>% select(-icd11Code, -icd11Title, -ncar), map_tab_plus2, by = c("icd10_sub5" = "icd10Code"), relationship = 'many-to-many') %>% mutate(ncar = 5) %>% distinct()

# Check the number of rows with missing icd11Code after the second mapping
nrow(result_icd11_sub5 %>% filter(is.na(icd11Code))) # 21651

# Add the codes found during the second mapping to the mapping table used
map_tab_plus3 <- rbind(
  result_icd11_sub5 %>% filter(!is.na(icd11Code)) %>% rename(icd10Code = icd10cm) %>% select(icd10Code, icd11Code, icd11Title, Maptype) %>% distinct(),
  map_tab_plus2
) %>% distinct()

# Check the number of rows in map_tab_plus3
nrow(map_tab_plus3) # 17169

# Join for the condition icd10_sub4 = icd10code
result_icd11_sub4 <- left_join(result_icd11_sub5 %>% filter(is.na(icd11Code)) %>% select(-icd11Code, -icd11Title, -ncar), map_tab_plus3, by = c("icd10_sub4" = "icd10Code"), relationship = 'many-to-many') %>% mutate(ncar = 4)

# Check the number of rows with missing icd11Code after the third mapping
nrow(result_icd11_sub4 %>% filter(is.na(icd11Code))) # 3164

# Add the codes found during the third mapping to the mapping table used
map_tab_plus4 <- rbind(
  result_icd11_sub4 %>% filter(!is.na(icd11Code)) %>% rename(icd10Code = icd10cm) %>% select(icd10Code, icd11Code, icd11Title, Maptype.y.y) %>% rename("Maptype" = "Maptype.y.y") %>% distinct(),
  map_tab_plus3
) %>% distinct()

# Check the number of rows in map_tab_plus4
nrow(map_tab_plus4) # 36446

# Join for the condition icd10_sub3 = icd10code
result_icd11_sub3 <- left_join(result_icd11_sub4 %>% filter(is.na(icd11Code)) %>% select(-icd11Code, -icd11Title, -ncar), map_tab_plus4, by = c("icd10_sub3" = "icd10Code"), relationship = 'many-to-many') %>% mutate(ncar = 3)

# Check the number of rows with missing icd11Code after the fourth mapping
nrow(result_icd11_sub3 %>% filter(is.na(icd11Code))) # 946

# Add the codes found during the fourth mapping to the mapping table used
map_tab_plus_5 <- rbind(
  result_icd11_sub3 %>% filter(!is.na(icd11Code)) %>% rename(icd10Code = icd10cm) %>% select(icd10Code, icd11Code, icd11Title, Maptype) %>% distinct(),
  map_tab_plus4
) %>% distinct()

# Check the number of rows in map_tab_plus_5
nrow(map_tab_plus_5) # 43370



# The last codes without correspondences
no_map_final <- result_icd11_sub3 %>% filter(is.na(icd11Code))
icd10_nomap <- no_map_final %>% select(icd10cm) %>% distinct()



# Check the number of rows without correspondences in CIM11
nrow(no_map_final %>% select(-ncar, -icd11Title) %>% distinct()) # 946



# Manual mapping of the most recent icd10cm codes in the diagnostics
manual_match_10to11 <- read.csv("./MIMIC/ManualMapping/icd10cm_to_icd11.csv")
mm_10cmto11 <- manual_match_10to11 %>% filter(!is.na(icd11Code)) %>% select(-icd10) %>% rename ("icd10Code" = "icd10cm")
nouvelle_colonne <- rep("manual matching", nrow(mm_10cmto11))

# Add the new column "Maptype" to the DataFrame
mm_10cmto11 <- mm_10cmto11 %>% mutate(Maptype = nouvelle_colonne)
View(mm_10cmto11)

# Combine all the mappings found
map_tab_plus6 <- rbind(map_tab_plus_5, mm_10cmto11)
nrow(map_tab_plus6)

# Join for the condition icd10_sub3 = icd10code
result_icd11_sub2 <- left_join(result_icd11_sub3 %>% filter(is.na(icd11Code)) %>% select(-icd11Code, -icd11Title, -ncar), map_tab_plus6, by = c("icd10_sub3" = "icd10Code"), relationship = 'many-to-many') %>% mutate(ncar = 3)
nrow(result_icd11_sub3 %>% filter(is.na(icd11Code)))

# Search for CIM10CM codes in mimic4 without correspondences
View(mimic_cim10)
mimic_cim10 <- mimic_cim10 %>% mutate(icd10cm = gsub("\\.", "", icd10cm)) %>% distinct()

map_mimic4 <- dplyr::select(mimic_cim10, icd10cm) %>% left_join(dplyr::select(map_tab_plus6, icd10Code, icd11Code),
                                                                by = c("icd10cm" = "icd10Code"), relationship = 'many-to-many') %>% distinct()

View(map_mimic4)
icd10toicd11_mimic <- map_mimic4 %>% filter(!is.na(icd11Code))
no_10cm_mimic_retrouv <- map_mimic4  %>% filter(is.na(icd11Code)) %>% select(icd10cm) %>% distinct()

View(no_10cm_mimic_retrouv) # 732

# Search for CIM9CM codes with CIM10CM codes without correspondences
cim9cm_map <- dplyr::select(map9to10, icd9cm, icd10cm) %>% left_join(dplyr::select(map_tab_plus6, icd10Code, icd11Code),
                                                                     by = c("icd10cm" = "icd10Code"), relationship = 'many-to-many') %>% distinct()
View(cim9cm_map)

# Retrieve CIM9CM codes with NA
no_cim9cm_map <- cim9cm_map %>% filter(is.na(icd11Code)) %>% distinct()
no_cim9_map_distinct <- no_cim9cm_map %>% dplyr::select(icd9cm) %>% distinct()
nrow(no_cim9_map_distinct)

# Retrieve CIM9CM codes without NA
map_cim9 <- cim9cm_map %>% filter(!is.na(icd11Code)) %>% distinct()
View(map_cim9)
nrow(map_cim9)

# Perform a join between CIM9CM codes with NA and CIM9CM codes without NA
test <- dplyr::select(map_cim9, icd9cm) %>% inner_join(dplyr::select(no_cim9cm_map, icd9cm, icd10cm, icd11Code), by = c("icd9cm" = "icd9cm"), relationship = 'many-to-many')
icd9cm_distinct <- test %>% dplyr::select(icd9cm) %>% distinct()
View(icd9cm_distinct)
View(test)

# Remove icd9cm from the join of icd9cm without NA
icd9cm <- no_cim9_map_distinct[!no_cim9_map_distinct$icd9cm %in% icd9cm_distinct$icd9cm,]

icd9_no_retrouv <- data.frame(icd9cm)

View(icd9_no_retrouv) # 348

# Rebuild the mapping table with Maptype
icd10_title <- icd_diagnoses %>% filter(icd_version == 10)
ICD10cm_to_ICD11_table <- map_tab_plus6 %>% left_join(dplyr::select(icd10_title, icd_code, long_title), by = c("icd10Code" = "icd_code")) %>% rename("icd10Title" = "long_title")
# Use the names() function to display the current column names
current_column_names <- names(ICD10cm_to_ICD11_table)
print(current_column_names)

ICD10cm_to_ICD11_table <- ICD10cm_to_ICD11_table[, c("icd10Code",  "icd11Code", "icd11Title", "Maptype")]
# Convert all columns in final_map to character
ICD10cm_to_ICD11_table[] <- lapply(ICD10cm_to_ICD11_table, as.character)
write.xlsx(ICD10cm_to_ICD11_table , "./MIMIC/MappingsFile/ICD10cm_to_ICD11.xlsx")
