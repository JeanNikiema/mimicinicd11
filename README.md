# The MIMIC database in ICD-11

This repository contains :
- mapping tables between ICD-9-CM and ICD-10-CM, as well as between ICD-10-CM and ICD-11. The mappings mainly convert all the ICD-10-CM and ICD-9-CM codes found in the Medical Information Mart for Intensive Care (MIMIC) IV Version 2.2 database.
- R script of the mapping table creation
- R script to convert all the diagnoses in MIMIC into ICD-11
- R script to compute Charlson and Elixhauser for the MIMIC population using ICD-9-CM, ICD-10-CM and also ICD-11
- Tables of weighted ICD-11 codes for Charlson and Elixhauser. 

## Mapping tables creation

### mappings sources
Four sources of mappings have been used to build the mapping table:
- The General Equivalence Mappings Table (GEMt) developed by the Centers for Medicare & Medicaid Services (CMS) establishes mappings between ICD-9-CM and ICD-10-CM.[GEM](https://www.nber.org/research/data/icd-9-cm-and-icd-10-cm-and-icd-10-pcs-crosswalk-or-general-equivalence-mappings)
- The mapping tables established by the World Health Organisation (WHO) between ICD-10 and ICD-11 (WHOt). [WHOt](https://icd.who.int/browse11/Downloads/Download?fileName=mapping.zip)
- The mapping tables (Fungt) established between the frequently used ICD-10-CM codes found in Medicare claims data and their corresponding codes in the ICD-11 classification system by Fung et al. [Fungt](https://pubmed.ncbi.nlm.nih.gov/34383897/)
- The Unified Medical Language system (UMLS) is used to generate mappings between ICD-9-CM and ICD-10-CM.[UMLS](https://pubmed.ncbi.nlm.nih.gov/33059367/)

### Creation of mappings tables across three versions of ICD
This step consists of creating mapping tables for ICD codes that are used in the MIMIC database. Each mapping table is created based on two automated steps followed by a manual mapping. 

![mappingTableCreation_JAMIA](https://github.com/JeanNikiema/mimicinicd11/assets/110404054/41cd2676-5974-4881-a346-9f11504bcfca)

    Figure: Applied steps for mapping table creation

- Step 1.1: Based on the table ”hosp“ in the MIMIC database, the column “diagnoses_icd” is used to retrieve all the ICD codes used to describe patients’ conditions.
- Step 1.2: Correspondences for ICD-9-CM codes were retrieved using the GEMt as well as the UMLS. Specifically, for GEM, the file icd9toicd10cmgem was utilized. For the UMLS, all corresponding ICD-10-CM codes found through the “mrconso” table  as sharing the same CUI were retrieved
- Step 1.i: The manual process involved utilizing the UMLS to identify the appropriate ICD code for a given entry label for not-mapped ICD-9-CM codes. In cases where multiple codes were retrievable, the most precise one was selected for use. The mapping process was performed by two annotators (MD with knowledge on biomedical terminologies) and divergence was validated by consensus.
- Step 2.1: Based on WHOt and Fungt,  corresponding ICD-11 codes are retrieved for ICD-10-CM codes present in the MIMIC database or obtained through the created mappings tables for ICD-9-CM to ICD-10-CM codes. For the WHOt, only the “10To11MapToMultipleCategories” and “10To11MapToOneCategory” files were utilized. In Fungt, mappings were retrieved based on the "FinalMaptype" tag. If the tag indicated "full map by post coordination," only post-coordinated mappings were retrieved. Otherwise, both pre-coordinated and post-coordinated expressions (when available) were retrieved as mappings for the ICD-10-CM code.
- Step 2.2: The reverse engineering process involved utilizing the existing mappings of ICD-10-CM codes to ICD-11, obtained through the previous process, in order to identify mappings for ICD-10-CM concepts that did not have any existing mappings. Then, when no mapping has been found for ICD-10-CM, we use the structure of ICD-10-CM to assign the mapping of parent codes when they exist. This step is performed until no new mappings can be retrieved by the process.
- Step 2.i: The manual mapping process consisted of using the coding system developed by WHO to find correspondences for the ICD-10 and ICD-9 codes without mappings to ICD-11. The mapping process was performed by two annotators and validated by consensus. The mappings exclusively applied to ICD-10-CM and ICD-9-CM codes that were not mapped to ICD-11 via the automatic steps and were present in more than 500 stays.

## Table of weighted ICD-11 code by comorbidity for Charlson and Elixhauser. 

This table is created using the mappings tables between ICD-10-CM/ ICD-9-CM to ICD-11


