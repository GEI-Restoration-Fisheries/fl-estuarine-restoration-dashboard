When downloading and using the Florida Estuarine Restoration Database, please consult the metadata to understand what each field represents and how the values should be interpreted. Below is an overview of the variables available in the database. You can access the complete metadata description <a href="https://github.com/GEI-Restoration-Fisheries/Florida_Estuarine_Restoration_Database" target="_blank">here</a>, which will also be included in the downloaded ZIP file.

**Master_Project_ID**: Unique identifier assigned to each project in the database. A project is defined as a distinct restoration effort, while a record represents all or part of a project.

**Project_Group_ID**: Unique identifier linking records that have different quantitative restoration metrics either across multiple locations and/or different restoration habitats.

**Merged_Record_ID**: Unique identifier linking project records merged across multiple data sources.

**Original_Project_ID**: Unique identifier for each record formally assigned by original data source.

**Data_Source**: Original data source from which project information was obtained. If duplicate records were merged across data sources, this field identifies the primary source record retained in the database.

**Data_Source_2**: Additional data source containing the same project record that was merged with the primary record identified in Data_Source.

**Project_Name**: Restoration project name reported by the original data source.

**Project_Description**: Restoration project description reported by the original data source.

**Project_Status**: Current project status reported by the original data source.

**Start_Year**: Year the project was initiated, funded, or awarded, based on information available from the original data source.

**Completion_Year**: Year the given record was completed as reported by the original data source.

**Project_Completion_Year**: The most recent completion year reported across all records of a given project. A given project may contain multiple records with different completion years, representing restoration activities completed across different habitats or phases as the project progresses.

**Exp_Completion_Year**: Expected project completion year provided by the original data source when a final completion year was not available.

**County**: Associated county in Florida where restoration project was conducted.

**County_Multiple**: List of counties originally reported by the original data source if the data source listed more than one county.

**EDA**: Associated estuarine drainage area where restoration project was conducted.

**Latitude**: Latitude coordinate of the project location reported by the original data source.

**Longitude**: Longitude coordinate of the project location reported by the original data source.

**Habitat_Description**: Brief description of the habitat being restored by the original data source.

**Project_n_Habitat_Types**: Number of unique habitat types targeted for a given project.

**Habitat_Type**: Standardized habitat classification developed for the Florida Estuarine Restoration database.

**Restoration_Technique**: Standardized restoration technique classification developed for the Florida Estuarine Restoration database.

**Acres_Restored**: Total area restored and/or created as reported by the original data source for a given record.

**Acres_Protected**: Total area protected or preserved as reported by the original data source for a given record.

**Miles_Restored**: Total linear distance restored and/or created as reported by the original data source for a given record.

**Miles_Protected**: Total linear distance protected or preserved as reported by the original data source for a given record.

**Project_Acres_Restored**: Sum of total area restored and/or created as reported by the original data source across all records for a given project.

**Project_Acres_Protected**: Sum of total area protected or preserved as reported by the original data source across all records for a given project.

**Project_Miles_Restored**: Sum of total linear distance restored and/or created as reported by the original data source across all records for a given project.

**Project_Miles_Protected**: Sum of total linear distance protected or preserved as reported by the original data source across all records for a given project.

**Acres_Restored_Category**: Standardized size classification of total acres restored and/or created, as reported by the original data source for a given record.

**Miles_Restored_Category**: Standardized size classification of total linear distance restored and/or created, as reported by the original data source for a given record.

**Project_Acres_Restored_Category**: Standardized size classification of total acres restored and/or created, as reported by the original data source for a given project.

**Project_Miles_Restored_Category**: Standardized size classification of total linear distance restored and or created, as reported by the original data source for a given project.

**Debris_Removed**: Total amount of debris removed (in tons).

**Species_Benefited**: Targeted species expected to directly benefit from project implementation, as identified by the original data source.

**Socioeconomic_Jobs_Created**: Total estimated jobs created by the project as reported by the original data source.

**Socioeconomic_Avoided_Travel_Losses**: Avoided travel time losses (USD) by a given restoration project as reported by the original data source, summed across auto and rail avoided travel time losses.

**Socioeconomic_Avoided_Damage_Losses**: Avoided property damage and reduced repairs (USD) by a given restoration project as reported by the original data source, summed across commercial and residential.

**Socioeconomic_Properties_Benefited**: Total number of properties benefitted from enhanced protection as reported by the original data source, summed across commercial, residential, public and critical infrastructure.

**Main_Funding_Source**: Primary funding source or funding administrator for the project as identified by the original data source.

**Main_Funding_Source_Category**: Standardized categorical classification of primary source of funding for the project, based on the funding entity, program, or mechanism identified by the original data source.

**Funding_Program**: Funding program through which project funds were distributed or administered as identified by the original data source.

**Lead_Implementer**: Primary organization or agency responsible for implementing the project as identified by the original data source.

**Lead_Implementer_Category**: Standardized categorical classification of the primary source responsible for implementing the project, as identified by the original data source.

**Partners**: Other organizations, agencies or entities involved in the project as reported by the original data source.

**Reported_Project_Cost**: Reported or source-derived cost of the restoration project based on funding or expenditure information available from the original data source, including funding authority amount funds when applicable.

**Point_of_Contact_Email**: General contact email associated with the original organization, program or database. Provided when available and intended for inquiries regarding project records.

**Project_URL**: Reference URL for project documentation, if provided by original data source.

**FWC_Grant**: Florida Fish and Wildlife grant number associated with the project.

**Federal_Grant**: Federal grant number associated with the project.

**NEP_Name**: Original categorical classification from the NEP data source describing the associated estuary program for the project.

**NEP_Activity_Name**: Original categorical classification from the NEP data source describing whether the activity was restoration, preservation, or maintenance.

**Manual_Recode**: Binary flag indicating whether the record was manually recoded for either the variables Habitat_Type or Restoration Technique (1 = Yes, 0 = No).

**County_GIS**: Binary flag indicating whether the record was assigned a value for the variable County using spatial coordinates (1 = Yes, 0 = No).



