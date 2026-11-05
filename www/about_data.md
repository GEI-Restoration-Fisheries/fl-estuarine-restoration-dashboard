## About the Data

The Florida Estuarine Restoration Database is an open-access, standardized compilation of habitat restoration and estuarine resource improvement projects implemented within Florida estuaries and their contributing watersheds. The database integrates records from multiple publicly available sources into a unified dataset with standardized project metadata, spatial information, restoration classifications, funding information, and project attributes. Source records were harmonized using consistent nomenclature, controlled vocabularies, and quality-control procedures, including duplicate identification and manual review.

Here we provide an overview of what the data includes, the methodology behind its creation, and our recommendations for appropriate use and interpretation of the data. For a more detailed description of the database, we encourage you to consult the data descriptor, as well as the accompanying metadata (see <a href="#" onclick="Shiny.setInputValue('goto_citation', Math.random()); return false;">Citation and Sources</a>).

This dataset is a result of research funded by the National Oceanic and Atmospheric Administration's RESTORE Science Program (<a href="https://ror.org/0042xzm63" target="_blank">ROR</a>) under award NA22NOS4510203 to the National Center for Ecological Analysis and Synthesis at the University of California, Santa Barbara and part of the Restoration & Fisheries Working Group of the Gulf Ecosystem Initiative.

<hr style="height:2px;border-width:0;color:gray;background-color:gray">

### Available Data

**Project locations**

Each project is mapped to a single reported location (latitude and longitude) within Florida’s estuaries and their contributing watersheds (see Inclusion criteria below). When a source didn’t report a county, we assigned one from the project’s coordinates using GIS. For projects listing several counties, we assigned a single primary county the same way and kept the full list in a separate field. The estuarine regions used for filtering and mapping on this dashboard come from the Florida Department of Environmental Protection’s Estuary Nutrient Regions, the same boundaries used to define the seaward edge of the study area.

**Project attributes**

Each project record describes what was done, where, when, and with what resources:

* **Basic information**: name, description, status, start and completion years, original data source(s), and a project webpage link when available.

* **Habitat and technique**: the habitat restored and the restoration technique used, each assigned to a standardized category.

* **Size**: acres and miles restored or protected, with size categories, for each record and summed for the whole project.

* **Costs and funding**: total reported cost, main funding source and category, funding program, legislation, lead implementer, and partners.

* **Other details**: when reported, species benefited, plant types, debris removed, and socioeconomic benefits such as jobs created.

Not every attribute is available for every project; a blank means the information wasn’t reported or wasn’t clear, not zero. Costs are as reported by each source, may not include all of a project’s funding, and may reflect money spent or awarded depending on the source. Definitions for every variable are in the metadata on the <a href="#" onclick="Shiny.setInputValue('goto_download_data', Math.random()); return false;">Download Data</a> page.

<hr style="height:2px;border-width:0;color:gray;background-color:gray">

### Overview of Methodology

The database was built in six steps, described below: gathering records from multiple sources, applying inclusion criteria, standardizing the data, filling key gaps, merging duplicates, and validating the result. Full details are in the data descriptor linked above.

**Data compilation**

We reviewed eleven Florida-focused source datasets identified by working group experts, including public databases, agency project platforms, and agency datasets obtained by direct request. When a download only linked to project webpages, we asked the agency for the underlying project information. The nine sources that contributed records to the final database are listed on the <a href="#" onclick="Shiny.setInputValue('goto_citation', Math.random()); return false;">Citation and Sources</a> page.

**Inclusion criteria**

The database focuses on habitat restoration, plus some habitat enhancement, creation, and conservation projects, as long as there was on-the-ground work. Projects are included regardless of year, funding, or whether they met their ecological targets. Projects that were only research, program development, planning or design, or stormwater management were excluded, as were projects that were canceled, inactive for over five years with no new plan, or had their permits denied or withdrawn.

Projects also had to fall within the study area, based on a single reported location. The seaward edge follows the Florida Department of Environmental Protection’s Estuary Nutrient Regions, minus regions that are mostly marine with few estuarine features (such as the Keys and the Gulf shelf). The landward edge includes all Florida coastal counties plus any watershed (Hydrologic Unit Code 10, or HUC10) that overlaps them, to capture upstream projects that benefit estuaries. These layers were combined into one boundary for GIS screening.

**Standardization**

Each source organizes information differently, so we mapped all of them into one common structure; source-specific decisions are documented in the data descriptor. Sources also vary in detail, so one project may appear as several records (for example, one per habitat restored), which share a master project ID.

Habitat types and restoration techniques were sorted into consistent categories using the sources’ own terms and, when needed, project descriptions; if the information wasn’t explicit, the field was left blank rather than guessed. Funders and lead implementers were grouped into standard categories based on the primary funder or implementer reported. Funding wasn’t traced further than what was reported, so a “state-funded” project may include federal funds passed through state agencies.

**Supplemental procedures**

Some sources systematically lacked key information, such as location, completion year, project size, or funding. In those cases, we looked for the missing details in the source’s own documentation (for example, annual reports) or asked the data provider directly. This was only done when the information could be clearly tied to the project, never to fill isolated gaps in otherwise complete records.

**De-duplication and merging of records**

Many projects appear in more than one source, so we searched for duplicates in three stages: exact or near-exact matches (project name, description, and coordinates), records with the same name but minor differences, and expert review of complex, multi-part projects guided by Florida restoration practitioners. Every suspected duplicate was manually reviewed. Confirmed matches were merged, keeping the most specific information (for example, “oysters” over “bivalves”), and linked by a merged record ID. When we couldn’t decide with confidence, both records were kept, so some duplicates may remain.

**Technical validation**

After merging, we recalculated size categories and derived each project’s total acres and miles, number of habitat types, and completion year from its final records. Some filtering was deliberately saved for this stage; for example, terminated projects were kept through de-duplication (a project terminated in one source may still be marked active in another due to source update frequencies) and then removed. Records were also removed based on feedback from data providers and consensus of the working group.

A final GIS screening checked every project against the study-area boundary, since merging can add new location details. Projects outside the boundary, or without enough location information to tell, were excluded. We also reviewed each source for completeness and reliability and left two out entirely: a state environmental permitting dataset that didn’t fit the database’s structure and goals, and a secondary compilation whose records mostly duplicated primary sources.

**Update frequency**

The database will undergo annual updates to pull in the most recent information about restoration projects throughout Florida's estuaries. Minor updates will occur on an ongoing, as-needed basis, such as for corrections to existing projects or additions of individual projects contributed by the public. Each update will be reflected in the version name associated with the database. Users should always cite the appropriate version of the database, as data and/or results will differ between versions.


<hr style="height:2px;border-width:0;color:gray;background-color:gray">

### Using the Data

The database is intended to facilitate analyses that require standardized restoration information across multiple jurisdictions and organizations. Potential applications include spatial analyses of restoration activities, evaluation of restoration investments, assessments of restoration distribution and habitat coverage, and inputs for ecological modeling. Because the database preserves standardized metadata while documenting original data sources, it can also support future updates and regional restoration syntheses.

**Recommended filters**

While the database focuses on habitat restoration activities, it also includes habitat enhancement, habitat creation, and conservation projects implemented within Florida’s estuaries and associated watershed. Consequently, not all projects represent restoration of previously existing habitat conditions, nor do they always directly restore estuarine habitats. When a user’s goal is to capture the most accurate reflection of estuarine habitat restoration activities throughout Florida, we recommend adopting the following data filters before analysis:

* **Activity Type**: We recommend excluding any projects listed as “Maintenance” projects by the National Estuary Program database. Because projects obtained from other sources do not have this information, we still recommend retaining any projects in which this activity is “Unknown.”

* **Habitat Type**: We recommend excluding any projects in which the habitat may not be representative of estuarine systems. This includes the following habitat types: “Coastal Upland”, “Upland”, “Upland and freshwater”, “Freshwater wetland”, “Other”, and “Unknown”.

* **Restoration Technique**: In many cases, projects utilize several different types of restoration techniques, some of which may not necessarily be considered true restorative actions. We recommend excluding any projects in which the recorded restoration technique is exclusively “Species control.” Projects that utilize this technique in combination with other techniques, however, can still be included. 

These exclusions have been applied to the data visualizations throughout this interactive dashboard. Of course, users are free to deviate from these criteria based on their own restoration questions, interests, or needs.

**Caveats and limitations**

While all data has undergone technical validation and data quality assurance and control procedures, we are unable to verify all project information. Project attributes are assumed accurate and subject to the validity of the person who originally created the project record. If you discover an error in one of the current project records, we would love to know how it might be corrected. To submit any corrections for consideration in the next version of the database, visit the <a href="#" onclick="Shiny.setInputValue('goto_contribute', Math.random()); return false;">Contribute</a> section.

The projects and their attributes should not be considered the true extent of estuarine restoration efforts in Florida. This database should be considered a minimum estimate of restoration activity and is subject to data availability. Trends in the spatial, temporal, and financial characteristics of estuarine restoration projects are only representative of the projects in which these attributes have been made publicly available, and they should not be used to make definitive claims about all restoration activities that have occurred in the state.
