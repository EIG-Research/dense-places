# Dense Places Can Still Build
This respository includes the data and necessary code to support EIG's analysis of new housing construction in areas with high population density. You can find the article [here](INSERT LINK).

All links are current at the time of publication. Contact Jess Remington with any questions at jess@eig.org .

## Variables and Data Sources

| **Variable** | **Data Source** | **Years** | **Var Names** |
|--------------|------------------|-----------|----------------|
| Population - County | PEP (Population Estimates Program) Vintage [2024](https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html), [2014](https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html); | 2014, 2024 | POPESTIMATE2024 |
| Population - Census Tracts | ACS Population [B01003](https://data.census.gov/table/ACSDT1Y2023.B01003?q=B01003) (5yr) | 2014, 2023 | B01003_001E |
| Population - Places | ACS Population [B01003](https://data.census.gov/table/ACSDT1Y2023.B01003?q=B01003) (1yr) for 2014; Census Population Estimates [City and Town Population Totals 2024](https://www.census.gov/programs-surveys/popest/data/tables.html) | 2014, 2024 | |
| Land Area | [Gazetteer Files](https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html) (National Counties and Census Tracts) | 2024 | ALAND_SQMI |
| Housing Units (total, not just occupied) | EIG validation of [HUD USPS data](https://www.huduser.gov/apps/public/usps/home) | 2014, 2024 | TOTAL_RESIDENTIAL_ADDRESSES |
| Residential Construction (final permits) → measures new units per year | [SOCDC Building Permits](https://hudgis-hud.opendata.arcgis.com/datasets/HUD::residential-construction-permits-by-county/about) | 2022 | ALL_PERMITS_YEAR, SINGLE_FAMILY_PERMITS_YEAR, ALL_MULTIFAMILY_PERMITS_YEAR |
| Rent burden | ACS [B25070](https://data.census.gov/table/ACSDT1Y2023.B25070?q=B25070:+Gross+Rent+as+a+Percentage+of+Household+Income+in+the+Past+12+Months) (1yr) | 2023 | |
| Housing cost burden (homeowners with a mortgage) | ACS [B25091](https://data.census.gov/table?q=B25091) (1yr) | 2023 | |
| Rent to income ratio | ACS [B25071](https://data.census.gov/table/ACSDT1Y2023.B25071?q=B25071) (1yr) | 2023 | |

### Note on HUD USPS Address Data
HUD's Aggregated United States Postal Service (USPS) Administrative Data on Address Vacancies is a key dataset for our analysis and is used to measure net address growth. This data is collected by USPS postal workers to facilitate mail delivery and provides quarterly counts of addresses serviced by USPS. These addresses are categorized as one of three types: residential, business, and other. An address within these three types can be coded as active, vacant, or no-stat. The address is considered vacant if delivery staff on urban routes have identified that mail has not been collected for 90 days or longer. Some addresses are considered to be "no-stat" (neither occupied nor non-occupied) for many reasons, including (1) being on a rural route that has been vacant for 90 days or longer, (2) the address is for a structure under construction and is not yet occupied, and (3) being identified by a carrier as not likely to be active for some time.

Due to issues with the data available online, the analysis uses the 2020 standardized file received directly from HUD. We are not authorized to publicly share the files through this repository. To retrieve the data from HUD and fully replicate the study using our R code, contact [Alexander Din](https://www.alexdin.com/) at Alexander.M.Din@hud.gov.
