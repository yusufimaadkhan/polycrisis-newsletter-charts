# Load packages

pacman::p_load(tidyverse, readr, readxl, janitor, here)

# set paths to make working with "here" package easier (note - these are all relative to project)

input_path <- "newsletter-charts/1-hockey-sticks-and-crosses-globalisation/inputs"
output_path <- "newsletter-charts/1-hockey-sticks-and-crosses-globalisation/outputs"

## Hockey stick of prosperity ----
## SOURCE - https://ourworldindata.org/grapher/world-gdp-over-the-last-two-millennia

world_gdp_2_millenia <- read_csv(
  here(input_path, 
       "world-gdp-over-the-last-two-millennia.csv")) %>% 
  rename(Values = 4)

## Hockey stick of doom ----
## SOURCE  - https://ourworldindata.org/grapher/cumulative-co2-emissions-region?stackMode=absolute

## The colour choices and factor ordering decisions of OWID are very interesting. Perhaps its an automatic decision. Or perhaps its IDEOLOGICAL. I mean really. China appears most prominent because of the colour choice. When really the US and Europe lead in cumulative emissions. I should have disaggregated for the UK too. They come to about 4.6% of the cumulative emissions share. More than India/NA/Africa. 

cumulative_co2_emissions_region <- read_csv(
  here(input_path,
       "cumulative-co2-emissions-region.csv"))

cumulative_co2_emissions_region <- cumulative_co2_emissions_region %>% 
  clean_names() %>% 
  filter(entity %in% c(
    "China", 
    "India", 
    "Africa", 
    "Oceania", 
    "South America", 
    "North America (excl. USA)", 
    "United States", 
    "European Union (27)", 
    "Europe (excl. EU-27)", 
    "Asia (excl. China and India)" )) %>% # Same categories as OWID - not transport
  arrange(factor(entity, levels=c(
    "United States",
    "European Union (27)",
    "Asia (excl. China and India)",
    "China",
    "Europe (excl. EU-27)",
    "North America (excl. USA)",
    "India",
    "Africa",
    "South America",
    "Oceania"
  )))

# make a separate object for the inset stacked bar chart that will clearly show regional
# shares for cumulative emissions

cumulative_bar <- cumulative_co2_emissions_region %>% 
  filter(year==2021) %>% # keep final year to calculate cumulative shares
  mutate(total_cumulative = sum(cumulative_co2_emissions_zero_filled), 
         percent = cumulative_co2_emissions_zero_filled/(total_cumulative)) %>% 
  select(entity, percent) %>% 
  mutate(bar = "bar",
         labels = if_else(!entity%in%c("Oceania", "South America"),1,2)) # label because I need to adjust these positions slightly

## Hockey stick of hope ----

## SOURCE - https://ourworldindata.org/grapher/installed-solar-PV-capacity + modelled values from solar power europe. I believe Simon Evans put these together. Link below

installed_solar_PV_cap <- read_csv(
  here(input_path, 
       "installed-solar-PV-capacity.csv")) 

installed_solar_PV_cap <- installed_solar_PV_cap %>%  
  clean_names() %>% 
  filter(entity=="World") %>% 
  select(-code) %>% 
  mutate(projected = "Actual", 
         year = year + 1)  

# create projected rows - sourced from https://www.linkedin.com/posts/simon-evans-53091614_it-took-22yrs-for-global-solar-power-activity-7074407505132445696-orvS and https://www.solarpowereurope.org/insights/market-outlooks/global-market-outlook-for-solar-power-2023-2027-1#downloadForm

projected_rows <- data.frame(
  entity = c("World","World","World","World","World","World"),
  year = c(2023, 2024, 2025, 2026, 2027, 2028),
  solar_capacity = c(1177, 1518, 1919, 2381, 2915, 3532), # see page 33 from report for hardcoded values
  projected = c("Projected","Projected","Projected","Projected","Projected","Projected"),
  stringsAsFactors = FALSE
) 

installed_solar_PV_cap <- installed_solar_PV_cap %>% 
  bind_rows(projected_rows)%>% 
  filter(year>=2000)

## Cross of power ----

## SOURCE https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD

## This is China and US as GDP PPP constant 2017 int $ as a share of world DP PPP constant 2017 int $

gdp <- read_csv(
  here(input_path,
        "API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_5734689.csv"), 
        skip = 3)

gdp <- gdp %>% 
  select(1, 2, 5:68) %>% 
  filter(`Country Code`%in%c("WLD", "CHN", "USA")) %>% 
  select(1, 2, 33:65) %>% 
  pivot_longer(3:35, names_to = "year", values_to = "gdp_ppp_2017_int_$")

gdp_world <- gdp %>% 
  filter(`Country Code`=="WLD") %>% 
  rename("world_amount"="gdp_ppp_2017_int_$") %>% 
  select(-`Country Code`, -`Country Name`)

df_chn_usa <- gdp %>% 
  filter(`Country Code`%in%c("CHN", "USA")) %>% 
  left_join(gdp_world,by="year") %>% 
  mutate(share=`gdp_ppp_2017_int_$`/world_amount) %>% 
  clean_names()


## Cross of income ----

## SOURCE - https://wid.world/ - shares of national income

## Used excel spreadsheet because WID CSV were throwing errors with semicolon separation

WID_p1vp50_US <- read_excel(
  here(input_path,
       "WID_Data_20062023-185112.xlsx")) 

WID_p1vp50_US <- WID_p1vp50_US %>% 
  clean_names() %>%  # the variables are especially annoying in this one 
  filter(year>=1980) %>% 
  mutate(percentile = case_match(percentile,
                                 "p99p100" ~ "Top 1% \nUS",
                                 "p0p50" ~ "Bottom 50% \nUS"))


## Cross of deindustrialisation ----

## SOURCES - https://ourworldindata.org/structural-transformation-and-deindustrialization-evidence-from-todays-rich-countries

employment_by_economic_sector <- read_csv(
  here(input_path,
    "employment-by-economic-sector.csv")) 

employment_by_economic_sector <- employment_by_economic_sector %>% 
  clean_names() %>% 
  filter(code=="USA") %>% 
  rename("number_employed_manufacturing"="number_of_people_employed_in_industry_herrendorf_et_al_data",
         "number_employed_services" = "number_of_people_employed_in_services_herrendorf_et_al_data") %>% 
  mutate(total = number_employed_agri + number_employed_manufacturing + number_employed_services,
         number_employed_agri_man = number_employed_agri + number_employed_manufacturing,
         share_agri_man = number_employed_agri_man/total,
         share_services = number_employed_services/total) %>% 
  select(entity, year, share_agri_man, share_services) %>% 
  pivot_longer(3:4, names_to = "category", values_to = "share") %>% 
  filter(!is.na(share)) %>% 
  mutate(labels = case_match(category,
                             "share_agri_man"~"Agriculture & manufacturing",                                                 
                             "share_services"~"Services",                                                   
                             .default = category))


## Cross of tax ----

## SOURCE Office of Management and Budget. Historical Tables. Table 2.3 - https://www.whitehouse.gov/omb/budget/historical-tables/

## An alternative I considered -  DataF4 - F + G - https://taxjusticenow.org/appendix. It wasn't the same because of denominator and inclusion/exclusion of payroll benefits (I THINK)

tax_burden_data <- read_excel(
  here(input_path,
  "hist02z3_fy2024.xlsx"), 
  skip = 1)


tax_burden_data <- tax_burden_data %>% 
  clean_names() %>% 
  select(1:4) %>% 
  filter(fiscal_year>=1950,
         !fiscal_year>2022) %>% 
  mutate(social_insurance_and_retirement_receipts = as.numeric(social_insurance_and_retirement_receipts)) %>% 
  pivot_longer(2:4, names_to = "categories", values_to = "values") %>% 
  rename(year = fiscal_year) %>% 
  filter(!categories=="individual_income_taxes") %>% 
  mutate(labels = case_match(categories,
                             "corporation_income_taxes"~"Corporate income taxes",                                                 
                             "social_insurance_and_retirement_receipts"~"Payroll taxes",                                                   
                             .default = categories))



# Exports ----

write.csv(world_gdp_2_millenia, here(output_path, "world_gdp_2_millenia.csv"), row.names=FALSE)

write.csv(cumulative_co2_emissions_region, here(output_path, "cumulative_co2_emissions_region.csv"), row.names=FALSE)

write.csv(cumulative_bar, here(output_path, "cumulative_bar.csv"), row.names=FALSE)

write.csv(installed_solar_PV_cap, here(output_path, "installed_solar_PV_cap.csv"), row.names=FALSE)

write.csv(df_chn_usa, here(output_path, "df_chn_usa.csv"), row.names=FALSE)

write.csv(WID_p1vp50_US, here(output_path, "WID_p1vp50_US.csv"), row.names=FALSE)

write.csv(employment_by_economic_sector, here(output_path, "employment_by_economic_sector.csv"), row.names=FALSE)

write.csv(tax_burden_data, here(output_path, "tax_burden_data.csv"), row.names=FALSE)

