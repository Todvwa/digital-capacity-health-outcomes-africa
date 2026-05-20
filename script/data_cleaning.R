
# Setup
library(tidyverse)

dir.create("cleaned", showWarnings = FALSE)


# Full African countries list (can expand/adjust later)
countries <- c(
  "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
  "Cabo Verde","Cameroon","Central African Republic","Chad",
  "Comoros","Congo","Democratic Republic of the Congo","Djibouti",
  "Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
  "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Ivory Coast",
  "Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi","Mali",
  "Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
  "Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles",
  "Sierra Leone","Somalia","South Africa","South Sudan","Sudan",
  "Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"
)


# Country Standardization
standardize_country <- function(x) {
  recode(x,
         "Swaziland" = "Eswatini",
         "Côte d'Ivoire" = "Ivory Coast",
         "Congo, Dem. Rep." = "Democratic Republic of the Congo",
         "Congo, Rep." = "Congo",
         "Egypt, Arab Rep." = "Egypt",
         "Gambia, The" = "Gambia",
         "Tanzania, United Republic of" = "Tanzania",
         .default = x)
}


## Cleaning function; 
# WDI style dataset
clean_wdi <- function(file, varname) {
  read_csv(file, skip = 4) %>%
    pivot_longer(cols = matches("^\\d{4}$"),
                 names_to = "year",
                 values_to = varname) %>%
    transmute(
      country = standardize_country(`Country Name`),
      year = as.integer(year),
      !!varname := as.numeric(.data[[varname]])
    ) %>%
    filter(country %in% countries)
}



# OWDI style dataset
clean_owid <- function(file, varname) {
  read_csv(file, show_col_types = FALSE) %>%
    rename(
      country = 1,
      year = 3,
      value = 4
    ) %>%
    transmute(
      country = standardize_country(country),
      year = as.integer(year),
      !!varname := as.numeric(value)
    ) %>%
    filter(country %in% countries)
}

# WHO
clean_who <- function(file, varname) {
  df <- read_csv(file, show_col_types = FALSE)
  
  df %>%
    transmute(
      country = standardize_country(GEO_NAME_SHORT),
      year    = as.integer(DIM_TIME),
      !!varname := as.numeric(VALUE)
    ) %>%
    filter(country %in% countries)
}



#############################
clean_who <- function(file, varname, value_col) {
  df <- read_csv(file, show_col_types = FALSE)
  
  out <- df %>%
    transmute(
      country = standardize_country(GEO_NAME_SHORT),
      year    = as.integer(DIM_TIME),
      !!varname := suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    filter(country %in% countries)
  
  if (all(is.na(out[[varname]]))) {
    warning(paste(varname, "is completely NA - check", value_col, "in", file))
  }
  
  return(out)
}
################################

## CLEANING FILES 

# Outcomes 
u5   <- clean_wdi("under_5_mortality.csv", "u5_mortality")
neo  <- clean_owid("neonatal-mortality-rate.csv", "neonatal_mortality")
mmr    <- clean_who("maternal_mortality_rate.csv",
                    "maternal_mortality",
                    "RATE_PER_100000_N")

# Digital - Bytes
internet1 <- clean_wdi("internet_user_pcnt_population.csv", "internet_users")
internet2 <- clean_owid("share-of-individuals-using-the-internet.csv", "internet_users_alt")

mobile <- clean_wdi("mobile-_cellular_subscriptions_per100.csv", "mobile_subs")
ict <- clean_owid("ict-adoption-per-100-people.csv", "ict_index")


# Health Systems - Bricks
beds   <- clean_wdi("hospital_bed_per_1000.csv", "hospital_beds")
nurses <- clean_wdi("nurses_per_1000.csv", "nurses")
phys   <- clean_wdi("physician_per_1000.csv", "physicians")

# Access/Delivery

dpt3   <- clean_who("dpt3_immu.csv", "dpt3", "RATE_PER_100_N")
anc4 <- clean_owid("share-of-mothers-receiving-at-least-four-antenatal-visits-during-pregnancy.csv", "anc4")
postnatal <- clean_owid("share-of-mothers-receiving-medical-care-after-giving-birth.csv", "postnatal")


# Health Risks
hiv_prev <- clean_wdi("Prevalence_of_HIV_ 15-49).csv", "hiv_prev")
hiv_art <- clean_owid("share-of-pregnant-women-with-hiv-that-receive-antiretroviral-therapy.csv", "hiv_art")
malnutrition <- clean_owid("malnutrition-share-of-children-who-are-underweight.csv", "malnutrition")
anemia <- clean_owid("prevalence-of-anemia-in-women-of-reproductive-age-aged-15-29.csv", "anemia")
vitA <- clean_owid("prevalence-of-vitamin-a-deficiency-in-children.csv", "vitA")


# Demographics
fertility <- clean_owid("children-born-per-woman.csv", "fertility")
adol_fert <- clean_owid("adolescent-fertility-15-19.csv", "adolescent_fertility")

# Socioeconomic
gdp <- clean_owid("gdp-per-capita-worldbank.csv", "gdp_pc")
literacy <- clean_owid("cross-country-literacy-rates.csv", "literacy")

# Infrastructure
water <- clean_owid("population-using-at-least-basic-drinking-water.csv", "water")
sanitation <- clean_owid("share-of-population-with-improved-sanitation-faciltities.csv", "sanitation")

# Health Spending 
health_exp <- clean_wdi("Current_health_expenditure.csv", "health_exp")
health_gdp <- clean_owid("total-healthcare-spending-as-a-share-of-gdp.csv", "health_gdp")
public_health <- clean_owid("public-healthcare-spending-as-a-share-of-gdp.csv", "public_health")


# Merge
panel <- u5 %>%
  full_join(neo, by = c("country","year")) %>%
  full_join(mmr, by = c("country","year")) %>%
  full_join(internet1, by = c("country","year")) %>%
  full_join(mobile, by = c("country","year")) %>%
  full_join(ict, by = c("country","year")) %>%
  full_join(beds, by = c("country","year")) %>%
  full_join(nurses, by = c("country","year")) %>%
  full_join(phys, by = c("country","year")) %>%
  full_join(dpt3, by = c("country","year")) %>%
  full_join(anc4, by = c("country","year")) %>%
  full_join(postnatal, by = c("country","year")) %>%
  full_join(hiv_prev, by = c("country","year")) %>%
  full_join(hiv_art, by = c("country","year")) %>%
  full_join(malnutrition, by = c("country","year")) %>%
  full_join(anemia, by = c("country","year")) %>%
  full_join(vitA, by = c("country","year")) %>%
  full_join(fertility, by = c("country","year")) %>%
  full_join(adol_fert, by = c("country","year")) %>%
  full_join(gdp, by = c("country","year")) %>%
  full_join(literacy, by = c("country","year")) %>%
  full_join(water, by = c("country","year")) %>%
  full_join(sanitation, by = c("country","year")) %>%
  full_join(health_exp, by = c("country","year")) %>%
  full_join(health_gdp, by = c("country","year")) %>%
  full_join(public_health, by = c("country","year"))

# Restrict Years 
panel <- panel %>%
  filter(year >= 2000, year <= 2022) %>%
  arrange(country, year)

# Save
write_csv(panel, "africa_panel_new.csv")


summary(panel)
