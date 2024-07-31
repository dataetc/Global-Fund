library(tidyverse)

source("./R/load_pip.R")

# Load datasets:
df_pip <- load_pip()

countryOfInterest = "Kenya"

# Which indicators from PIP are we interested in?
vec_indicatorsOfInterest <- c(
  # HIV
  "HIV1", 
  "HIV121",
  "HIV440",
  "HIV46","HIV163", "HIV168",
  "HIV437", "HIV479",
  "HIV4",
  "Malaria454",
  "HIV187",
  "HIV586",
  "HIV581",
  "HIV576",
  "HIV591",
  "HIV16",
  "HIV709", "HIV710",
  "HIV586",
  
  # TB
  "TB11", 
  "TB58",
  "TB1",
  "TB249"
  # Malaria
  
)


# Subset the input data to the country of interest:
df_pip_thisCountry <- df_pip |>
  filter(Country %in% countryOfInterest) |> 
  print()


# Calculations ----
calculateChange <- function(code,
                            calculationType = c("Percent"), 
                                   referenceYear = 2017,
                                   latestYear = 2022){
  
  if(calculationType == "Percent"){
  df_calcs <- df_pip |>
    filter(IndicatorCode %in% code) |>
    filter(Year %in% c(referenceYear, max(Year))) |>
    mutate(Year = case_when(
      Year == referenceYear ~ "Ref",
      Year == max(Year) ~ "Latest",
      TRUE ~ "Missing"
    )) |>
    select(Country, ISO3, Indicator, Year, Value) |>
    group_by(Country, ISO3) |> 
    pivot_wider(names_from = "Year", values_from = "Value") |>
    ungroup() |>
    mutate(Percent = (Latest - Ref)/Ref)

return(df_calcs)
  }
}

calculatePercent <- function(code_numerator,
                             code_denominator,
                             year = "Latest"){
  
  
  df_calcs <- df_pip |>
    filter(IndicatorCode %in% c(code_numerator, code_denominator)) |>
    filter((Year == max(Year) & year == "Latest") | Year == year) |>
    mutate(Code = case_when(
      IndicatorCode == code_numerator ~ "N",
      IndicatorCode == code_denominator ~ "D",
      TRUE ~ "Missing"
    )) |>
    select(Country, ISO3, Code, Value) |> 
    group_by(Country, ISO3) |> 
    pivot_wider(names_from = "Code", values_from = "Value") |>
      mutate(Percent = N/D) |> 
    select(Country, ISO3, N, D, Percent)
  
  return(df_calcs)
}

cleanPip_byCode <- function(code){
  df_pip |>
    filter(IndicatorCode == code) |>
    select(Country, ISO3, Year, Value)
}

                                   
                                  
# HIV ----

# Impact
# New infections

df_hiv_newInfections <- calculateChange(code = "HIV1",
                                        referenceYear =  2017,
                                        latestYear = 2022)
    

# Deaths
df_hiv_deaths <- calculateChange(code = "HIV121",
                                 referenceYear =  2016,
                                 latestYear = 2021) 

# Service
# % of PLHIV who know their status
df_hiv_percentKnowStatus <- calculatePercent(code_numerator = "HIV440",
                                             code_denominator = "HIV46")

# % PLHIV on ART
df_hiv_percentOnART <- calculatePercent(code_numerator = "HIV163",
                                             code_denominator ="HIV46")

# to do: "HIV163"/("HIV46" or "HIV168") ----
# % PLHIV with suppressed viral load
df_hiv_percentSVL <- calculatePercent(code_numerator = "HIV437",
                                        code_denominator ="HIV46")

# to do:  "HIV437"/("HIV46" or "HIV479") ----

# PMTCT
# % of 0-14 new infection among total
df_hiv_newInfections_0to14 <- calculatePercent(code_numerator = "HIV4",
                    code_denominator ="HIV1")

"HIV4"/"HIV1"
# % Pregnant women attending ANC
df_hiv_pregnanceWomenAttendingANC <- cleanPip_byCode(code = "Malaria454")
"Malaria454"
# % HIV+ pregnant preventing PMTCT
df_hiv_hivPositivePregnancyWomen_MTCT <- cleanPip_byCode(code = "HIV187")
 
# KP
# ART coverage among SW
df_hiv_kp_SW <-  cleanPip_byCode(code = "HIV586")

# ART coverage among MSM
df_hiv_kp_MSM <-  cleanPip_byCode(code = "HIV581")

# ART coverage among TG
df_hiv_kp_TG <-  cleanPip_byCode(code = "HIV576")

# ART coverage among PWID
df_hiv_kp_PWID <-  cleanPip_byCode(code = "HIV591")


# YP
# New infection
df_hiv_yp_infections <- calculateChange(code = "HIV16",
                                        referenceYear =  2017,
                                        latestYear = 2022) 

# Knowledge about HIV prevention
cleanPip_byCode(code = "HIV709")
cleanPip_byCode(code = "HIV710")

", HIV710"

# % Infants among new infections
"HIV4"/"HIV1"

# ART coverage among SW 
"HIV586"



# Check results for this country
thisCountry = "Kenya"
# Impact
df_hiv_newInfections |> filter(Country %in% c(thisCountry))
df_hiv_deaths |> filter(Country %in% c(thisCountry))
# Service
df_hiv_percentKnowStatus |> filter(Country %in% c(thisCountry))
df_hiv_percentOnART |> filter(Country %in% c(thisCountry))
df_hiv_percentSVL |> filter(Country %in% c(thisCountry))
# PMTCT
df_hiv_newInfections_0to14 |> filter(Country %in% c(thisCountry))
df_hiv_pregnanceWomenAttendingANC |> filter(Country %in% c(thisCountry))
df_hiv_hivPositivePregnancyWomen_MTCT |> filter(Country %in% c(thisCountry))
# KP
df_hiv_kp_SW |> filter(Country %in% c(thisCountry))
df_hiv_kp_MSM |> filter(Country %in% c(thisCountry))
df_hiv_kp_TG |> filter(Country %in% c(thisCountry))
df_hiv_kp_PWID |> filter(Country %in% c(thisCountry))
# YP 
df_hiv_yp_infections |> filter(Country %in% c(thisCountry))
