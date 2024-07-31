# [] transition to pulling pip data from the database
# [] clean column names (if needed)

load_pip <- function(){
  
  # Read in the csv extract
  df_pip_raw <- read_csv("./Data/PIP_HIV_extract.csv")
  

  # Clean the pip data
  df_pip <- df_pip_raw |>
    mutate(IndicatorCode = paste0(partner_indicator_category_name, partner_indicator_sequence)) |> 
    select(activity_area_name, 
           Continent = continent_name, 
           ISO3Continent = iso3code_continent, 
           ISO33SubContinent = iso3code_sub_continent,
           Region = who_region_name, 
           Country = country_name, 
           ISO3 = iso3code_country,
           IncomeLevel = current_income_level,
           RiskIndex = external_risk_index, 
           DataType = indicator_data_type,
           Year = year,
           Indicator = partner_indicator_label_name, 
           IndicatorLabel = partner_indicator_name,
           IndicatorCode,
           Value = partner_indicator_value_internal,
           ValueLower = partner_indicator_value_internal_lower_estimate,
           ValueUpper = partner_indicator_value_internal_higher_estimate,
           ValuePublic = partner_indicator_value_public,
           valuePublicFormatted = partner_indicator_value_public_formatted)
  
  
  remove(df_pip_raw)
  
  # Return the clean dataset
  return(df_pip)
}
