library(tidyverse)

df1 <- read.csv('intrvw18/fmli181x.csv')
df2 <- read.csv('intrvw18/fmli182.csv')
df3 <- read.csv('intrvw18/fmli183.csv')
df4 <- read.csv('intrvw18/fmli184.csv')
df5 <- read.csv('intrvw18/fmli191.csv')

# See this pdf for variable names & codebok: http://data.nber.org/ces/2004/Documentation/Intdoc04%20v2.pdf


df_all <- bind_rows(df1, df2, df3, df4, df5) %>% #pipe to only select the variables we care about
  select(NEWID, # Outcome Variable 
    EDUC_REF, # education of reference person // data will soon be split by educational level
    FINCBTXM, # consumer unit (CU) income before taxes in the past 12 months
    
    SHELTPQ, SHELTCQ, # shelter
         
    #FOOD split into two categories
    FDHOMEPQ, FDHOMECQ, # food at home
    FDAWAYPQ, FDAWAYCQ, # food away from home

    # HEALTH split into four categories
    HLTHINPQ, HLTHINCQ, # health insurance
    MEDSRVPQ, MEDSRVCQ, # medical services
    PREDRGPQ, PREDRGCQ, # prescription drugs
    MEDSUPPQ, MEDSUPCQ, # medical supplies

    #UTILITIES - we want to split it as utilities = water/gas/electricity and telephone = telephone
    NTLGASPQ, NTLGASCQ, # natural gas
    ELCTRCPQ, ELCTRCCQ, # electricity
    ALLFULPQ, ALLFULCQ, # all fuel
    WATRPSPQ, WATRPSCQ, # water
    TELEPHPQ, TELEPHCQ, # telephone
    
    EDUCAPQ, EDUCACQ, # education
    BBYDAYPQ, BBYDAYCQ, # childcare
    CASHCOPQ, CASHCOCQ, # cash contributions
    APPARPQ, APPARCQ, # apparel
    TOTEXPPQ, TOTEXPCQ) %>% # total expenditures)
  
  # Create variables for education level
  mutate(education_level = case_when(
    EDUC_REF <= 11 ~ "below high school",
    EDUC_REF == 12 ~ "high school grad",
    EDUC_REF %in% c(13, 14) ~ "some college",
    EDUC_REF > 14 ~ "at least bachelors"
  )
  ) %>%
  
  # Create a better name for income
  mutate( income = (FINCBTXM)) %>%
  mutate( income_quintile = (ntile(income, 5))) %>%
   
  # Create New Variables Containing: monthly average by using the previous quarter & current quarter data
  #mutate(FOOD_month_avg = (FOODPQ + FOODCQ)/3) %>% ## we have split the variables contained by the food variable into two variables
  mutate(SHELT_month_avg = (SHELTPQ + SHELTCQ)/3) %>%
  mutate(FOOD_atHome_month_avg = (FDHOMEPQ + FDHOMECQ)/3) %>%
  mutate(FOOD_away_month_avg = (FDAWAYPQ + FDAWAYCQ)/3) %>%
  #mutate(HEALTH_month_avg = (HEALTHPQ + HEALTHCQ)/3) %>% ## we have split the variables contained by health into four variables
  mutate(HEALTH_insurance_month_avg = (HLTHINPQ + HLTHINCQ)/3) %>%
  mutate(HEALTH_medical_services_month_avg = (MEDSRVPQ + MEDSRVCQ)/3) %>%
  mutate(HEALTH_perscription_drugs_month_avg = (PREDRGPQ + PREDRGCQ)/3) %>%
  mutate(HEALTH_medical_supplies_month_avg = (MEDSUPPQ + MEDSUPCQ)/3) %>%
  #mutate(UTIL_month_avg = (UTILPQ + UTILPQ)/3) %>% ## we have split the variables contained in utilities into two variables
  mutate(UTIL_month_avg = (NTLGASPQ + NTLGASCQ + ELCTRCPQ + ELCTRCCQ + ALLFULPQ + ALLFULCQ + WATRPSPQ + WATRPSCQ)/3) %>% # all utilities except telephone
  mutate(TEL_month_avg = (TELEPHPQ + TELEPHCQ)/3) %>%
  mutate(EDUCA_month_avg = (EDUCAPQ + EDUCACQ)/3) %>%
  mutate(BBYDAY_month_avg = (BBYDAYPQ + BBYDAYCQ)/3) %>%
  mutate(CASHCO_month_avg = (CASHCOPQ + CASHCOCQ)/3) %>%
  mutate(APPAR_month_avg = (APPARPQ + APPARCQ)/3) %>%
  mutate(TOTEXP_month_avg = (TOTEXPPQ + TOTEXPCQ)/3)

# Create a new dataframe with only the monthly average values for each expenditure
df_monthly_averages <- select(df_all, NEWID, education_level, income, income_quintile,
                              SHELT_month_avg,
                              FOOD_atHome_month_avg,
                              FOOD_away_month_avg,
                              HEALTH_insurance_month_avg, 
                              HEALTH_medical_services_month_avg,
                              HEALTH_perscription_drugs_month_avg,
                              HEALTH_medical_supplies_month_avg,
                              UTIL_month_avg,
                              TEL_month_avg,
                              EDUCA_month_avg,
                              BBYDAY_month_avg,
                              CASHCO_month_avg, 
                              APPAR_month_avg,
                              TOTEXP_month_avg) %>%
  mutate(expense_ratio_month_avg = TOTEXP_month_avg/income)

df_monthly_averages %>%
  group_by(income_quintile) %>%
  summarize(income)

## Generate Summary Statistics for all monthly average variables 

# df %>% split(.$group) %>% map(summary)

# function for returning the mean for all non-zero values
mean_nonzero <- function(x) {
  mean <- mean(x[x!=Inf])
  return(mean)
}

# function for returning the median for all non-zero values
median_nonzero <- function(x) {
  median <- median(x[x!=Inf])
  return(median)
}

# function for finding the number of rows which are not zero (to be used to find the % of nonzero responses)
n_nonzero_resp <- function(column) {
    df_monthly_averages %>%
      filter(!!sym(column) !=0 ) %>%
      select(!!sym(column)) %>%
      nrow()
}

# function to generate a summary statistic for each education level
df_monthly_averages %>%
  group_by(income_quintile) %>% 
  summarize_at(vars(contains("io_month_avg")), funs(mean(.), mean_nonzero(.), median(.), median_nonzero(.))) %>% 
  gather(var, value, -income_quintile) %>%
  View()

df_monthly_averages %>%
  filter(df_monthly_averages$education_level == "below high school") %>% 
  summarize_at(vars(contains("month_avg")), funs(mean(.), mean_nonzero(.), median(.), median_nonzero(.))) %>% 
  gather(var, value, -education_level) %>%
  View()

below_hs <- filter(df_monthly_averages, education_level == "below high school")
hs_grad <- filter(df_monthly_averages, education_level == "high school grad")
some_college <- filter(df_monthly_averages, education_level == "some college")
at_least_bachelors <- filter(df_monthly_averages, education_level == "at least bachelors")

one <- filter(df_monthly_averages, income_quintile == 1)
two <- filter(df_monthly_averages, income_quintile == 2)
three <- filter(df_monthly_averages, income_quintile == 3)
four <- filter(df_monthly_averages, income_quintile == 4)
five <- filter(df_monthly_averages, income_quintile == 5)


# Get Summary Stats (for education level)

get_summary_stats <- function(data_frame) {
  data_frame %>%
    summarize() %>%
    summarize_at(vars(contains("income")), funs(median_nonzero(.))) %>% #, mean_nonzero(.), median(.), median_nonzero(.))) %>%
    gather() %>%
  View()
}

get_summary_stats(i5)

get_summary_stats(one)

i1 <- filter(df_monthly_averages, income_quintile == 1)
i2 <- filter(df_monthly_averages, income_quintile == 2)
i3 <- filter(df_monthly_averages, income_quintile == 3)
i4 <- filter(df_monthly_averages, income_quintile == 4)
i5 <- filter(df_monthly_averages, income_quintile == 5)


 

# function to generate a summary statistic for each income quintile
df_monthly_averages %>%
  group_by(income_quintile) %>% 
  summarize_at(vars(contains("month_avg")), funs(mean(.), mean_nonzero(.), median(.), median_nonzero(.))) %>% 
  View()


ntile(df_monthly_averages$FINCBTXM, 5) %>% View()

#execute function to find the number of nonzero rows for Shelter expenses
n_nonzero_resp("SHELT_month_avg")


# Prints to the console: Mean and Median first: unconditionally, and secondly: with the condition x!=0 for all month_avg variables 
df_all %>%
  summarize_at(vars(contains("month_avg")), funs(mean(.), mean_nonzero(.), median(.), median_nonzero(.))) %>% 
  gather()


## Plot histograms for each variable

# create a function for making histograms
  # input data_column is the column name within the data frame df_monthly_averages
  # input bin_width is the width of the bin, in dollars
  # xlim and ylim respectively are the upper limmits on the x and y axis
create_histogram <- function(data_column, bin_width, xlim, ylim) {
  df_monthly_averages %>%
    ggplot(aes(!!sym(data_column))) +
      geom_histogram(color = "pink", fill = "#8C1515", mapping = NULL, data = NULL, stat = "bin",
                   position = "stack",binwidth = bin_width, bins = NULL, #center = -bin_width/2,
                   boundary = 0, na.rm = TRUE, show.legend = TRUE, inherit.aes = TRUE) +
      xlim(c(-bin_width,xlim)) +
      # ylim(c(0, 20000)) +
      ylim(c(0,ylim)) +
      labs(title=data_column, caption="Consumer Expenditure Survey") +
      theme_minimal()
}

#simpler version of the histogram function without xlim and ylim variables
simple_histogram <- function(data_column, bin_width) {
  df_monthly_averages %>%
    ggplot(aes(!!sym(data_column))) +
    geom_histogram(color = "pink", fill = "#8C1515", mapping = NULL, data = NULL, stat = "bin",
                   position = "stack",binwidth = bin_width, bins = NULL, #center = -bin_width/2,
                   boundary = 0, na.rm = TRUE, show.legend = TRUE, inherit.aes = TRUE) +
    labs(title=data_column, caption="Consumer Expenditure Survey") +
    theme_minimal()
}

# Create histograms for each variable inputs = (data, binwidth, x,y)
create_histogram("SHELT_month_avg", 10, 5000, 1100)
create_histogram("FOOD_atHome_month_avg", 50, 2000, 3400)
create_histogram("FOOD_away_month_avg", 50, 2000, 3900)
create_histogram("HEALTH_insurance_month_avg", 20, 1400, 2600)
create_histogram("HEALTH_medical_services_month_avg", 50, 1000, 5100)
create_histogram("HEALTH_perscription_drugs_month_avg", 10, 600, 2700)
create_histogram("HEALTH_medical_supplies_month_avg", 50, 1000, 1400)
create_histogram("UTIL_month_avg", 1, 1000, 140) # try with 5 - shape of graph
create_histogram("TEL_month_avg", 1, 400, 200)
create_histogram("EDUCA_month_avg", 50, 3000, 1200)
create_histogram("BBYDAY_month_avg", 50, 3700, 190)
create_histogram("CASHCO_month_avg", 100, 2500, 7000)
create_histogram("APPAR_month_avg", 50, 500, 18000)
create_histogram("TOTEXP_month_avg", 250, 20000, 1500)

# If we want to make a simpler histogram without xlim and ylim 
simple_histogram("APPAR_month_avg", 50)

# Create summary for some of the monthly averages
summary(df_monthly_averages$APPAR_month_avg)
summary(df_monthly_averages$TOTEXP_month_avg)






  