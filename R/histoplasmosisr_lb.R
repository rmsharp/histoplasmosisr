#' Returns dataframe with Ids, the date, the day of the year, 
#' and the month of the year (as an integer)
#' each animal was first noted to have histoplasmosis.
#' 
#' @param histo_data name and path of the Excel file containing animal Ids
#' and the date each animal was noted to have histoplasmosis.
#' @export
get_affected_animals_df <- function (histo_data) {
  df <- readWorksheet(histo_data, sheet = 1)
  names(df) <-  c("id", "first_noted")
  
  df$id <- blank_fill_ids(df$id)
  df$day_of_year <- as.integer(strftime(df$first_noted, format = '%j'))
  df$month <- as.integer(strftime(df$first_noted, format = '%m'))
  df
}
#' Returns dataframe with Ids of animals that were in a corral within 10 
#' days of being first seen with histoplasmosis.
#' 
#' @param conn database connection object
#' @export
roundup_animals <- function(conn) {
  sql <- str_c(
    "SELECT x.id FROM ", X_ed_hist, " x
    WHERE x.id in (select l.id from location l
      WHERE l.move_date_tm < dateadd(day, -10, x.first_noted)
        AND l.exit_date_tm > dateadd(day, -10, x.first_noted)
        AND cast(l.exit_date_tm as DATE) <= x.first_noted 
        AND l.location in (", 
        vector2string(define_corral_locations(conn), SS = ", "), ") )")
  not_roundup_df <- sqlQuery(conn, sql, stringsAsFactors = FALSE)
  not_roundup_df
}
#' Creates ed_hist database table with id and first_noted columns
#' 
#' @param conn database connection object
#' @param X_ed_hist character vector of length one with the name of the table to 
#' create
#' @export
create_X_ed_hist <- function (conn, X_ed_hist) {
  create_status <- sqlQuery(conn, str_c(
    "CREATE TABLE ", X_ed_hist, " ( 
      id CHAR(6),
      first_noted DATE) "))
}              

#' Inserts ids and dates that correspond to when the Histoplasmosis infection
#' was first noted.
#' 
#' @param conn database connection object
#' @param X_ed_hist name of database table receiving inserts
#' @param df dataframe containing data being inserted.
#' @export
insert_id_first_noted <- 
  function(conn, X_ed_hist, df) {
    for (i in seq_along(df$id)) {
      status <- sqlQuery(conn, str_c(
        "insert into ", X_ed_hist, " 
        (id, first_noted) 
        values ('", df$id[i], "', '", 
        strftime(df$first_noted[i], format = "%m/%d/%Y"), "')"))
    }
  }

#' Creates a dataframe with a row for each day of life prior to the date 
#' Histoplasmosis was first noted for each animal in the table with the name
#' contained in X_ed_hist.
#' 
#' @param conn database connection object
#' @param X_ed_hist name of database table with Ids and dates Histoplasmosis
#' was first noted.
#' @export
make_daily_df <- function(conn, X_ed_hist) {
  daily_df <- sqlQuery(conn, str_c(
    "SELECT c.target_date , ",
    "l.location , ",
    "ad.id ",
    "FROM dbo.location l ",
    "INNER JOIN ", X_ed_hist, " h ",
    "ON h.id = l.id ",
    "INNER JOIN dbo.acq_disp ad ",
    "ON ad.id = l.id ",
    "INNER JOIN master m ",
    "ON m.id = l.id ",
    "CROSS JOIN dbo.sm_cal c ",
    "WHERE c.target_date BETWEEN cast(m.birth_date as date) ",
    "AND h.first_noted ",
    "AND c.target_date BETWEEN ad.acq_date_tm ",
    "AND ISNULL(ad.disp_date_tm, ",
    "h.first_noted) ",
    "AND c.target_date BETWEEN l.move_date_tm ",
    "AND ISNULL(l.exit_date_tm, ",
    "h.first_noted) ") , stringsAsFactors = FALSE)
  housing_types <- get_housing_types(conn)
  daily_df$gang <- ifelse(daily_df$location %in% housing_types$gang, 1, 0)
  daily_df$corral <- ifelse(daily_df$location %in% housing_types$corral, 1, 0)
  daily_df$single <- ifelse(daily_df$location %in% housing_types$single, 1, 0)
  daily_df  
}

#' Returns dataframw with the days_alive, days_gang, percent_gang, days_corral,
#' percent_corral, days_single, and percent_single columns defined.
#' 
#' @param df dataframe being used with one record per animal that will take 
#' sums and percents.
#' @param daily_df dataframe that has one record per day for each id from birth
#' to first_noted date.
#' @export
add_location_type_percents <- function (df, daily_df) {
  df$days_alive <- 1 + (df$first_noted - df$birth_date) / edays(1)
  
  df$days_gang <- sapply(df$id, FUN = function(id) {get_days_gang(id, daily_df)})
  df$percent_gang <- (df$days_gang / df$days_alive) * 100
  df$days_corral <- sapply(df$id, FUN = function(id) {get_days_corral(id, daily_df)})
  df$percent_corral <- (df$days_corral / df$days_alive) * 100
  df$days_single <- sapply(df$id, FUN = function(id) {get_days_single(id, daily_df)})
  df$percent_single <- (df$days_single / df$days_alive) * 100
  df
}

#' Returns a numeric vector with the locations that are classified as gang
#' housing structures.
#' 
#' @param conn database connection object
#' @export
define_gang_locations <- function(conn) {
  location <- sqlQuery(conn, str_c(
  "SELECT vl.location from animal.dbo.valid_locations vl 
  WHERE (vl.description like '%gang%' 
    OR vl.description like '%breeding%'
    OR vl.location >= 114 and vl.location < 115)
    AND vl.location < 200 
    AND vl.location not in (100.00, 100.01, 100.02, 105.00)"))$location
  location[location >= 1]
}

#' Returns a numeric vector with the locations that are classified as corral
#' housing structures.
#' 
#' @param conn database connection object
#' @export
define_corral_locations <- function(conn) {
  corral <- sqlQuery(conn, str_c(
    "SELECT vl.location from animal.dbo.valid_locations vl ",
    "WHERE vl.description like '%corral%' "))
  corral$location[corral$location >= 1]
}

#' Returns a numeric vector with the locations that are classified as single
#' housing structures.

#' @param conn database connection object
#' @export
define_single_locations <- function(conn) {
  group <- sqlQuery(conn, str_c(
    "SELECT vl.location from animal.dbo.valid_locations vl ",
    "WHERE vl.group_housing_flag = 'N' "))
  group$location[group$location >= 1]
}

#' Returns a list of length 3 with a vector of gang cage locations, a vector
#' of corral locations, and a vector of single locations.
#' 
#' @param conn database connection object
#' @export
get_housing_types <- function(conn) {
  list(gang = define_gang_locations(conn),
       corral = define_corral_locations(conn),
       single = define_single_locations(conn))
}

#' Returns the number of days a specific animal was in gang cages.
#' 
#' @param id animal id
#' @param daily_df dataframe containing the animal and location data
#' @export
get_days_gang <- function(id, daily_df) {
  length(daily_df$id[daily_df$id == id & daily_df$gang == 1])
}

#' Returns the number of days a specific animal was in a corral.
#' 
#' @param id animal id
#' @param daily_df dataframe containing the animal and location data
#' @export
get_days_corral <- function(id, daily_df) {
  length(daily_df$id[daily_df$id == id & daily_df$corral == 1])
}

#' Returns the number of days a specific animal was in a single housing 
#' location.
#' 
#' @param id animal id
#' @param daily_df dataframe containing the animal and location data
#' @export
get_days_single <- function(id, daily_df) {
  length(daily_df$id[daily_df$id == id & daily_df$single == 1])
}

#' Returns a dataframe with the Id, date, and the number of males and females
#' of a specified species on that date.
#' 
#' @param conn database connection object
#' @param affected_df dataframe containing data about affected animals (e.g.,
#' id, sex, date Histoplasmosis was first noted, etc.)
#' @param target_date_df dataframe with the dates (target_date)
#' @param arc_species_code of all animals being counted.
#' @export
get_male_female_ratio <- function(conn, affected_df,arc_species_code) {
  target_date_df <- data.frame(target_date = unique(affected_df$first_noted))
  sql <- list (
    "DROP TABLE #temp",
    "DROP TABLE #tempm",
    "DROP TABLE #tempf",
    "CREATE TABLE #temp (target_date date, males int, females int)",
    "CREATE TABLE #tempm (target_date date, males int)",
    "CREATE TABLE #tempf (target_date date, females int)",
    str_c(
      "INSERT INTO #tempm (target_date, males)
      SELECT dd.target_date, count(dd.target_date)
      FROM daily_demo AS dd
      WHERE target_date in ('", 
        vector2string(strftime(target_date_df$target_date, format = "%m/%d/%Y"), 
                     SS = "', '"), "')
        AND sex = 'M'
      GROUP by target_date"),
    str_c(
      "INSERT INTO #tempf (target_date, females)
      SELECT dd.target_date, count(dd.target_date)
      FROM daily_demo AS dd
      WHERE target_date in ('", 
        vector2string(strftime(target_date_df$target_date, format = "%m/%d/%Y"), 
                      SS = "', '"), "')
        AND sex = 'F'
      GROUP by target_date"),
    str_c(
      "INSERT INTO #temp (target_date, males, females)
      SELECT m.target_date, m.males, f.females 
      FROM #tempm m 
      INNER JOIN #tempf f ON m.target_date = f.target_date "))
  sqlOutput <- lapply(sql, function(x) sqlQuery(conn, x))
  
  male_female_ratio_df <- sqlQuery(conn, str_c(
    "SELECT *
    FROM #temp"), stringsAsFactors = FALSE)
  male_female_ratio_df$target_date <- 
    as.POSIXct(male_female_ratio_df$target_date)
  merge(affected_df, male_female_ratio_df, by.x = "first_noted", by.y = "target_date")
}

#' Returns relative risk for a cohort study. 
#' 
#' Code adapted from 
#' http://a-little-book-of-r-for-biomedical-statistics.readthedocs.org/en/latest/src/biomedicalstats.html
#' @param mymatrix 2 by 2 matrix
#' @param alpha Type 1 error rate
#' @param reference_row (unexposed or control group)
#' @export
calc_relative_risk <- function(mymatrix, alpha=0.05, reference_row=2)
{
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  relative_risk <- numeric(numrow)
  lowervalue <- numeric(numrow)
  uppervalue <- numeric(numrow)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    disease_unexposed <- mymatrix[reference_row,1]
    control_unexposed <- mymatrix[reference_row,2]
    if (i != reference_row)
    {
      disease_exposed <- mymatrix[i,1]
      control_exposed <- mymatrix[i,2]
      tot_exposed <- disease_exposed + control_exposed
      tot_unexposed <- disease_unexposed + control_unexposed
      prob_disease_given_exposed <- disease_exposed/tot_exposed
      prob_disease_given_unexposed <- disease_unexposed/tot_unexposed
      
      # calculate the relative risk
      relative_risk[i] <- prob_disease_given_exposed/prob_disease_given_unexposed
      #print(paste("category =", rowname, ", relative risk = ",relative_risk))
      
      # calculate a confidence interval
      confidence_level <- (1 - alpha)*100
      sigma <- sqrt((1/disease_exposed) - (1/tot_exposed) +
                      (1/disease_unexposed) - (1/tot_unexposed))
      # sigma is the standard error of estimate of log of relative risk
      z <- qnorm(1-(alpha/2))
      lowervalue[i] <- relative_risk[i] * exp(-z * sigma)
      uppervalue[i] <- relative_risk[i] * exp( z * sigma)
      #print(paste("category =", rowname, ", ", confidence_level,
      #            "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
  list(RR = relative_risk[-reference_row], 
       lowervalue = lowervalue[-reference_row], 
       uppervalue = uppervalue[-reference_row],
       alpha = rep(alpha, numrow - 1),
       rownames = myrownames[-reference_row])
}

#'
#' 
#' specified for each animal in the dataframe
# \item Housing type (corral, gang, single)
# \item Housing location (near vegetation or away from vegetation)
# \item Housing surface types (highly porous rock and concrete or 
#                              Stonehard surfaces)
# \item Seasons of the year
# \item Birth location
# \item Days in groups
# \item Over time (has there been and increase or decrease in incidence).
