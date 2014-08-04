#' Creates ed_hist database table with id and first_noted columns
#' 
#' @param conn database connection object
#' @param X_ed_hist character vector of length one with the name of the table to 
#' create
#' @export
create_X_ed_hist <- function (conn, X_ed_hist) {
  create_status <- sqlQuery(conn, str_c(
    "CREATE TABLE ", X_ed_hist, " ( 
      id VARCHAR(6),
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
make_new_df <- function(conn, X_ed_hist) {
  sqlQuery(conn, str_c(
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
}

#' Returns a numeric vector with the locations that are classified as gang
#' housing structures.
#' 
#' @param conn database connection object
#' @export
define_gang_locations <- function(conn) {
  location <- sqlQuery(conn, str_c(
  "SELECT vl.location from valid_locations vl 
  WHERE (vl.description like '%gang%' 
    OR vl.description like '%breeding%'
    OR vl.location >= 114 and vl.location < 115)
    AND vl.location < 200 "))$location
  location[location >= 1]
}

#' Returns a numeric vector with the locations that are classified as corral
#' housing structures.
#' 
#' @param conn database connection object
#' @export
define_corral_locations <- function(conn) {
  corral <- sqlQuery(conn, str_c(
    "SELECT vl.location from valid_locations vl ",
    "WHERE vl.description like '%corral%' "))
  corral$location[corral$location >= 1]
}

#' Returns a numeric vector with the locations that are classified as single
#' housing structures.

#' @param conn database connection object
#' @export
define_single_locations <- function(conn) {
  group <- sqlQuery(conn, str_c(
    "SELECT vl.location from valid_locations vl ",
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
#' @param new_df dataframe containing the animal and location data
#' @export
get_days_gang <- function(id, new_df) {
  length(new_df$id[new_df$id == id & new_df$gang == 1])
}

#' Returns the number of days a specific animal was in a corral.
#' 
#' @param id animal id
#' @param new_df dataframe containing the animal and location data
#' @export
get_days_corral <- function(id, new_df) {
  length(new_df$id[new_df$id == id & new_df$corral == 1])
}

#' Returns the number of days a specific animal was in a single housing 
#' location.
#' 
#' @param id animal id
#' @param new_df dataframe containing the animal and location data
#' @export
get_days_single <- function(id, new_df) {
  length(new_df$id[new_df$id == id & new_df$single == 1])
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
