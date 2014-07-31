#' Creates ed_hist database table with id and first_noted columns
#' 
#' @param conn database connection object
#' @param X_ed_hist character vector of length one with the name of the table to 
#' create
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
define_gang_locations <- function(conn) {
  location <- sqlQuery(conn, str_c(
  "SELECT vl.location from valid_locations vl ",
  "WHERE vl.description like '%gang%' "))$location
  location[location >= 1]
}

#' Returns a numeric vector with the locations that are classified as corral
#' housing structures.
#' 
#' @param conn database connection object
define_corral_locations <- function(conn) {
  corral <- sqlQuery(conn, str_c(
    "SELECT vl.location from valid_locations vl ",
    "WHERE vl.description like '%corral%' "))
  corral$location[corral$location >= 1]
}

#' Returns a numeric vector with the locations that are classified as single
#' housing structures.

#' @param conn database connection object
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
get_housing_types <- function(conn) {
  list(gang = define_gang_locations(conn),
       corral = define_corral_locations(conn),
       single = define_single_locations(conn))
}

#' Returns the number of days a specific animal was in gang cages.
#' 
#' @param id animal id
#' @param new_df dataframe containing the animal and location data
get_days_gang <- function(id, new_df) {
  length(new_df$id[new_df$id == id & new_df$gang == 1])
}

#' Returns the number of days a specific animal was in a corral.
#' 
#' @param id animal id
#' @param new_df dataframe containing the animal and location data
get_days_corral <- function(id, new_df) {
  length(new_df$id[new_df$id == id & new_df$corral == 1])
}

#' Returns the number of days a specific animal was in a single housing 
#' location.
#' 
#' @param id animal id
#' @param new_df dataframe containing the animal and location data
get_days_single <- function(id, new_df) {
  length(new_df$id[new_df$id == id & new_df$single == 1])
}

