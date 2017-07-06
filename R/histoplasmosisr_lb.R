#' Returns dataframe with day_of_year and month added given a first_noted.
#' 
#' @param df dataframe with first_noted
add_day_of_year_and_month <- function(df) {
  df$day_of_year <- as.integer(strftime(df$first_noted, format = "%j"))
  df$month <- as.integer(strftime(df$first_noted, format = "%m"))
  df
}

#' Returns dataframe with id, verified_date_tm, specimen_num, test_id,
#' test_name, observed_value, and entry_date_tm for records indication positive
#' for \emph{H. CAP. VAR. DUBOISII}
#' @param conn database connection
#' @param arc_specoes_code to limit the query to a single species.
#' @export
get_sqlmed_h_cap_var_duboisii <- function(conn, arc_species_code) {
  df <- sqlQuery(conn, str_c(
    "select obr.animal_id as id, obr.verified_date_tm, obr.specimen_num,
    obx.test_id, obx.test_name, obx.observed_value, obx.entry_date_tm
    from clinical_path_obr obr
    inner join clinical_path_obx obx on obr.message_id = obx.message_id
    inner join current_data cd on obr.animal_id = cd.id 
    where obx.observed_value like '%DUB%'
      and obx.observed_value not like '%NEGATIVE FOR H. CAP.%'
      and obx.observed_value not like '%NO H. CAP. VAR. DUBOISII%'
      and obx.observed_value not like '%neg. for H.cap var duboisii%'
      and cd.arc_species_code = '", arc_species_code, "' "), 
    stringsAsFactors = FALSE)
  df  
} 

#' Returns dataframe with Ids, the date, the day of the year, 
#' and the month of the year (as an integer)
#' each animal was first noted to have histoplasmosis.
#' 
#' @param conn database connection object
#' @param histo_data name and path of the Excel file containing animal Ids
#' @param arc_species_code two characters in a single character string
#' species code 
#' and the date each animal was noted to have histoplasmosis.
#' @export
get_affected_animals_df <- function(conn, histo_data, arc_species_code) {
  affected_df <- readWorksheet(histo_data, sheet = 1)
  names(affected_df) <-  c("id", "first_noted")
  affected_df$arc_species <- arc_species_code
  
  affected_df$id <- blank_fill_ids(affected_df$id)
  affected_df <- add_day_of_year_and_month(affected_df)
  affected_df <- add_birth_date(conn, affected_df)
  affected_df$birth_date <- as.POSIXct(strftime(affected_df$birth_date, 
                                                format = "%Y-%m-%d"), 
                                       format = "%Y-%m-%d")
  affected_df$days_alive <- 
    as.integer(1L + round((affected_df$first_noted - affected_df$birth_date) / 
                            ddays(1), 0))
  affected_df <- add_sex(conn, affected_df) 
  affected_df$sex <- as.character(affected_df$sex)
  affected_df$age <- 
    (affected_df$first_noted - affected_df$birth_date) / dyears(1)
  affected_df
}

#' Returns dataframe for control animals with id, first_noted, day_of_year, 
#' and month so that it has the same structure as df.
#' 
#' @param conn database connection object
#' @param affected_df 
#' @param arc_species_codes character vector with one or more two character
#' representations of animal species originally developed for IACUC use.
#' @return Dataframe with the following columns:
#' \itemize{
#' \item {id of the experimental animal (id)}
#' \item {id of the control animal having the minimal difference in age 
#' (min_match_id)}
#' \item {age in days of the experimental animal (age_in_days)}
#' \item {age in days of the control animal (match_age)}
#' \item {number of days different}
#' }
#' @export
get_ctrl_df <- function(conn, exp_df, arc_species_code) {
  ctrl_df <- get_age_sex_matched_controls(conn, exp_df, arc_species_code)
  
  ctrl_df <- merge(exp_df[ , c("id", "first_noted", "sex")], 
                   ctrl_df[ , c("id", "min_match_id")], by = "id")
  ctrl_df <- data.frame(id = ctrl_df$min_match_id, 
                        first_noted = ctrl_df$first_noted,
                        arc_species = arc_species_code)
  ctrl_df <- add_day_of_year_and_month(ctrl_df)
  ctrl_df <- add_birth_date(conn, ctrl_df)
  ctrl_df$birth_date <- as.POSIXct(strftime(ctrl_df$birth_date, format = "%Y-%m-%d"), 
                                   format = "%Y-%m-%d")
  ctrl_df$days_alive <- as.integer(round(1L + (ctrl_df$first_noted -
                                           ctrl_df$birth_date) / ddays(1), 0))
  ctrl_df <- add_sex(conn, ctrl_df)
  ctrl_df$sex <- as.character(ctrl_df$sex)
  ctrl_df$age <- (ctrl_df$first_noted - ctrl_df$birth_date) / dyears(1)
  
  ctrl_df
}

#' Returns dataframe with Ids of animals that were in a corral within 10 
#' days of being first seen with histoplasmosis.
#' 
#' @param wt_conn database connection object
#' @param X_id_first_noted name of database table with Ids and dates Histoplasmosis
#' @export
get_roundup_animals <- function(wt_conn, X_id_first_noted) {
  sql <- str_c(
    "SELECT x.id FROM ", X_id_first_noted, " x
    WHERE x.id in (select l.id from animal.dbo.location l
      WHERE l.move_date_tm < dateadd(day, -10, x.first_noted)
        AND l.exit_date_tm > dateadd(day, -10, x.first_noted)
        AND cast(l.exit_date_tm as DATE) <= x.first_noted 
        AND l.location in (", 
        vector2string(define_corral_locations(conn), SS = ", "), ") )")
  not_roundup_df <- sqlQuery(wt_conn, sql, stringsAsFactors = FALSE)
  not_roundup_df
}

#' Creates ed_hist database table with id and first_noted columns
#' 
#' @param conn database connection object
#' @param X_id_first_noted character vector of length one with the name of the 
#' table to create
#' @export
create_X_id_first_noted <- function(conn, X_id_first_noted) {
  sqlQuery(conn, str_c(
    "CREATE TABLE ", X_id_first_noted, " ( 
      id CHAR(6),
      first_noted DATE) "))
}              

#' Inserts ids and dates that correspond to when the Histoplasmosis infection
#' was first noted.
#' 
#' @param wt_conn database connection object
#' @param X_id_first_noted name of database table receiving inserts
#' @param df dataframe containing data being inserted.
#' @export
insert_id_first_noted <- function(wt_conn, X_id_first_noted, df) {
    for (i in seq_along(df$id)) {
      status <- sqlQuery(wt_conn, str_c(
        "insert into ", X_id_first_noted, " 
        (id, first_noted) 
        values ('", df$id[i], "', '", 
        strftime(df$first_noted[i], format = "%m/%d/%Y"), "')"))
    }
  }

#' Creates a dataframe with a row for each day of life prior to the 
#' first_noted date for each animal in the table with the name
#' contained in X_id_first_noted.
#' 
#' @param conn database connection object
#' @param X_id_first_noted name of database table with Ids and dates Histoplasmosis
#' was first noted.
#' @export
make_daily_df <- function(wt_conn, X_id_first_noted, df, housing_types) {
  drop_status <- sqlQuery(wt_conn, str_c("drop table ", X_id_first_noted))
  create_X_id_first_noted(wt_conn, X_id_first_noted)
  insert_id_first_noted(wt_conn, X_id_first_noted, df)
  daily_df <- sqlQuery(wt_conn, str_c(
    "SELECT dd.target_date , ",
    "dd.midnight_location as location, ",
    "dd.id ",
    "FROM daily_demo dd ",
    "INNER JOIN ", X_id_first_noted, " h ",
    "ON h.id = dd.id ",
    "INNER JOIN animal.dbo.master m ",
    "ON m.id = dd.id ",
    "WHERE dd.target_date BETWEEN cast(m.birth_date as date) 
      AND h.first_noted") , stringsAsFactors = FALSE)

  daily_df$gang <- ifelse(daily_df$location %in% housing_types$gang, 1, 0)
  daily_df$corral <- ifelse(daily_df$location %in% housing_types$corral, 1, 0)
  daily_df$single <- ifelse(daily_df$location %in% housing_types$single, 1, 0)
  daily_df$other <- ifelse(daily_df$location %in% housing_types$other, 1, 0)
  daily_df  
}

#' Returns a dataframe with age and sex matched controls corresponding to 
#' animals within affected_df.
#' 
#' Modify this to ensure \code{affected_df$id} has the same species as 
#' \code{arc_species_code}.
#' 
#' @param conn database connection object
#' @param affected_df dataframe with animal Id and date to match on.
#' @param arc_species_codes character vector with one or more two character
#' representations of animal species originally developed for IACUC use.
#' 
#' Dataframe returned has 
#' id of the experimental animal (id),
#' the id of the control animal having the minimal difference in age 
#' (min_match_id), 
#' the age in days of the experimental animal (age_in_days),
#' the age in days of the control animal (match_age), 
#' and the number of days different
#' @import stringr
#' @export
get_age_sex_matched_controls <- function(conn, affected_df, arc_species_code) {
#' @description The first temporary table created (\code{#ctrl}) is 
#' used to store the potential control animals found.

  status <- sqlQuery(conn, str_c(
    "CREATE TABLE #ctrl (
      id char(6), sex char(1), first_noted DATE, arc_species CHAR(2), 
        age_in_days INT)"))
#' @description The second temporary table created (\code{#result}) is used to
#' store the selected control animals found. 
#' 
#' Control animals are defined as age and sex matched. Thus, a control animal 
#' is selected for each animal in \code{affected_df} that has the same species
#' and sex and 
#' is as close to the same age as possible. Care is taken to insure that 
#' the same control animal is not selected more than once.
#' 
  status <- sqlQuery(conn, str_c(
    "CREATE TABLE #result(
       [match_id] [varchar](6) NOT NULL,
       [match_sex] [char](1) NOT NULL,
       [match_date] [date] NOT NULL,
       [match_age] [int] NULL,
       [match_species] [char](2) NULL,
       [id] [char](6) NULL,
       [first_noted] [date] NULL,
       [age_in_days] [int] NULL,
       [day_diff] [int] NULL)"))
  
  for (i in seq_along(affected_df$id)) {
    (status <- sqlQuery(conn, str_c(
      "INSERT INTO #ctrl (id, sex, first_noted, arc_species, age_in_days)
      VALUES ('", affected_df$id[i], "', '", affected_df$sex[i], 
      "', '", affected_df$first_noted[i], "', '", arc_species_code, "', ",
      affected_df$days_alive[i], ")")))
  }
  # sqlQuery(conn, str_c(
  #   "UPDATE #ctrl
  #   SET age_in_days = d.age_in_days
  #   FROM #ctrl c
  #   INNER JOIN dbo.v_animal_age d
  #   ON c.id = d.id AND c.first_noted = d.target_date"))
  #   
    
  status <- sqlQuery(conn, str_c(
      "INSERT INTO #result
    ( match_id ,
      match_sex ,
      match_date ,
      match_age ,
      match_species ,
      id ,
      first_noted ,
      age_in_days ,
      day_diff
    )
    SELECT d.id AS match_id, d.sex match_sex, d.target_date AS match_date, 
      d.age_in_days AS match_age, d.arc_species_code AS match_species,
      c.id, c.first_noted, c.age_in_days,
      MIN(ABS(DATEDIFF(DAY, c.first_noted, d.target_date))) AS day_diff
    FROM dbo.v_animal_age_sex_species d
    INNER JOIN #ctrl c ON d.sex = c.sex ",
      ##AND d.age_in_days = c.age_in_days # does not use index if here
      ## Do not want a control animal to be any affected animal
      "AND NOT EXISTS (SELECT 1 FROM #ctrl c2 WHERE d.id = c2.id)
      AND d.arc_species_code = '", arc_species_code, "' ", 
      ## AND c.arc_species = d.arc_species_code
    " WHERE c.age_in_days = d.age_in_days
        AND ABS(DATEDIFF(DAY, c.first_noted, d.target_date)) < 1000   
    GROUP BY d.id, d.sex, d.target_date, d.age_in_days, d.arc_species_code, 
      c.id, c.first_noted,c.age_in_days"))
    
#     sqlQuery(conn, str_c(
#       "INSERT INTO #ctrl (id, sex, first_noted)
#       VALUES ('", affected_df$id[i], "', '", affected_df$sex[i], 
#       "', '", affected_df$first_noted[i], "')"))
  
  ctrl_df <- sqlQuery(conn, str_c(
    "SELECT r.id, MIN(r.match_id) AS min_match_id, 
      r.match_species as arc_species, 
      r.age_in_days, r.match_age,  r.day_diff
    FROM #result r
    INNER JOIN #result r2
    ON r.id = r2.id
    WHERE r.day_diff < r2.day_diff
    AND r.day_diff = (SELECT MIN(z.day_diff) FROM #result z WHERE z.id = r.id )
    AND r.match_id > r2.match_id
    GROUP BY r.id,  r.age_in_days, r.match_age, r.day_diff"))

#' @description Once the result set has been created the temporary tables 
#' (\code{#ctrl} and \code{#result}) are deleted.

  status <- sqlQuery(conn, str_c("DROP TABLE #ctrl"))
  status <- sqlQuery(conn, str_c("DROP TABLE #result"))

  ctrl_df
}

#' Returns dataframw with the days_alive, days_gang, percent_gang, days_corral,
#' percent_corral, days_single, and percent_single columns defined.
#' 
#' @param df dataframe being used with one record per animal that will take 
#' sums and percents.
#' @param daily_df dataframe that has one record per day for each id from birth
#' to first_noted date.
#' @export
add_location_type_percents <- function(df, daily_df, 
                                       threshold_min_percent = 100) {
  df$days_gang <- sapply(df$id, FUN = function(id) {get_days_gang(id, daily_df)})
  df$percent_gang <- (df$days_gang / df$days_alive) * 100
  df$days_corral <- 
    sapply(df$id, FUN = function(id) {get_days_corral(id, daily_df)})
  df$percent_corral <- (df$days_corral / df$days_alive) * 100
  df$days_single <- 
    sapply(df$id, FUN = function(id) {get_days_single(id, daily_df)})
  df$percent_single <- (df$days_single / df$days_alive) * 100
  df$days_other <- 
    sapply(df$id, FUN = function(id) {get_days_other(id, daily_df)})
  df$percent_other <- (df$days_other / df$days_alive) * 100
  df$total_percent <- 
    df$percent_gang + df$percent_single + df$percent_corral + df$percent_other
  df[df$total_percent >= threshold_min_percent,]
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

#' Returns a numeric vector with the locations that are classified as other
#' housing structures.

#' @param daily_df dataframe with individual daily information for subjects
#' @export
define_other_locations <- function(conn) {
  other <- sqlQuery(conn, str_c(
  "select vl.location from valid_locations vl ",
  "where vl.location not in ( '", 
    vector2string(define_corral_locations(conn)), "') ",
  "and vl.location not in ( '", 
    vector2string(define_gang_locations(conn)), "') ",
  "and vl.location not in ( '", 
    vector2string(define_single_locations(conn)), "')"))
  other$location[other$location >= 1]
}

#' Returns a list of length 3 with a vector of gang cage locations, a vector
#' of corral locations, and a vector of single locations.
#' 
#' @param conn database connection object
#' @export
get_housing_types <- function(conn) {
  list(gang = define_gang_locations(conn),
       corral = define_corral_locations(conn),
       single = define_single_locations(conn),
       other = define_other_locations(conn))
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

#' Returns the number of days a specific animal was in an other housing 
#' location.
#' 
#' @param id animal id
#' @param daily_df dataframe containing the animal and location data
#' @export
get_days_other <- function(id, daily_df) {
  length(daily_df$id[daily_df$id == id & daily_df$other == 1])
}

get_daily_location_ct <- function(conn, wt_conn, daily_df) {
  #unique_locations <- unique(daily_df$location)
  gang_loc <- vector2string(define_gang_locations(conn))
  corral_loc <- vector2string(define_corral_locations(conn))
  single_loc <- vector2string(define_single_locations(conn))
  other_loc <- vector2string(define_other_locations(conn))
  unique_dates <- unique(daily_df$target_date)
  daily_gang_ct <- sqlQuery(wt_conn, str_c(
    "select count(d.id) as cnt, d.target_date, 'gang' as housing  ",
    "from daily_demo d ",
    "where d.midnight_location in ('", gang_loc, "') ",
    "and d.target_date in ('", vector2string(unique_dates), "') ",
    "group by  d.target_date "))
  daily_corral_ct <- sqlQuery(wt_conn, str_c(
    "select count(d.id) as cnt, d.target_date, 'corral' as housing  ",
    "from daily_demo d ",
    "where d.midnight_location in ('", corral_loc, "') ",
    "and d.target_date in ('", vector2string(unique_dates), "') ",
    "group by  d.target_date "))
  daily_single_ct <- sqlQuery(wt_conn, str_c(
    "select count(d.id) as cnt, d.target_date, 'single' as housing  ",
    "from daily_demo d ",
    "where d.midnight_location in ('", single_loc, "') ",
    "and d.target_date in ('", vector2string(unique_dates), "') ",
    "group by  d.target_date "))
  daily_other_ct <- sqlQuery(wt_conn, str_c(
    "select count(d.id) as cnt, d.target_date, 'other' as housing  ",
    "from daily_demo d ",
    "where d.midnight_location in ('", other_loc, "') ",
    "and d.target_date in ('", vector2string(unique_dates), "') ",
    "group by  d.target_date "))
  daily_location_ct <- rbind(daily_gang_ct, daily_corral_ct, 
                             daily_single_ct, daily_other_ct)
 
  daily_location_ct <- daily_location_ct[order(daily_location_ct$target_date, 
                                               daily_location_ct$housing),]
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
  sql <- str_c(
      "SELECT dd.target_date, sex, count(dd.target_date)
      FROM daily_demo AS dd
      WHERE target_date in ('", 
        vector2string(strftime(target_date_df$target_date, format = "%m/%d/%Y"), 
                     SS = "', '"), "')
        AND arc_species = '", arc_species_code, "'
      GROUP by target_date, sex 
      ORDER by target_date, sex")
  
  m_f_ratio_df <- sqlQuery(conn, sql, stringsAsFactors = FALSE)
  m_f_ratio_df <- merge(m_f_ratio_df[m_f_ratio_df$sex == "M", ],
                        m_f_ratio_df[m_f_ratio_df$sex == "F", ],
                        by = "target_date")
  m_f_ratio_df$sex.x <- NULL
  m_f_ratio_df$sex.y <- NULL
  names(m_f_ratio_df) <- c("target_date", "males", "females")
  m_f_ratio_df$target_date <- 
    as.POSIXct(m_f_ratio_df$target_date)
  df <- merge(affected_df, m_f_ratio_df, by.x = "first_noted", 
              by.y = "target_date")
  ## Calculate the probability of a male being selected at random on the
  ## target_date
  within(df, {male_prob <- males / (males + females)})
}
#' Returns a dataframe with the Id, date, and the number of gang, corral, 
#' single, and other housing type animals of the a specified species on that 
#' target date.
#' 
#' @param conn database connection object
#' @param affected_df dataframe containing data about affected animals (e.g.,
#' id, sex, date Histoplasmosis was first noted, etc.)
#' @param target_date_df dataframe with the dates (target_date)
#' @param arc_species_code of all animals being counted.
#' @export
get_housing_type_ratios <- function(conn, affected_df, housing_types,
                                    arc_species_code) {
  target_date_df <- data.frame(target_date = unique(affected_df$first_noted))
  sqlQuery(conn, "CREATE TABLE #ht_temp (location numeric(6, 2), ht varchar(6))")
  for (i in seq_along(housing_types)) {
    name <- names(housing_types)[i]
    for (location in housing_types[[i]]) {
      sqlQuery(conn, str_c(
        "INSERT INTO #ht_temp (location, ht) values (", location, 
        ", '", name, "')"))
    }
  }
  ht_df <- sqlQuery(conn, str_c(
    "SELECT dd.target_date, #ht_temp.ht, count(#ht_temp.ht) as count
    FROM daily_demo dd
    INNER JOIN #ht_temp ON #ht_temp.location = dd.midnight_location
    WHERE target_date in ('", 
    vector2string(strftime(target_date_df$target_date, format = "%m/%d/%Y"), 
                  SS = "', '"), "')
        AND dd.arc_species = '", arc_species_code, "'
    GROUP BY dd.target_date, #ht_temp.ht
    ORDER BY dd.target_date, #ht_temp.ht"), stringsAsFactors = FALSE)
  
  sqlQuery(conn, "DROP TABLE #ht_temp")
  corral <- ht_df[ht_df$ht == "corral", ]
  corral$ht <- NULL
  names(corral) <- c("target_date", "corral")
  gang <- ht_df[ht_df$ht == "gang", ]
  gang$ht <- NULL
  names(gang) <- c("target_date", "gang")
  single <- ht_df[ht_df$ht == "single", ]
  single$ht <- NULL
  names(single) <- c("target_date", "single")
  other <- ht_df[ht_df$ht == "other", ]
  other$ht <- NULL
  names(other) <- c("target_date", "other")
  ht_df_1 <- merge(corral, gang, by = "target_date")
  ht_df_2 <- merge(single, other, by = "target_date")
  ht_df_3 <- merge(ht_df_1, ht_df_2, by = "target_date")
  ht_df_3$target_date <- as.POSIXct(ht_df_3$target_date, 
                                    format = "%Y-%m-%d")
  ht_df_4 <- merge(affected_df, ht_df_3,
                 by.x = "first_noted", by.y = "target_date")
  ## Calculate the probability of an animal in a 
  ##    gang cage being selected at random on the target_date,
  within(ht_df_4, {
    p_gang <- gang / (gang + corral + single + other)
    p_corral <- corral / (gang + corral + single + other)
    p_single <- single / (gang + corral + single + other)})
}


#' Returns relative risk for a cohort study. 
#' 
#' Code adapted from 
#' http://a-little-book-of-r-for-biomedical-statistics.readthedocs.org/en/latest/src/biomedicalstats.html
#' @param mymatrix 2 by 2 matrix
#' @param alpha Type 1 error rate
#' @param reference_row (unexposed or control group)
#' @export
calc_relative_risk <- function(mymatrix, alpha=0.05, reference_row=2) {
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  relative_risk <- numeric(numrow)
  lowervalue <- numeric(numrow)
  uppervalue <- numeric(numrow)
  for (i in 1:numrow) {
    rowname <- myrownames[i]
    disease_unexposed <- mymatrix[reference_row, 1]
    control_unexposed <- mymatrix[reference_row, 2]
    if (i != reference_row) {
      disease_exposed <- mymatrix[i, 1]
      control_exposed <- mymatrix[i, 2]
      tot_exposed <- disease_exposed + control_exposed
      tot_unexposed <- disease_unexposed + control_unexposed
      prob_disease_given_exposed <- disease_exposed / tot_exposed
      prob_disease_given_unexposed <- disease_unexposed / tot_unexposed
      
      # calculate the relative risk
      relative_risk[i] <- prob_disease_given_exposed /
        prob_disease_given_unexposed
      #print(paste("category =", rowname, ", relative risk = ",relative_risk))
      
      # calculate a confidence interval
      confidence_level <- (1 - alpha) * 100
      sigma <- sqrt((1 / disease_exposed) - (1 / tot_exposed) +
                      (1 / disease_unexposed) - (1 / tot_unexposed))
      # sigma is the standard error of estimate of log of relative risk
      z <- qnorm(1 - (alpha / 2))
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

#' Returns a numeric vector with the number of samples smaller than the observed
#' statistic, the number of samples equal to the observed statistic, and the
#' number of samples greater than the observed statistic.
#' 
#' @param x Either a vector of one or more elements from which to choose, 
#' or a positive integer. See `Details.'
#' @param ntrials number of trials to use. 
#' @param probs is a numeric vector with one cell per affected individual. 
#' It has the probability of a prespecified event occuring. 
#' (The calling environment establishes what that is, but only needs to 
#' give this routine the probability
#' of it occuring for each potential event.)
#' @param obs_stat observed statistic is a numeric value equal to the statistic
#' calculated for the observed data.
#' @param stat_f statistical function to use to calculate the statistic
#' @export
get_mce <- function(x, ntrials = 10, probs, obs_stat, 
                    stat_f = function(samp) mean(samp)) {
  my_matrix <- matrix(numeric(ntrials * length(probs)), 
                             nrow = length(probs))

  for (i in seq_along(probs)) {
    prob <- probs[i]
    my_matrix[i, ] <- sample(x, size = ntrials, replace = TRUE, 
                             prob = c(prob, 1 - prob))
  }
  results <- sapply(1:ntrials, FUN = function(j) {
    stat_f(my_matrix[ , j])})
  list(lt = length(results[results < obs_stat]),
    eq = length(results[results == obs_stat]),
    gt = length(results[results > obs_stat]))
}

#' Returns the function to be used for the Monte Carlo estimates of p.
#' 
#' @param variable name of the variable being counted.
#' @export
get_stat_f <- function(variable) {
  if (variable == "sex") {
    stat_f <- function(samp) {
      length(samp[samp == "M"]) / length(samp)
    }
  } else if (variable == "housing_type_corral") {
    stat_f <- function(samp) {
      length(samp[samp == "corral"]) / length(samp)
    }
  } else if (variable == "housing_type_gang") {
    stat_f <- function(samp) {
      length(samp[samp == "gang"]) / length(samp)
    }
  } else if (variable == "housing_type_single") {
    stat_f <- function(samp) {
      length(samp[samp == "single"]) / length(samp)
    }
  } 
  stat_f
}
