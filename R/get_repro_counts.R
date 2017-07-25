#' Get SQL description to define minimum and maximum ages
#' 
#' Helper function for \code{get_repro_counts()}.
#' @param alias character string representing the table alias (either "m" or 
#' "m2")
#' @param age_range integer vector of length two having the minimum age in 
#' months and the maximum age in months. NA is used to indicate the range is 
#' not bound for that side of the distribution. Thus, 
#' \code{age_range = c(NA, NA)} includes animals of all ages; 
#' \code{age_range = c(48, NA)} includes animals from 48 months and greater;
#' \code{age_range = c(NA, 240)} includes all animals up to 240 months 
#' (20 years); and \code{age_range = c(48, 240)} includes animals from 48
#' months (4 years) up to 240 months (20 years).
#' @param first_noted character string of date diagnosis was first noted.
#' @import stringi
get_min_max_age_str <- function(alias, age_range, first_noted) {
  if (is.na(age_range[1])) {
    min_date_str <- " "
  } else {
    min_date_str <- stri_c(
      "and datediff(day, ", alias, ".birth_date, '",  first_noted, "')
      >= ", round(age_range[1], 0))
  }
  if (is.na(age_range[2])) {
    max_date_str <- " "
  } else {
    max_date_str <- stri_c(
      "and datediff(day, ", alias, ".birth_date, '",  first_noted, "')
      <= ", round(age_range[2], 0))
  }
  stri_c(min_date_str, " ", max_date_str)
}
#' Get reproductive status and species counts.
#' 
#' @return A dataframe with each affected animal, with whether or not the 
#' animal was nursing, pregnant, and the number of nursing or pregnant animals
#' of the same species on that \code{first_noted} date.
#' 
#' Modify this to ensure \code{affected_df$id} has the same species as 
#' \code{arc_species_code}.
#' 
#' @param conn database connection object
#' @param affected_df dataframe with animal Id and date to match on.
#' @param age_range integer vector of length two having the minimum age in 
#' months and the maximum age in days NA is used to indicate the range is 
#' not bound for that side of the distribution. Thus, 
#' \code{age_range = c(NA, NA)} includes animals of all ages; 
#' \code{age_range = c(1461, NA)} includes animals from four years (4 * 365.25)
#'  and greater;
#' \code{age_range = c(NA, 7669)} includes all animals up to 20 years 
#' ((21 * 365.25) - 1); and \code{age_range = c(1461, 7669)} includes animals from 
#' 4 years up tthrough the 20th year.
#' @param arc_species_code character vector with one two character
#' representations of animal species originally developed for IACUC use.
#' 
#' Dataframe returned has 
#' \itemize{
#' \item id of the experimental animal (id),
#' \item first_noted date animal was first diagnosed with histoplasmosis
#' \item repro "Y" if animal was nursing or pregant shortly before the 
#' \code{first_noted} date. This is assessed by whether or not the animal had
#' and offspring within plus or minus 183 days of \code{first_noted} date, 
#' The values are "Y" for not nursing or pregant, "N" if animal was female and 
#' did not have an offspring within plus or minus 183 days of \code{first_noted}
#'  date, and \code{NA} if the animal was male.
#' \item repro_count number of animals of the same species that had an offspring
#' within plus or minus 183 days of \code{first_noted} date
#' }
#' @import RODBC
#' @import stringi
#' @export
get_repro_counts <- function(conn, affected_df, age_range = c(NA, NA), 
                             arc_species_code) {
  #' @description The first temporary table created (\code{#ctrl}) is
  #' used to store the potential control animals found.
  
  status <- sqlQuery(conn, stri_c("DROP TABLE #affected"))
  status <- sqlQuery(conn, stri_c("DROP TABLE #repro"))
  
  status <- sqlQuery(conn, stri_c(
    "CREATE TABLE #affected (
    id char(6),
    repro char(1),
    first_noted DATE)"))
  #' @description The second temporary table created (\code{#repro}) is used to
  #' store the counts of the females that are likely either nursing or
  #' pregnant based on birth of an offspring within 6 months prior to
  #' \code{first_noted} date or 6 months after \code{first_noted} date.
  #'
  #' @import RODBC
  #' @import stringi
  #' @export
  status <- sqlQuery(conn, stri_c(
    "CREATE TABLE #repro(
    id char(6) not null,
    match_date date NOT NULL)"))
  
  for (i in seq_along(affected_df$id)) {
    if (affected_df$sex[i] == "M")
      next
    affected_min_max_age_str <- 
      get_min_max_age_str(alias = "m", age_range, affected_df$first_noted[i])
    r_sql <- stri_c(
        "select 'Y' from master m
        where exists (select 1 from offspring o
        where o.id = m.id
        and datediff(day, o.offspring_birth_date, '",
        affected_df$first_noted[i], "') <= 183
        and m.id = '", affected_df$id[i], "') ",
        affected_min_max_age_str)
      r_status <- sqlQuery(conn, r_sql, stringsAsFactors = FALSE)
      if (nrow(r_status) > 0) {
        repro <- "Y"
      } else {
        repro <- "N"
      }
      status <- sqlQuery(conn, stri_c(
        "INSERT INTO #affected (id, repro, first_noted)
        VALUES ('", affected_df$id[i], "', '", repro,
        "', '", affected_df$first_noted[i], "')"))
  }
  affected_repro_df <- sqlQuery(conn, "select * from #affected", 
                                stringsAsFactors = FALSE)
  first_noted_dates <- 
    unique(format(affected_df$first_noted[affected_df$sex == "F"], 
                  format = "%Y-%m-%d"))
  ## Since animals can have more than one offspring within 366 days, this
  ## query uses "distinct" on the animal ID.
  for (first_noted in first_noted_dates) {
    repro_min_max_age_str <- 
      get_min_max_age_str(alias = "m2", age_range, first_noted)
    status <- sqlQuery(conn, stri_c(
      "INSERT INTO #repro
      (id,
      match_date)
      SELECT distinct o.id, '", first_noted, "'
      from offspring o
      inner join current_data cd on o.id = cd.id
      inner join master m2 on o.id = m2.id ",
      repro_min_max_age_str, "
      where abs(datediff(day, o.offspring_birth_date, '",
      first_noted, "')) <= 183
      and cd.arc_species_code = '", arc_species_code, "'"))
  }
  counts_df <- sqlQuery(conn, stri_c(
    "select match_date, count(1) as repro_count
    from #repro
    group by match_date"))
  
  status <- sqlQuery(conn, stri_c("DROP TABLE #affected"))
  status <- sqlQuery(conn, stri_c("DROP TABLE #repro"))
  repro_total_sql <- stri_c(
    "SELECT dd.target_date, count(dd.target_date) as females
    FROM V_ANIMAL_AGE_SEX_SPECIES AS dd
    WHERE target_date in ('", 
      vector2string(first_noted_dates, SS = "', '"), "')
      AND arc_species_code = '", arc_species_code, "'
      AND sex = 'F'
      AND age_in_days >= ", age_range[1], "
      AND age_in_days <= ", age_range[2], "
    GROUP by target_date, sex 
    ORDER by target_date, sex")
  repro_total_df <- sqlQuery(conn, repro_total_sql, stringsAsFactors = FALSE)
  
  affected_new_df <- merge(affected_repro_df, counts_df, 
                           by.x = "first_noted",
                           by.y = "match_date",
                           all = TRUE)
  affected_new_df <- merge(affected_new_df, repro_total_df,
                           by.x = "first_noted",
                           by.y = "target_date",
                           all = TRUE)
  affected_new_df <- rbind(affected_new_df, data.frame(
    first_noted = affected_df$first_noted[affected_df$sex == "M"],
    id = affected_df$id[affected_df$sex == "M"],
    repro = rep(NA, length(affected_df$first_noted[affected_df$sex == "M"])),
    repro_count = rep(NA, length(affected_df$first_noted[
      affected_df$sex == "M"])),
    females = rep(NA, length(affected_df$first_noted[
      affected_df$sex == "M"]))))
  affected_new_df <- affected_new_df[order(affected_new_df$id), ]
  
  affected_new_df
}
