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
get_repro_counts <- function(conn, affected_df, arc_species_code) {
  
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
    if (affected_df$sex[i] == "F") {
      r_sql <- stri_c(
        "select 'Y' from master m
        where exists (select 1 from offspring o
        where o.id = m.id
        and datediff(day, o.offspring_birth_date, '",
        affected_df$first_noted[i], "') <= 183
        and m.id = '", affected_df$id[i], "')")
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
  }
  affected_repro_df <- sqlQuery(conn, "select * from #affected", 
                                stringsAsFactors = FALSE)
  first_noted_dates <- 
    unique(format(affected_df$first_noted[affected_df$sex == "F"], 
                  format = "%Y-%m-%d"))
  ## Since animals can have more than one offspring within 366 days, this
  ## query uses "distinct" on the animal ID.
  for (first_noted in first_noted_dates) {
    status <- sqlQuery(conn, stri_c(
      "INSERT INTO #repro
      (id,
      match_date)
      SELECT distinct o.id, '", first_noted, "'
      from offspring o
      inner join current_data cd on o.id = cd.id
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
  
  affected_new_df <- merge(affected_repro_df, counts_df, 
                           by.x = c("first_noted"),
                           by.y = c("match_date"),
                           all = TRUE)
  affected_new_df <- rbind(affected_new_df, data.frame(
    first_noted = affected_df$first_noted[affected_df$sex == "M"],
    id = affected_df$id[affected_df$sex == "M"],
    repro = rep(NA, length(affected_df$first_noted[affected_df$sex == "M"])),
    repro_count = rep(NA, length(affected_df$first_noted[affected_df$sex == "M"]))))
  affected_new_df <- affected_new_df[order(affected_new_df$id), ]
  
  affected_new_df
  }
