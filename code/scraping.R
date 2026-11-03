
# Libraries
library(rvest)

# Meetings per league
get_meets <- function(leagues) {

  meet_list <- lapply(leagues, function(league) {

    url <- paste0("https://", league, ".org.au/index.php?id=30")

    links <- html_nodes(read_html(url), "a[href*='meeting=']")

    text <- html_text(links, trim = TRUE)
    text <- gsub("[\r\n]", "", text)
    text <- gsub(" +", " ", text)

    dates <- as.Date(
      sub(".* ([A-z]+) ([0-9]{1,2})[A-z]{2} ([0-9]{4})$", "\\2\\1\\3", text),
      format = "%d%b%Y"
    )

    years <- as.numeric(format(dates, "%Y"))
    years <- as.numeric(sub("^[0-9]{2}", "", years))
    months <- as.numeric(format(dates, "%m"))

    seasons <- ifelse(
      months >= 7,
      paste0(years, "-", years + 1),
      paste0(years - 1, "-", years)
    )

    seasons <- paste0("20", seasons)

    meets <- sub(
      "(.*) - ([A-z]+) ([0-9]{1,2})[A-z]{2} ([0-9]{4})$",
      "\\1",
      text
    )

    id <- sub(
      ".*meeting=([0-9]+).*",
      "\\1",
      html_attr(links, "href")
    )

    data.frame(
      Date = dates,
      Season = seasons,
      League = toupper(league),
      Meeting = meets,
      MeetID = as.integer(id),
      stringsAsFactors = FALSE
    )

  })

  do.call(rbind, meet_list)
}

# Events per meeting
get_events <- function(league, meet_id) {

  stopifnot(length(league) == length(meet_id))

  event_list <- lapply(seq_along(meet_id), function(i) {

    url <- paste0(
      "https://",
      league[i],
      ".org.au/index.php?id=30&meeting=",
      meet_id[i]
    )

    links <- html_nodes(read_html(url), "a[href*='race=']")

    desc <- html_text(links, trim = TRUE)

    types <- sub(
      "^[0-9]+[Mm] (.*)",
      "\\1",
      desc
    )

    events <- sub(
      "^([0-9]+)[Mm] .*",
      "\\1",
      desc
    )

    id <- sub(
      ".*race=([0-9]+).*",
      "\\1",
      html_attr(links, "href")
    )

    data.frame(
      League = toupper(league[i]),
      MeetID = as.integer(meet_id[i]),
      EventID = as.integer(id),
      Event = as.integer(events),
      Type = types,
      TypeDesc = desc,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, event_list)
}

# Times per event
get_times <- function(league, meet_id, event_id) {

  stopifnot(
    length(league) == length(meet_id),
    length(meet_id) == length(event_id)
  )

  df <- do.call(
    rbind,
    mapply(
      function(league, meet_id, event_id) {

        url <- paste0(
          "https://", league, ".org.au/index.php?id=30",
          "&meeting=", meet_id,
          "&race=", event_id
        )

        page <- read_html(url)

        tables <- html_nodes(page, "table")

        info <- do.call(rbind, lapply(seq_along(tables), function(i) {

          tbl <- tables[[i]]

          preceding_text <- html_text(
            html_elements(
              tbl,
              xpath = "preceding-sibling::text()[normalize-space()]"
            ),
            trim = TRUE
          )

          preceding_text <- toupper(preceding_text)

          wind <- preceding_text[grepl("WIND", preceding_text)]
          wind <- if (length(wind) > 0) tail(wind, 1) else NA_character_
          wind <- sub(".*READING: (.*) MPS.*", "\\1", wind)

          round <- preceding_text[grepl("HEAT|SEMI|FINAL", preceding_text)]
          round <- if (length(round) > 0) tail(round, 1) else NA_character_
          round <- sub(".*- (.*): ([0-9]+).*", "\\1 \\2", round)
          round <- sub("SEMI FINAL", "SEMI", round)

          data.frame(
            id = as.character(i),
            League = toupper(league),
            MeetID = meet_id,
            EventID = event_id,
            Round = round,
            Wind = as.numeric(wind)
          )
        }))

        data <- do.call(rbind, lapply(seq_along(tables), function(i) {

          tbl <- html_table(tables[[i]])
          tbl$id <- i
          as.data.frame(tbl)

        }))

        df <- merge(info, data, by = "id", all.x = TRUE)

        df$id <- NULL
        df

      },
      league,
      meet_id,
      event_id,
      SIMPLIFY = FALSE
    )
  )

  rownames(df) <- NULL

  df
}

# Compile times
get_results <- function(meets, events) {

  df <- get_times(
    events$League,
    events$MeetID,
    events$EventID
  )

  df <- merge(
    events,
    df,
    by = c("League", "MeetID", "EventID"),
    sort = FALSE
  )

  df <- merge(
    meets,
    df,
    by = c("League", "MeetID"),
    sort = FALSE
  )

  rownames(df) <- NULL

  df <- df[!is.na(df$Time), ]

  df[order(df$MeetID, df$EventID, df$Round, df$Position), ]
}

# Tidy results
tidy_results <- function(results) {

  stopifnot(
    names(results) == c(
      "League", "MeetID", "Date", "Season", "Meeting",
      "EventID", "Event", "Type", "TypeDesc", "Round",
      "Wind", "Position", "Name", "Colour", "Mark",
      "Time", "Adj Time (wind)", "Adj Time (wind + track)",
      "MPS (wind + track)"
    )
  )

  names(results) <- c(
    "League", "MeetID", "Date", "Season", "Meeting",
    "EventID", "Event", "Type", "TypeDesc", "Round",
    "Wind", "Position", "Name", "Colour", "Mark",
    "Time", "AdjTime", "AdjTimeWT", "MPSWT"
  )

  cols <- c(
    "Date", "Season", "League", "MeetID", "Meeting",
    "EventID", "Event", "Type", "TypeDesc", "Round",
    "Wind", "Position", "Name", "Mark", "Time", "AdjTime"
  )

  results <- results[, cols]
  results <- results[(results$Position != "-"), ]
  results <- results[(results$Time != "Scratched"), ]

  results$Time <- ifelse(
    grepl(":", results$Time),
    60 * as.numeric(sub(":.*", "", results$Time)) +
      as.numeric(sub(".*:", "", results$Time)),
    suppressWarnings(as.numeric(results$Time))
  )

  results$AdjTime <- ifelse(
    grepl(":", results$AdjTime),
    60 * as.numeric(sub(":.*", "", results$AdjTime)) +
      as.numeric(sub(".*:", "", results$AdjTime)),
    suppressWarnings(as.numeric(results$AdjTime))
  )

  results <- results[results$Time > 0, ]
  results <- results[results$AdjTime > 0, ]
  results$Time <- round(results$Time, 3)
  results$AdjTime <- round(results$AdjTime, 3)

  results$Mark <- as.numeric(sub("[Mm]", "", results$Mark))
  results$Position <- as.integer(results$Position)
  results$Distance <- results$Event - results$Mark
  results$MPS <- round(results$Distance / results$Time, 3)
  results$AdjMPS <- round(results$Distance / results$AdjTime, 3)

  results$Name <- gsub("^ +| +$", "", results$Name)
  results$Name <- gsub(" +", " ", results$Name)
  results$Name <- toupper(results$Name)

  results$Meeting <- toupper(results$Meeting)
  results$Type <- toupper(results$Type)
  results$TypeDesc <- toupper(results$TypeDesc)

  rownames(results) <- NULL

  results
}
