###############################################################################
###############################################################################

#' Scrape Batted Ball Distance/Velocity Data from Baseball Savant / MLB Statcast
#'
#' This function allows you to scrape all leaderboard statistics from the Baseball Savant batted ball data leaderboard
#' @param bat_pitch either 'bat' or 'pit'
#' @param year YYYY number format
#' @param from.date YYYY-MM-DD format
#' @param qual Number of ABs that meets the qualification
#' @return data frame
#' @examples
#' statcast<-statcast_leaderboard('bat', 2016, "2016-04-10", 20)
#' @export
###############################################################################


statcast_leaderboard <-
  function(bat_pitch = 'pit',year = NULL, from.date = NULL, qual = 20) {
    library(rvest)
    library(stringr)
    library(sqldf)
    
    options(warn = -1)
    
    if (bat_pitch == 'bat') {
      bat_pitch <- 'batter'
    } else if (bat_pitch == 'pit') {
      bat_pitch <- 'pitcher'
    }
    
    # default to current year unless specified
    year <- ifelse(is.null(year),
                   ifelse(
                     as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
                       1,as.numeric(format(Sys.time(), "%Y"))
                   )
                   ,year)
    
    # default to pull since APR 1st unless specified
    from_date <-
      ifelse(is.null(from.date),paste0(year,"-","04-01"),from.date)
    
    # Scrap AVG distance
    base_url <-
      paste0(
        "https://baseballsavant.mlb.com/statcast_search?hfPT=&hfZ=&hfGT=R%7C&hfPR=&hfAB=&stadium=&hfBBT=&hfBBL=&hfC=&season=",
        "&player_type=",
        bat_pitch,
        "&hfOuts=&pitcher_throws=&batter_stands=&start_speed_gt=&start_speed_lt=&perceived_speed_gt=&perceived_speed_lt=&spin_rate_gt=&spin_rate_lt=&exit_velocity_gt=&exit_velocity_lt=&launch_angle_gt=&launch_angle_lt=&distance_gt=&distance_lt=&batted_ball_angle_gt=&batted_ball_angle_lt=&game_date_gt=",
        from_date,
        "&game_date_lt=&team=&position=&hfRO=&home_road=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=bbdist&sort_order=desc&min_abs=",
        qual,
        "&px1=&px2=&pz1=&pz2=#results"
      )
    
    df <- read_html(base_url)
    df <-
      df %>% html_nodes(xpath = '//*[@id="search_results"]') %>%  html_table(fill = TRUE)
    
    # guess_encoding(df)
    avg_dist <- as.data.frame(df)
    avg_dist <- avg_dist[,c(1:4)]
    
    names(avg_dist) <-
      c("Rank", "Name", "Events", "Avg_Distance")
    
    avg_dist <- avg_dist[complete.cases(avg_dist),]
    avg_dist$Avg_Distance <-
      substr(avg_dist$Avg_Distance,1,nchar(avg_dist$Avg_Distance) - 3)
    
    # Char to integer
    for (i in c(3:ncol(avg_dist))) {
      avg_dist[,i] <-
        as.integer(as.character(avg_dist[,i]))
    }
    
    #Replacing Names that match Fangraphs
    avg_dist$Name <-
      str_replace(avg_dist$Name,"Byung Ho Park","Byung-ho Park")
    
    avg_dist$Name <-
      str_replace(avg_dist$Name,"Norichika Aoki","Nori Aoki")
    
    # Scrap AVG Exit Velocity
    base_url <-
      paste0(
        "https://baseballsavant.mlb.com/statcast_search?hfPT=&hfZ=&hfGT=R%7C&hfPR=&hfAB=&stadium=&hfBBT=&hfBBL=&hfC=&season=",
        "&player_type=",
        bat_pitch,
        "&hfOuts=&pitcher_throws=&batter_stands=&start_speed_gt=&start_speed_lt=&perceived_speed_gt=&perceived_speed_lt=&spin_rate_gt=&spin_rate_lt=&exit_velocity_gt=&exit_velocity_lt=&launch_angle_gt=&launch_angle_lt=&distance_gt=&distance_lt=&batted_ball_angle_gt=&batted_ball_angle_lt=&game_date_gt=",
        from_date,
        "&game_date_lt=&team=&position=&hfRO=&home_road=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=exit_velocity&sort_order=desc&min_abs=",
        qual,
        "&px1=&px2=&pz1=&pz2=#results"
      )
    
    df <- read_html(base_url)
    df <-
      df %>% html_nodes(xpath = '//*[@id="search_results"]') %>%  html_table(fill = TRUE)
    
    # guess_encoding(df)
    avg_exit <- as.data.frame(df)
    avg_exit <- avg_exit[,c(1:4)]
    
    names(avg_exit) <-
      c("Rank", "Name", "Events", "Avg_Exit_Velocity")
    
    avg_exit <- subset(avg_exit, select = c("Rank", "Name", "Events", "Avg_Exit_Velocity"))
    
    avg_exit <- avg_exit[complete.cases(avg_exit),]
    avg_exit$Avg_Exit_Velocity <-
      substr(avg_exit$Avg_Exit_Velocity,1,nchar(avg_exit$Avg_Exit_Velocity) -
               4)
    
    # Char to numeric
    for (i in c(3:ncol(avg_exit))) {
      avg_exit[,i] <-
        as.numeric(as.character(avg_exit[,i]))
    }
    
    #Replacing Names that match Fangraphs
    avg_exit$Name <-
      str_replace(avg_exit$Name,"Byung Ho Park","Byung-ho Park")
    
    avg_exit$Name <-
      str_replace(avg_exit$Name,"Norichika Aoki","Nori Aoki")
    
    #combine distance and exit velocity
    df <-
      sqldf(
        "select avg_dist.Name, avg_dist.Events, avg_dist.Avg_Distance, avg_exit.Avg_Exit_Velocity
        from avg_dist
        join avg_exit on lower(avg_exit.Name) = lower(avg_dist.Name)
        where 1=1
        order by avg_exit.Avg_Exit_Velocity desc"
      )
    
    return(df)
    
  }


###############################################################################
###############################################################################
#' Scrape Batter Leaderboards from FanGraphs.com
#'
#' This function allows you to scrape all leaderboard statistics from FanGraphs.com.
#' @param bat_pitch either 'bat' or 'pit'
#' @param yearfrom First season for which you want data.
#' @param yearto Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If yearto = yearfrom, function will return single-season data.
#' @param qual Whether you want only batters that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimumm number of plate appearaces, use the number desired.
#' @param split '0' - full season '1' - 7 days '2' - 15 days '3' - 30 days '30' - 1st half '31' - 2nd half '15' - Home '16' - Away '13' - vs LHP or LHB '14' - vs RHP or RHB
#' @return data frame
#' @examples
#' fgh<-fangraphs_leaderboard('pit',2016,2016, 60, 0)
#' @export
###############################################################################

fangraphs_leaderboard <-
  function(bat_pitch = 'pit', yearfrom = NULL, yearto = NULL, qual = NULL, split = NULL) {
    library(data.table)
    library(XML)
    library(stringr)
    
    options(warn = -1)
    
    qual <- ifelse(is.null(qual),"y",qual)
    split <- ifelse(is.null(split),0,split)
    yearfrom <- ifelse(
      is.null(yearfrom),
      ifelse(
        as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
          1,as.numeric(format(Sys.time(), "%Y"))
      )
      ,yearfrom
    )
    yearto <- ifelse(is.null(yearto),
                     ifelse(
                       as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
                         1,as.numeric(format(Sys.time(), "%Y"))
                     )
                     ,yearto)
    
    if (bat_pitch == 'bat') {
      end_params <- '&players=0&sort=4,d'
      type <-
        'c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211'
    } else if (bat_pitch == 'pit') {
      end_params <- '&players=0&sort=9,d'
      type <-
        'c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224'
    }
    
    base_url <-
      paste0(
        "http://www.fangraphs.com/leaders.aspx?pos=all&stats=",
        bat_pitch,"&lg=all&qual=",
        qual,"&type=",
        type,"&season=",
        yearto, "&month=",
        split,"&season1=",
        yearfrom, "&ind=0&team="
      )
    teams <- unlist(strsplit(as.character(1:30),','))
    urls <- paste0(base_url, rep(teams, each = 1),
                   "&rost=1&age=0&filter=",end_params)
    # Scrape
    leaders <-
      lapply(urls, function(x) {
        data.table(
          readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
        )
      })
    
    # Combine Scrapes
    fangraphs_leaders <- c()
    for (i in 1:30) {
      fangraphs_leaders <-
        as.data.frame(rbind(fangraphs_leaders,leaders[[i]]))
    }
    
    # Rename columns
    c <- as.matrix(names(fangraphs_leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub("+", "plus", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- gsub("-", "_", c, fixed = TRUE)
    c <- gsub("1b", "x1b", c, fixed = TRUE)
    c <- gsub("2b", "x2b", c, fixed = TRUE)
    c <- gsub("3b", "x3b", c, fixed = TRUE)
    c <-
      ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    names(fangraphs_leaders) <- c
    
    # Remove percentages
    for (i in c(1,4:ncol(fangraphs_leaders))) {
      fangraphs_leaders[,i] <-
        str_trim(str_replace_all(fangraphs_leaders[,i],"%",""))
    }
    
    # Char to Number
    for (i in c(1,4:ncol(fangraphs_leaders))) {
      fangraphs_leaders[,i] <-
        as.numeric(as.character(fangraphs_leaders[,i]))
    }
    
    # replace NA with 0
    fangraphs_leaders[is.na(fangraphs_leaders)] <- 0
    
    return(fangraphs_leaders)
    
  }

###############################################################################
###############################################################################

#' Returns a full list of MLB players eligibility
#'  for hitters or pitchers for ESPN Standard leagues'
#'  Retreives primary position only
#' @param bat_pitch either 'bat' or 'pit'
#' @return data frame
#' @export
###############################################################################

ESPN_eligibility <- function(bat_pitch) {
  library(sqldf)
  library(data.table)
  library(XML)
  library(stringr)
  
  options(warn = -1)
  
  if (bat_pitch == 'bat') {
    end_params <- '&slotCategoryGroup=1'
  } else if (bat_pitch == 'pit') {
    end_params <- '&slotCategoryGroup=2'
  }
  
  base_url <-
    paste0("http://games.espn.go.com/flb/tools/eligibility?startIndex=")
  indx <- unlist(strsplit(as.character(seq(0, 900, 50)),','))
  urls <-
    paste0(base_url, rep(indx, each = 1), end_params)
  
  # Scrape
  ESPN <-
    lapply(urls, function(x) {
      data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
    })
  
  # Combine Scrapes
  eligibility <- c()
  for (i in c(1:18)) {
    eligibility <-
      as.data.frame(rbind(eligibility,ESPN[[i]]))
  }
  
  # Rename columns
  colnames(eligibility) <-
    c(
      "NameTeamPos","Name","C","x1B","x2B","x3B","SS","LF","CF","RF","DH","SP","RP"
    )
  
  
  # Remove DTD/SSPD/DL
  eligibility$NameTeamPos <-
    str_trim(gsub("DTD", "", eligibility$NameTeamPos))
  eligibility$NameTeamPos <-
    str_trim(gsub("SSPD", "", eligibility$NameTeamPos))
  eligibility$NameTeamPos <-
    str_trim(gsub("DL60", "", eligibility$NameTeamPos))
  eligibility$NameTeamPos <-
    str_trim(gsub("DL15", "", eligibility$NameTeamPos))
  
  # Parse Name
  eligibility$Name <- gsub(",.*$", "", eligibility$NameTeamPos)
  eligibility$Name <-
    str_trim(gsub('[^.a-zA-Z0-9]',' ',eligibility$Name))
  
  eligibility <- sqldf(
    "select eligibility.*,
    case when C = 'PP' then 'C'
    when x1B = 'PP' then '1B'
    when x2B = 'PP' then '2B'
    when x3B = 'PP' then '3B'
    when SS = 'PP' then 'SS'
    when LF = 'PP' then 'OF'
    when CF = 'PP' then 'OF'
    when RF = 'PP' then 'OF'
    when DH = 'PP' then 'DH'
    when SP = 'PP' then 'SP'
    when RP = 'PP' then 'RP'
    else '--' end as Position
    from eligibility
    "
  )
  
  return(eligibility)
}

###############################################################################
###############################################################################

#' Returns projections for hitters pr pitchers for WAR Games ESPN FB league (ONLY)
#' because column headings will be different for other leagues!!
#' @param bat_pitch either 'bat' or 'pit'
#' @param leagueID ESPN league ID
#' @examples
#' WARGames.Hitters<-WarGames_ESPN_proj('bat',86607)
#' @export
###############################################################################

WarGames_ESPN_proj <- function(bat_pitch,leagueID = 86607) {
  library(sqldf)
  library(data.table)
  library(XML)
  library(stringr)
  
  options(warn = -1)
  
  leagueID <- ifelse(is.null(leagueID),86607,86607)
  
  if (bat_pitch == 'bat') {
    end_params <- '&slotCategoryGroup=1'
    cols <-
      c(
        "Number","NameTeamPos","Type","Name","AB","H","x2B","x3B","HR","BB","HBP","SAC","SB","CS","PTS"
      )
  } else if (bat_pitch == 'pit') {
    end_params <- '&slotCategoryGroup=2'
    cols <-
      c(
        "Number","NameTeamPos","Type","Name","GS","IP","H","HR","BB","HB","K","SV","HD","PTS"
      )
  }
  
  base_url <-
    paste0(
      "http://games.espn.go.com/flb/tools/projections?leagueId="
      ,leagueID,end_params,"&startIndex="
    )
  indx <- unlist(strsplit(as.character(seq(0, 900, 40)),','))
  urls <- paste0(base_url, rep(indx, each = 1))
  
  # Scrape
  ESPNproj <-
    lapply(urls, function(x) {
      data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
    })
  
  # Combine Scrapes
  ESPNprojections <- c()
  for (i in c(1:15)) {
    ESPNprojections <-
      as.data.frame(rbind(ESPNprojections,ESPNproj[[i]]))
  }
  
  # Rename columns
  colnames(ESPNprojections) <- cols
  
  # Remove DTD/SSPD/DL
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("DTD", "", ESPNprojections$NameTeamPos))
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("SSPD", "", ESPNprojections$NameTeamPos))
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("DL60", "", ESPNprojections$NameTeamPos))
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("DL15", "", ESPNprojections$NameTeamPos))
  
  # Char to Number
  for (i in c(1,5:ncol(ESPNprojections))) {
    ESPNprojections[,i] <-
      as.numeric(as.character(ESPNprojections[,i]))
  }
  
  # replace NA with 0
  ESPNprojections[is.na(ESPNprojections)] <- 0
  
  # Clean Name
  ESPNprojections$Name <-
    gsub(",.*$", "", ESPNprojections$NameTeamPos)
  ESPNprojections$Name <-
    str_trim(gsub('[^.a-zA-Z0-9]',' ',ESPNprojections$Name))
  
  ESPNprojections$PTS <- round(ESPNprojections$PTS)
  
  # Sort by Points desc
  out <- ESPNprojections[order(-ESPNprojections$PTS),]
  
  return(out)
}


###############################################################################
###############################################################################

#' Returns projections for hitters or pitchers for a standard 5x5 ROTO leagues
#' @param bat_pitch either 'bat' or 'pit'
#' @param leagueID ESPN league ID
#' @examples
#' ESPN_hitters<-ESPN_proj('bat')
#'
###############################################################################

ESPN_proj <- function(bat_pitch,leagueID = 0) {
  library(data.table)
  library(XML)
  library(stringr)
  
  options(warn = -1)
  
  leagueID <- ifelse(is.null(leagueID),0,0)
  
  if (bat_pitch == 'bat') {
    end_params <- '&slotCategoryGroup=1'
    cols <- c("Number","NameTeamPos","R","HR","RBI","SB","AVG")
  } else if (bat_pitch == 'pit') {
    end_params <- '&slotCategoryGroup=2'
    cols <- c("Number","NameTeamPos","K","W","SV","ERA","WHIP")
  }
  
  base_url <-
    paste0(
      "http://games.espn.go.com/flb/tools/projections?leagueId="
      ,leagueID,end_params,"&startIndex="
    )
  indx <- unlist(strsplit(as.character(seq(0, 900, 40)),','))
  urls <- paste0(base_url, rep(indx, each = 1))
  
  # Scrape
  ESPNproj <-
    lapply(urls, function(x) {
      data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
    })
  
  # Combine Scrapes
  ESPNprojections <- c()
  for (i in c(1:15)) {
    ESPNprojections <-
      as.data.frame(rbind(ESPNprojections,ESPNproj[[i]]))
  }
  
  # Rename columns
  colnames(ESPNprojections) <- cols
  
  # Remove DTD/SSPD/DL
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("DTD", "", ESPNprojections$NameTeamPos))
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("SSPD", "", ESPNprojections$NameTeamPos))
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("DL60", "", ESPNprojections$NameTeamPos))
  ESPNprojections$NameTeamPos <-
    str_trim(gsub("DL15", "", ESPNprojections$NameTeamPos))
  
  # Char to Number
  for (i in c(1,5:ncol(ESPNprojections))) {
    ESPNprojections[,i] <-
      as.numeric(as.character(ESPNprojections[,i]))
  }
  
  # replace NA with 0
  ESPNprojections[is.na(ESPNprojections)] <- 0
  
  # Parse Name
  ESPNprojections$Name <-
    gsub(",.*$", "", ESPNprojections$NameTeamPos)
  ESPNprojections$Name <-
    str_trim(gsub('[^.a-zA-Z0-9]',' ',ESPNprojections$Name))
  
  # Sort by Rank
  out <- ESPNprojections[order(ESPNprojections$Number),]
  
  return(out)
}

###############################################################################
###############################################################################
#' Scrape Pitching or Hitting data from Baseball Reference
#'
#' Original code from Bill Petti and modified as needed
#' @param bat_pitch either 'bat' or 'pit'
#' @param t1 First date data should be scraped from. Should take the form "YEAR-DAY-MONTH"
#' @param t2 Second date data should be scraped from. Should take the form "YEAR-DAY-MONTH"
#' @param n use number of days prior from today
#' @export
#' @examples
#' bb_p<-bbref_leaderboard('pit',"2015-05-10", "2015-06-20")
#' bb_h<-bbref_leaderboard('bat',n=30) # current year; last 30 days from today

bbref_leaderboard <- function(bat_pitch, t1 = NULL, t2 = NULL, n = 30) {
  library(rvest)
  library(dplyr)
  library(stringr)
  
  if (bat_pitch == 'bat') {
    pos <- 'h'
    base_url <-
      paste0(
        "http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=",pos,
        "&lastndays=",n,
        "&dates=fromandto&fromandto=", t1, ".", t2,
        "&level=mlb&franch=&stat=&stat_value=0"
      )
    df <- read_html(base_url)
    df <-
      df %>% html_nodes(xpath = '//*[@id="daily"]') %>% html_table(fill = TRUE)
    df <- as.data.frame(df)[-c(1,3,5)]
    names(df)[1:4] <- c("Name", "Age", "Level", "Team")
    
    # Remove percentages
    for (i in c(5:ncol(df))) {
      df[,i] <-
        str_trim(str_replace_all(df[,i],"%",""))
    }
    
    # Char to Number
    for (i in c(5:ncol(df))) {
      df[,i] <-
        as.numeric(as.character(df[,i]))
    }
    
    # replace NA with 0
    df[is.na(df)] <- 0
    
    df$X1B <- with(df, H - (X2B + X3B + HR))
    currentyear <- as.numeric(format(Sys.time(), "%Y"))
    t1 <- ifelse(is.null(t1),currentyear,t1)
    season <- substr(t1, 1, 4)
    df$season <- season
    df$uBB <- with(df, BB - IBB)
    
    df$Team <- gsub(" $", "", df$Team, perl = T)
    df <- filter_(df, ~ Name != "Name")
    df <- arrange_(df, ~ desc(AB), ~ desc(OPS))
    
    
  } else if (bat_pitch == 'pit') {
    pos <- 'p'
    base_url <-
      paste0(
        "http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=",pos,
        "&lastndays=",n,
        "&dates=fromandto&fromandto=", t1, ".", t2,
        "&level=mlb&franch=&stat=&stat_value=0"
      )
    df <- read_html(base_url)
    df <-
      df %>% html_nodes(xpath = '//*[@id="daily"]') %>% html_table(fill = TRUE)
    df <- as.data.frame(df)[-c(1,3,5)]
    names(df)[1:4] <- c("Name", "Age", "Level", "Team")
    
    # Remove percentages
    for (i in c(5:ncol(df))) {
      df[,i] <-
        str_trim(str_replace_all(df[,i],"%",""))
    }
    
    # Remove percentages
    for (i in c(5:ncol(df))) {
      df[,i] <-
        str_trim(str_replace_all(df[,i],"%",""))
    }
    
    # Char to Number
    for (i in c(5:ncol(df))) {
      df[,i] <-
        as.numeric(as.character(df[,i]))
    }
    
    # replace NA with 0
    df[is.na(df)] <- 0
    
    df$X1B <- with(df, H - (X2B + X3B + HR))
    currentyear <- as.numeric(format(Sys.time(), "%Y"))
    t1 <- ifelse(is.null(t1),currentyear,t1)
    season <- substr(t1, 1, 4)
    df$season <- season
    df$uBB <- with(df, BB - IBB)
    
    # Percent to decimal
    df$Str <- df$Str / 100
    df$StL <- df$StL / 100
    df$StS <- df$StS / 100
    df$GB.FB <- df$GB.FB / 100
    
    df$SO_perc <- with(df, round(SO / BF,3))
    df$uBB_perc <- with(df, round(uBB / BF,3))
    df$SO_uBB <- with(df, round(SO_perc - uBB_perc))
    df$Team <- gsub(" $", "", df$Team, perl = T)
    df <- filter_(df, ~ Name != "Name")
    df <- arrange_(df, ~ desc(IP), ~ desc(WHIP))
  }
  
  return(df)
}

###############################################################################
###############################################################################
#' Scrape Home Run Tracker leadersboards
#'
#' @details Limitations: Cannot change year. Always current
#' @param bat_pitch either 'bat' or 'pit'
#' @export
#' @examples
#' hrt<-HRtracker_leaderboard('bat')

HRtracker_leaderboard <- function(bat_pitch = 'bat') {
  library(XML)
  library(dplyr)
  library(stringr)
  library(griffun)
  library(Hmisc)
  
  if (bat_pitch == 'bat') {
    # No doubt HR
    url <-
      "http://www.hittrackeronline.com/homeruns_special.php?league=&type=ND"
    htmltbl <-
      readHTMLTable(doc = url)
    nd <- data.frame(htmltbl[5])
    colnames(nd) <-
      c("RawName","HR")
    
    # Just enough HR
    url <-
      "http://www.hittrackeronline.com/homeruns_special.php?league=&type=JE"
    htmltbl <-
      readHTMLTable(doc = url)
    je <- data.frame(htmltbl[5])
    colnames(je) <-
      c("RawName","HR")
    
    # Lucky HR
    url <-
      "http://www.hittrackeronline.com/homeruns_special_luck.php?league=&type=L"
    htmltbl <-
      readHTMLTable(doc = url)
    luck <- data.frame(htmltbl[5])
    colnames(luck) <-
      c("RawName","HR")
    
    # Golden Sledgehammer
    url <- "http://www.hittrackeronline.com/golden_sledgehammer.php"
    htmltbl <-
      readHTMLTable(doc = url)
    gs <- data.frame(htmltbl[5])
    colnames(gs) <-
      c("RawName","Team","GoldenSledgeHammer","Avg Distance")
    
    #Full outer join
    m1 <- merge(x = gs, y = nd, by = "RawName", all = TRUE)
    m2 <- merge(x = m1, y = je, by = "RawName", all = TRUE)
    df <- merge(x = m2, y = luck, by = "RawName", all = TRUE)
    
    colnames(df) <-
      c(
        "RawName","Team","GoldenSledgeHammer","Avg Distance","NoDoubt","JustEnough","Lucky"
      )
    
  }
  
  
  else if (bat_pitch == 'pit') {
    # No doubt HR
    url <-
      "http://www.hittrackeronline.com/homeruns_allowed_special.php?league=&type=ND"
    htmltbl <-
      readHTMLTable(doc = url)
    nd <- data.frame(htmltbl[5])
    colnames(nd) <-
      c("RawName","HR")
    
    # Just enough HR
    url <-
      "http://www.hittrackeronline.com/homeruns_allowed_special.php?league=&type=JE"
    htmltbl <-
      readHTMLTable(doc = url)
    je <- data.frame(htmltbl[5])
    colnames(je) <-
      c("RawName","HR")
    
    # Lucky HR
    url <-
      "http://www.hittrackeronline.com/homeruns_allowed_special_luck.php?league=&type=L"
    htmltbl <-
      readHTMLTable(doc = url)
    luck <- data.frame(htmltbl[5])
    colnames(luck) <-
      c("RawName","HR")
    
    # Golden Anvil
    url <- "http://www.hittrackeronline.com/golden_anvil.php"
    htmltbl <-
      readHTMLTable(doc = url)
    gs <- data.frame(htmltbl[5])
    colnames(gs) <-
      c("RawName","Team","GoldenAnvil","Avg Distance")
    
    #Full outer join
    m1 <- merge(x = gs, y = nd, by = "RawName", all = TRUE)
    m2 <- merge(x = m1, y = je, by = "RawName", all = TRUE)
    df <- merge(x = m2, y = luck, by = "RawName", all = TRUE)
    
    colnames(df) <-
      c(
        "RawName","Team","GoldenAnvil","Avg Distance","NoDoubt","JustEnough","Lucky"
      )
    
  }
  
  # Char to Number
  for (i in c(3:ncol(df))) {
    df[,i] <-
      as.numeric(as.character(df[,i]))
  }
  
  # replace NA with 0
  df[is.na(df)] <- 0
  
  # Currently does NOT handle Jr.
  df$Name <- reverse_name(df$RawName)
  
  return(df)
}
###############################################################################
###############################################################################
#' Scrape Baseball Heatmaps leadersboards
#'
#' @details Limitations: Cannot change year. Always current
#' @param bat_pitch either 'bat' or 'pit'
#' @export
#' @examples
#' heatmaps<-Heatmaps_leaderboard('bat')

Heatmaps_leaderboard <- function(bat_pitch = 'bat') {
  library(XML)
  library(dplyr)
  library(stringr)
  library(griffun)
  library(Hmisc)
  library(sqldf)
  
  if (bat_pitch == 'bat') {
    url <- "http://www.baseballheatmaps.com/graph/distanceleader.php"
    htmltbl <-
      readHTMLTable(doc = url)
    df <- data.frame(htmltbl[1])
    colnames(df) <-
      c("Rank","RawName","Stance","Year","Hits","Distance","Angle")
    
    df$RawName <-
      str_replace(df$RawName,"Stanton Michael","Stanton Giancarlo")
    df$RawName <-
      str_replace(df$RawName,"Davis Khristopher","Davis Khris")
    df$RawName <-
      str_replace(df$RawName,"Moreland Mitchell","Moreland Mitch")
    df$RawName <-
      str_replace(df$RawName,"Machado Manuel","Machado Manny")
    df$RawName <-
      str_replace(df$RawName,"Freeman Frederick","Freeman Freddie")
    df$RawName <-
      str_replace(df$RawName,"Park Byung Ho","Park Byung-ho")
    
    # Currently does NOT handle Jr.
    df$Name <- reverse_name(df$RawName)
    
    # Resovle duplicates because of Stance
    df <- sqldf(
      "select Name, Year, sum(Hits) as Hits,
      avg(Distance) as Distance,
      avg(Angle) as Angle
      from df
      group by Name, Year
      "
    )
    
  }
  
  else if (bat_pitch == 'pit') {
    url <-
      "http://www.baseballheatmaps.com/graph/pitcherdistanceleader.php"
    htmltbl <-
      readHTMLTable(doc = url)
    df <- data.frame(htmltbl[1])
    colnames(df) <-
      c("Rank","RawName","Stance","Year","Hits","Distance","Angle")
    
    df$RawName <-
      str_replace(df$RawName,"Fister Douglas","Fister Doug")
    df$RawName <-
      str_replace(df$RawName,"Archer Christopher","Archer Chris")
    df$RawName <-
      str_replace(df$RawName,"Bolsinger Michael","Bolsinger Mike")
    df$RawName <-
      str_replace(df$RawName,"Colome Alexander","Colome Alex")
    df$RawName <-
      str_replace(df$RawName,"Martinez Nicholas","Martinez Nick")
    
    # Currently does NOT handle Jr.
    df$Name <- reverse_name(df$RawName)
    
    # Resolve duplicates because of Stance
    df <- sqldf(
      "select Name, Year, sum(Hits) as Hits,
      avg(Distance) as Distance,
      avg(Angle) as Angle
      from df
      group by Name, Year
      "
    )
  }
  
  # Char to Number
  for (i in c(3:ncol(df))) {
    df[,i] <-
      as.numeric(as.character(df[,i]))
  }
  
  # replace NA with 0
  df[is.na(df)] <- 0
  
  return(df)
  }
###############################################################################