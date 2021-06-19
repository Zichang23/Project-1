Project 1
================
Zichang Xiang
6/16/2021

-   [Reading and summarizing data from NHL
    API](#reading-and-summarizing-data-from-nhl-api)
-   [Functions to contact the NHL records
    API](#functions-to-contact-the-nhl-records-api)
-   [Function to contact the NHL stats
    API](#function-to-contact-the-nhl-stats-api)
-   [Wrapper function to call other
    functions](#wrapper-function-to-call-other-functions)
-   [Basic exploratory data analysis](#basic-exploratory-data-analysis)

### Reading and summarizing data from NHL API

#### Required Packages

jsonlite, RCurl, rmarkdown, httr, and ggplot2

``` r
#load the required packages
library(rmarkdown)
library(dplyr)
library(jsonlite)
library(RCurl)
library(httr)
library(ggplot2)
```

### Functions to contact the NHL records API

#### Function to return franchise

``` r
#retrieve the info and convert to a list
get_franchise <- GET("https://records.nhl.com/site/api/franchise")
franchise_cont <- content(get_franchise, "text", encoding = "UTF-8")
franchise_json<- fromJSON(franchise_cont, flatten = TRUE)
franchise_list <-as_tibble(franchise_json$data)
#create function franchise
franchise <- function(name) {
  if (is.null(name)){
  return(franchise_list)
  }else if (is.character(name)){
  return(franchise_list %>% 
           filter(fullName == name | teamAbbrev == name | teamCommonName == name |
                    teamPlaceName == name))
  }else
  return(franchise_list %>% filter(mostRecentTeamId == name))
}
#try to use function franchise
franchise(NULL)
```

    ## # A tibble: 39 x 8
    ##       id firstSeasonId fullName             lastSeasonId mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ##    <int>         <int> <chr>                       <int>            <int> <chr>      <chr>          <chr>        
    ##  1     1      19171918 Montréal Canadiens             NA                8 MTL        Canadiens      Montréal     
    ##  2     2      19171918 Montreal Wanderers       19171918               41 MWN        Wanderers      Montreal     
    ##  3     3      19171918 St. Louis Eagles         19341935               45 SLE        Eagles         St. Louis    
    ##  4     4      19191920 Hamilton Tigers          19241925               37 HAM        Tigers         Hamilton     
    ##  5     5      19171918 Toronto Maple Leafs            NA               10 TOR        Maple Leafs    Toronto      
    ##  6     6      19241925 Boston Bruins                  NA                6 BOS        Bruins         Boston       
    ##  7     7      19241925 Montreal Maroons         19371938               43 MMR        Maroons        Montreal     
    ##  8     8      19251926 Brooklyn Americans       19411942               51 BRK        Americans      Brooklyn     
    ##  9     9      19251926 Philadelphia Quakers     19301931               39 QUA        Quakers        Philadelphia 
    ## 10    10      19261927 New York Rangers               NA                3 NYR        Rangers        New York     
    ## # … with 29 more rows

#### Function to return total stats for every franchise

``` r
#retrieve info and convert to a list
get_total <- GET("https://records.nhl.com/site/api/franchise-team-totals")
total_cont <- content(get_total, "text", encoding = "UTF-8")
total_json<- fromJSON(total_cont, flatten = TRUE)
total_list <-as_tibble(total_json$data)
#create function team_totals
team_totals <- function(name) {
  if (is.null(name)){
  return(total_list)
  }else if (is.character(name)){
  return(total_list %>% filter(teamName == name | triCode == name))
  }else
  return(total_list %>% filter(franchiseId == name) )
}

#try to use function team_totals
team_totals("MTL")
```

    ## # A tibble: 2 x 30
    ##      id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses
    ##   <int>           <int>         <int>       <int>      <int>       <int>        <int>    <int>      <int>              <int>
    ## 1    15               1      19171918           1          3         773         1959     2306        133                  0
    ## 2    16               1      19171918           1          2        6787        18260    21791        881                 95
    ## # … with 20 more variables: homeTies <int>, homeWins <int>, lastSeasonId <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>, roadTies <int>,
    ## #   roadWins <int>, shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>, ties <int>,
    ## #   triCode <chr>, wins <int>

``` r
team_totals(NULL)
```

    ## # A tibble: 105 x 30
    ##       id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses
    ##    <int>           <int>         <int>       <int>      <int>       <int>        <int>    <int>      <int>              <int>
    ##  1     1               1      19821983          23          2        2993         8902     8792        525                 85
    ##  2     2               1      19821983          23          3         257          634      697         53                  0
    ##  3     3               1      19721973          22          2        3788        11907    12045        678                 84
    ##  4     4               1      19721973          22          3         309          897      983         53                  1
    ##  5     5               1      19261927          10          2        6560        20020    20041       1143                 76
    ##  6     6               1      19261927          10          3         518         1447     1404        104                  0
    ##  7     7               1      19671968          16          3         449         1332     1335         97                  0
    ##  8     8               1      19671968          16          2        4171        12255    13690        584                 93
    ##  9     9               1      19671968          17          2        4171        14049    13874        683                 60
    ## 10    10               1      19671968          17          3         391         1131     1190         85                  0
    ## # … with 95 more rows, and 20 more variables: homeTies <int>, homeWins <int>, lastSeasonId <int>, losses <int>,
    ## #   overtimeLosses <int>, penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>,
    ## #   roadTies <int>, roadWins <int>, shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>,
    ## #   ties <int>, triCode <chr>, wins <int>

#### Function to return season records

``` r
#create function season
season <- function(name) {
  if (is.null(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-season-records")
    get_season <- GET(base_url)
    season_cont <- content(get_season, "text", encoding = "UTF-8")
    season_json <- fromJSON(season_cont, flatten = TRUE)
    season_list <- as_tibble(season_json$data)
  return(season_list)
  }else if (is.numeric(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=", name)
    get_season <- GET(base_url)
    season_cont <- content(get_season, "text", encoding = "UTF-8")
    season_json <- fromJSON(season_cont, flatten = TRUE)
    season_list <- as_tibble(season_json$data)
  return(season_list)
  }else{
    get_season <- GET("https://records.nhl.com/site/api/franchise-season-records")
    season_cont <- content(get_season, "text", encoding = "UTF-8")
    season_json <- fromJSON(season_cont, flatten = TRUE)
    season_list <- as_tibble(season_json$data)
  return(season_list %>% filter(franchiseName == name))
  }
}

#try to use function season
season("Montréal Canadiens")
```

    ## # A tibble: 1 x 57
    ##      id fewestGoals fewestGoalsAgainst fewestGoalsAgainstSeaso… fewestGoalsSeaso… fewestLosses fewestLossesSeaso… fewestPoints
    ##   <int>       <int>              <int> <chr>                    <chr>                    <int> <chr>                     <int>
    ## 1     8         155                131 1955-56 (70)             1952-53 (70)                 8 1976-77 (80)                 65
    ## # … with 49 more variables: fewestPointsSeasons <chr>, fewestTies <int>, fewestTiesSeasons <chr>, fewestWins <int>,
    ## #   fewestWinsSeasons <chr>, franchiseId <int>, franchiseName <chr>, homeLossStreak <int>, homeLossStreakDates <chr>,
    ## #   homePointStreak <int>, homePointStreakDates <chr>, homeWinStreak <int>, homeWinStreakDates <chr>,
    ## #   homeWinlessStreak <int>, homeWinlessStreakDates <chr>, lossStreak <int>, lossStreakDates <chr>, mostGameGoals <int>,
    ## #   mostGameGoalsDates <chr>, mostGoals <int>, mostGoalsAgainst <int>, mostGoalsAgainstSeasons <chr>, mostGoalsSeasons <chr>,
    ## #   mostLosses <int>, mostLossesSeasons <chr>, mostPenaltyMinutes <int>, mostPenaltyMinutesSeasons <chr>, mostPoints <int>,
    ## #   mostPointsSeasons <chr>, mostShutouts <int>, mostShutoutsSeasons <chr>, mostTies <int>, mostTiesSeasons <chr>,
    ## #   mostWins <int>, mostWinsSeasons <chr>, pointStreak <int>, pointStreakDates <chr>, roadLossStreak <int>,
    ## #   roadLossStreakDates <chr>, roadPointStreak <int>, roadPointStreakDates <chr>, roadWinStreak <int>,
    ## #   roadWinStreakDates <chr>, roadWinlessStreak <int>, roadWinlessStreakDates <chr>, winStreak <int>, winStreakDates <chr>,
    ## #   winlessStreak <int>, winlessStreakDates <chr>

``` r
season(NULL)
```

    ## # A tibble: 39 x 57
    ##       id fewestGoals fewestGoalsAgain… fewestGoalsAgainst… fewestGoalsSeasons fewestLosses fewestLossesSeasons    fewestPoints
    ##    <int>       <int>             <int> <chr>               <chr>                     <int> <chr>                         <int>
    ##  1     1         174               164 2003-04 (82)        2010-11 (82)                 19 2000-01 (82)                     36
    ##  2     2         170               190 1975-76 (80)        1972-73 (78)                 15 1978-79 (80)                     30
    ##  3     3         150               177 1970-71 (78)        1954-55 (70)                 17 1971-72 (78)                     47
    ##  4     4         173               164 1973-74 (78)        1967-68 (74)                 12 1979-80 (80)                     56
    ##  5     5         182               188 1997-98 (82)        1969-70 (76)                 21 1992-93 (84), 2016-17…           38
    ##  6     6         147               172 1952-53 (70)        1955-56 (70)                 13 1971-72 (78)                     38
    ##  7     7         157               175 1998-99 (82)        2013-14 (82)                 16 1974-75 (80)                     51
    ##  8     8         155               131 1955-56 (70)        1952-53 (70)                  8 1976-77 (80)                     65
    ##  9     9         191               179 1998-99 (82)        1995-96 (82), 201…           21 2000-01 (82), 2002-03…           24
    ## 10    10         147               131 1953-54 (70)        1954-55 (70)                 16 1950-51 (70)                     48
    ## # … with 29 more rows, and 49 more variables: fewestPointsSeasons <chr>, fewestTies <int>, fewestTiesSeasons <chr>,
    ## #   fewestWins <int>, fewestWinsSeasons <chr>, franchiseId <int>, franchiseName <chr>, homeLossStreak <int>,
    ## #   homeLossStreakDates <chr>, homePointStreak <int>, homePointStreakDates <chr>, homeWinStreak <int>,
    ## #   homeWinStreakDates <chr>, homeWinlessStreak <int>, homeWinlessStreakDates <chr>, lossStreak <int>, lossStreakDates <chr>,
    ## #   mostGameGoals <int>, mostGameGoalsDates <chr>, mostGoals <int>, mostGoalsAgainst <int>, mostGoalsAgainstSeasons <chr>,
    ## #   mostGoalsSeasons <chr>, mostLosses <int>, mostLossesSeasons <chr>, mostPenaltyMinutes <int>,
    ## #   mostPenaltyMinutesSeasons <chr>, mostPoints <int>, mostPointsSeasons <chr>, mostShutouts <int>,
    ## #   mostShutoutsSeasons <chr>, mostTies <int>, mostTiesSeasons <chr>, mostWins <int>, mostWinsSeasons <chr>,
    ## #   pointStreak <int>, pointStreakDates <chr>, roadLossStreak <int>, roadLossStreakDates <chr>, roadPointStreak <int>,
    ## #   roadPointStreakDates <chr>, roadWinStreak <int>, roadWinStreakDates <chr>, roadWinlessStreak <int>,
    ## #   roadWinlessStreakDates <chr>, winStreak <int>, winStreakDates <chr>, winlessStreak <int>, winlessStreakDates <chr>

#### Function to return goalie records

``` r
#create function goalie
goalie <- function(name) {
  if (is.null(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-goalie-records")
    get_goalie <- GET(base_url)
    goalie_cont <- content(get_goalie, "text", encoding = "UTF-8")
    goalie_json <- fromJSON(goalie_cont, flatten = TRUE)
    goalie_list <- as_tibble(goalie_json$data)
  return(goalie_list)
  }else if (is.numeric(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-goalie-records?cayenneExp=franchiseId=", name)
    get_goalie <- GET(base_url)
    goalie_cont <- content(get_goalie, "text", encoding = "UTF-8")
    goalie_json <- fromJSON(goalie_cont, flatten = TRUE)
    goalie_list <- as_tibble(goalie_json$data)
  return(goalie_list %>% filter(franchiseId == name))
  }else{
    get_goalie <- GET("https://records.nhl.com/site/api/franchise-goalie-records")
    goalie_cont <- content(get_goalie, "text", encoding = "UTF-8")
    goalie_json <- fromJSON(goalie_cont, flatten = TRUE)
    goalie_list <- as_tibble(goalie_json$data)
  return(goalie_list %>% filter(franchiseName == name | lastName == name | 
                                    firstName == name))
  }
}

#try to use function goalie
goalie(1)
```

    ## # A tibble: 38 x 29
    ##       id activePlayer firstName franchiseId franchiseName   gameTypeId gamesPlayed lastName  losses mostGoalsAgainstDates     
    ##    <int> <lgl>        <chr>           <int> <chr>                <int>       <int> <chr>      <int> <chr>                     
    ##  1  1182 FALSE        Ken                 1 Montréal Canad…          2         397 Dryden        57 1974-11-13, 1972-01-22    
    ##  2   414 FALSE        Stephane            1 Montréal Canad…          2           2 Fiset          1 2002-04-12                
    ##  3   437 FALSE        Jeff                1 Montréal Canad…          2         161 Hackett       68 2001-12-29, 2000-04-02, 2…
    ##  4   450 FALSE        Brian               1 Montréal Canad…          2         141 Hayward       48 1990-03-24, 1988-11-13, 1…
    ##  5   457 FALSE        Denis               1 Montréal Canad…          2          86 Herron        18 1980-01-26                
    ##  6   469 FALSE        Pat                 1 Montréal Canad…          2          40 Jablonski     15 1996-02-23, 1995-12-07    
    ##  7   511 FALSE        Roland              1 Montréal Canad…          2           9 Melanson       3 1991-11-21                
    ##  8   527 FALSE        Andy                1 Montréal Canad…          2          42 Moog          17 1998-03-26, 1997-10-11    
    ##  9   549 FALSE        Lorne               1 Montréal Canad…          2          47 Chabot        20 1933-12-05                
    ## 10   559 FALSE        Abbie               1 Montréal Canad…          2           1 Cox            0 1936-02-16                
    ## # … with 28 more rows, and 19 more variables: mostGoalsAgainstOneGame <int>, mostSavesDates <chr>, mostSavesOneGame <int>,
    ## #   mostShotsAgainstDates <chr>, mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>, mostShutoutsSeasonIds <chr>,
    ## #   mostWinsOneSeason <int>, mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>, positionCode <chr>,
    ## #   rookieGamesPlayed <int>, rookieShutouts <int>, rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
goalie(NULL)
```

    ## # A tibble: 1,078 x 29
    ##       id activePlayer firstName franchiseId franchiseName     gameTypeId gamesPlayed lastName losses mostGoalsAgainstDates    
    ##    <int> <lgl>        <chr>           <int> <chr>                  <int>       <int> <chr>     <int> <chr>                    
    ##  1   235 FALSE        Don                15 Dallas Stars               2         315 Beaupre     125 1983-10-07               
    ##  2   236 FALSE        Bob                28 Arizona Coyotes            2         281 Essensa     114 1992-12-11, 1992-10-12   
    ##  3   237 FALSE        Tony               11 Chicago Blackhaw…          2         873 Esposito    302 1983-10-15, 1980-11-26   
    ##  4   238 FALSE        Grant              25 Edmonton Oilers            2         423 Fuhr        117 1984-02-05, 1982-10-12   
    ##  5   239 FALSE        Ron                16 Philadelphia Fly…          2         489 Hextall     172 1987-04-05               
    ##  6   240 FALSE        Curtis             18 St. Louis Blues            2         280 Joseph       96 1992-11-25, 1990-02-20   
    ##  7   241 FALSE        Olie               24 Washington Capit…          2         711 Kolzig      293 2006-01-25, 2005-10-08, …
    ##  8   242 FALSE        Mike               18 St. Louis Blues            2         347 Liut        133 1982-02-25               
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks          2         516 McLean      228 1996-10-19               
    ## 10   244 FALSE        Gilles             13 Cleveland Barons           2         250 Meloche     140 1973-10-21               
    ## # … with 1,068 more rows, and 19 more variables: mostGoalsAgainstOneGame <int>, mostSavesDates <chr>, mostSavesOneGame <int>,
    ## #   mostShotsAgainstDates <chr>, mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>, mostShutoutsSeasonIds <chr>,
    ## #   mostWinsOneSeason <int>, mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>, positionCode <chr>,
    ## #   rookieGamesPlayed <int>, rookieShutouts <int>, rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

#### Function to return skater records

``` r
#create function skater
skater <- function(name) {
  if (is.null(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-skater-records")
    get_skater <- GET(base_url)
    skater_cont <- content(get_skater, "text", encoding = "UTF-8")
    skater_json <- fromJSON(skater_cont, flatten = TRUE)
    skater_list <- as_tibble(skater_json$data)
  return(skater_list)
  }else if (is.numeric(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-skater-records?cayenneExp=franchiseId=", name)
    get_skater <- GET(base_url)
    skater_cont <- content(get_skater, "text", encoding = "UTF-8")
    skater_json <- fromJSON(skater_cont, flatten = TRUE)
    skater_list <- as_tibble(skater_json$data)
  return(skater_list %>% filter(franchiseId == name))
  }else{
    get_skater<- GET("https://records.nhl.com/site/api/franchise-skater-records")
    skater_cont <- content(get_skater, "text", encoding = "UTF-8")
    skater_json <- fromJSON(skater_cont, flatten = TRUE)
    skater_list <- as_tibble(skater_json$data)
  return(skater_list %>% filter(franchiseName == name | lastName==name | 
                                    firstName == name))
  }
}

#try to use function skater
skater(1)
```

    ## # A tibble: 800 x 31
    ##       id activePlayer assists firstName franchiseId franchiseName  gameTypeId gamesPlayed goals lastName mostAssistsGameDates 
    ##    <int> <lgl>          <int> <chr>           <int> <chr>               <int>       <int> <int> <chr>    <chr>                
    ##  1 17199 FALSE              0 Reg                 1 Montréal Cana…          2           3     0 Abbott   1952-10-09, 1952-10-…
    ##  2 17223 FALSE              2 Art                 1 Montréal Cana…          2          11     0 Alexand… 1932-03-08, 1932-03-…
    ##  3 17272 FALSE              0 Dave                1 Montréal Cana…          2           3     0 Allison  1983-10-06, 1983-10-…
    ##  4 17351 FALSE              0 Ossie               1 Montréal Cana…          2           2     0 Asmunds… 1937-11-09, 1937-11-…
    ##  5 17389 FALSE              0 Ron                 1 Montréal Cana…          2           6     0 Andruff  1974-10-09, 1974-10-…
    ##  6 17440 FALSE              0 Jimmy               1 Montréal Cana…          2           2     0 Bartlett 1954-10-07, 1954-10-…
    ##  7 17484 FALSE              0 Max                 1 Montréal Cana…          2           1     0 Bennett  1935-11-12, 1935-11-…
    ##  8 17508 FALSE              0 Bob                 1 Montréal Cana…          2           2     0 Berry    1968-10-12, 1968-10-…
    ##  9 17544 FALSE              0 Garry               1 Montréal Cana…          2           1     0 Blaine   1954-10-07, 1954-10-…
    ## 10 17623 FALSE              0 Conrad              1 Montréal Cana…          2           6     0 Bourcier 1935-11-12, 1935-11-…
    ## # … with 790 more rows, and 20 more variables: mostAssistsOneGame <int>, mostAssistsOneSeason <int>,
    ## #   mostAssistsSeasonIds <chr>, mostGoalsGameDates <chr>, mostGoalsOneGame <int>, mostGoalsOneSeason <int>,
    ## #   mostGoalsSeasonIds <chr>, mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>,
    ## #   mostPointsGameDates <chr>, mostPointsOneGame <int>, mostPointsOneSeason <int>, mostPointsSeasonIds <chr>,
    ## #   penaltyMinutes <int>, playerId <int>, points <int>, positionCode <chr>, rookieGamesPlayed <int>, rookiePoints <int>,
    ## #   seasons <int>

``` r
skater(NULL)
```

    ## # A tibble: 17,209 x 31
    ##       id activePlayer assists firstName franchiseId franchiseName  gameTypeId gamesPlayed goals lastName mostAssistsGameDates 
    ##    <int> <lgl>          <int> <chr>           <int> <chr>               <int>       <int> <int> <chr>    <chr>                
    ##  1 16888 FALSE            417 George              5 Toronto Maple…          2        1188   296 Armstro… 1956-01-07, 1957-03-…
    ##  2 16889 FALSE              0 Billy               2 Montreal Wand…          2           2     1 Bell     1917-12-19, 1917-12-…
    ##  3 16890 FALSE            794 Johnny              6 Boston Bruins           2        1436   545 Bucyk    1971-01-01           
    ##  4 16891 FALSE            712 Jean                1 Montréal Cana…          2        1125   507 Beliveau 1955-02-19, 1956-12-…
    ##  5 16892 FALSE           1111 Ray                 6 Boston Bruins           2        1518   395 Bourque  1990-02-18, 1994-01-…
    ##  6 16893 FALSE             33 Harold              9 Philadelphia …          2         216    60 Darragh  1926-01-19, 1929-11-…
    ##  7 16894 FALSE             13 Herb                9 Philadelphia …          2         216    24 Drury    1926-02-06, 1926-03-…
    ##  8 16895 FALSE            852 Bobby              16 Philadelphia …          2        1144   358 Clarke   1976-04-01           
    ##  9 16896 FALSE            142 Ken                23 New Jersey De…          2        1283    36 Daneyko  1999-02-13           
    ## 10 16897 FALSE              0 Gerry               2 Montreal Wand…          2           4     0 Geran    1917-12-19, 1917-12-…
    ## # … with 17,199 more rows, and 20 more variables: mostAssistsOneGame <int>, mostAssistsOneSeason <int>,
    ## #   mostAssistsSeasonIds <chr>, mostGoalsGameDates <chr>, mostGoalsOneGame <int>, mostGoalsOneSeason <int>,
    ## #   mostGoalsSeasonIds <chr>, mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>,
    ## #   mostPointsGameDates <chr>, mostPointsOneGame <int>, mostPointsOneSeason <int>, mostPointsSeasonIds <chr>,
    ## #   penaltyMinutes <int>, playerId <int>, points <int>, positionCode <chr>, rookieGamesPlayed <int>, rookiePoints <int>,
    ## #   seasons <int>

#### Function to return detail records

``` r
#create function detail
detail <- function(name) {
  if (is.null(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-detail?")
    get_detail <- GET(base_url)
    detail_cont <- content(get_detail, "text", encoding = "UTF-8")
    detail_json <- fromJSON(detail_cont, flatten = TRUE)
    detail_list <- as_tibble(detail_json$data)
  return(detail_list)  
  }else if (is.numeric(name)){
    base_url <- paste0("https://records.nhl.com/site/api/franchise-detail?cayenneExp=mostRecentTeamId=", name)
    get_detail <- GET(base_url)
    detail_cont <- content(get_detail, "text", encoding = "UTF-8")
    detail_json <- fromJSON(detail_cont, flatten = TRUE)
    detail_list <- as_tibble(detail_json$data)
  return(detail_list %>% filter(mostRecentTeamId == name))
  }else{
    get_detail <- GET("https://records.nhl.com/site/api/franchise-detail")
    detail_cont <- content(get_detail, "text", encoding = "UTF-8")
    detail_json <- fromJSON(detail_cont, flatten = TRUE)
    detail_list <- as_tibble(detail_json$data)
  return(detail_list %>% filter(teamAbbrev == name | teamFullName == name))
  }
}

#try to use function detail
detail("Vancouver Canucks")
```

    ## # A tibble: 1 x 13
    ##      id active captainHistory     coachingHistory     dateAwarded directoryUrl firstSeasonId generalManagerHist… heroImageUrl 
    ##   <int> <lgl>  <chr>              <chr>               <chr>       <chr>                <int> <chr>               <chr>        
    ## 1    20 TRUE   "<ul class=\"stri… "<ul class=\"strip… 1970-05-22… https://www…      19701971 "<ul class=\"strip… https://reco…
    ## # … with 4 more variables: mostRecentTeamId <int>, retiredNumbersSummary <chr>, teamAbbrev <chr>, teamFullName <chr>

``` r
detail(NULL)
```

    ## # A tibble: 39 x 13
    ##       id active captainHistory    coachingHistory    dateAwarded  directoryUrl firstSeasonId generalManagerHis… heroImageUrl  
    ##    <int> <lgl>  <chr>             <chr>              <chr>        <chr>                <int> <chr>              <chr>         
    ##  1     1 TRUE   "<ul class=\"str… "<ul class=\"stri… 1917-11-26T… https://www…      19171918 "<ul class=\"stri… https://recor…
    ##  2     2 FALSE   <NA>              <NA>              1917-11-26T… <NA>              19171918  <NA>              https://recor…
    ##  3     3 FALSE   <NA>              <NA>              1917-11-26T… <NA>              19171918  <NA>              https://recor…
    ##  4     4 FALSE   <NA>              <NA>              1917-11-26T… <NA>              19191920  <NA>              https://recor…
    ##  5     5 TRUE   "<ul class=\"str… "<ul class=\"stri… 1917-11-26T… https://www…      19171918 "<ul class=\"stri… https://recor…
    ##  6     6 TRUE   "<ul class=\"str… "<ul class=\"stri… 1924-11-01T… https://www…      19241925 "<ul class=\"stri… https://recor…
    ##  7     7 FALSE   <NA>              <NA>              1924-11-01T… <NA>              19241925  <NA>              https://recor…
    ##  8     8 FALSE   <NA>              <NA>              1925-09-22T… <NA>              19251926  <NA>              https://recor…
    ##  9     9 FALSE   <NA>              <NA>              1925-11-07T… <NA>              19251926  <NA>              https://recor…
    ## 10    10 TRUE   "<ul class=\"str… "<ul class=\"stri… 1926-05-15T… https://www…      19261927 "<ul class=\"stri… https://recor…
    ## # … with 29 more rows, and 4 more variables: mostRecentTeamId <int>, retiredNumbersSummary <chr>, teamAbbrev <chr>,
    ## #   teamFullName <chr>

### Function to contact the NHL stats API

``` r
#create function stats
stats <- function(name) {
  if (is.null(name)){
    full_url <- paste0("https://statsapi.web.nhl.com/api/v1/teams",
                       "?expand=team.stats")
  }else{full_url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/", name,
                           "/?expand=team.stats")}
    get_stats <- GET(full_url)
    stats_cont <- content(get_stats, "text", encoding = "UTF-8")
    stats_json <- fromJSON(stats_cont, flatten = TRUE)
    stats_list <- as_tibble(stats_json$teams)
  return(stats_list)
}

#try to use function stats
stats(NULL)
```

    ## # A tibble: 32 x 28
    ##       id name  link  abbreviation teamName locationName firstYearOfPlay teamStats shortName officialSiteUrl franchiseId active
    ##    <int> <chr> <chr> <chr>        <chr>    <chr>        <chr>           <list>    <chr>     <chr>                 <int> <lgl> 
    ##  1     1 New … /api… NJD          Devils   New Jersey   1982            <df [1 ×… New Jers… http://www.new…          23 TRUE  
    ##  2     2 New … /api… NYI          Islande… New York     1972            <df [1 ×… NY Islan… http://www.new…          22 TRUE  
    ##  3     3 New … /api… NYR          Rangers  New York     1926            <df [1 ×… NY Range… http://www.new…          10 TRUE  
    ##  4     4 Phil… /api… PHI          Flyers   Philadelphia 1967            <df [1 ×… Philadel… http://www.phi…          16 TRUE  
    ##  5     5 Pitt… /api… PIT          Penguins Pittsburgh   1967            <df [1 ×… Pittsbur… http://pittsbu…          17 TRUE  
    ##  6     6 Bost… /api… BOS          Bruins   Boston       1924            <df [1 ×… Boston    http://www.bos…           6 TRUE  
    ##  7     7 Buff… /api… BUF          Sabres   Buffalo      1970            <df [1 ×… Buffalo   http://www.sab…          19 TRUE  
    ##  8     8 Mont… /api… MTL          Canadie… Montréal     1909            <df [1 ×… Montréal  http://www.can…           1 TRUE  
    ##  9     9 Otta… /api… OTT          Senators Ottawa       1990            <df [1 ×… Ottawa    http://www.ott…          30 TRUE  
    ## 10    10 Toro… /api… TOR          Maple L… Toronto      1917            <df [1 ×… Toronto   http://www.map…           5 TRUE  
    ## # … with 22 more rows, and 16 more variables: venue.name <chr>, venue.link <chr>, venue.city <chr>, venue.id <int>,
    ## #   venue.timeZone.id <chr>, venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>, division.name <chr>,
    ## #   division.link <chr>, conference.id <int>, conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

### Wrapper function to call other functions

``` r
#create wrapper function to call other functions
wrapper <- function(fun, name){
  if (fun == "franchise"){
    return(franchise(name))
  }else if (fun == "team_totals"){
    return(team_totals(name))
  }else if (fun == "season"){
    return(season(name))
  }else if (fun == "goalie"){
    return(goalie(name))
  }else if (fun == "skater"){
    return(skater(name))
  }else if (fun == "detail"){
    return(detail(name))
  }else if (fun == "stats"){
    return(stats(name))
  }else{
    stop("Type the correct function!")
  }
}

#try to use function wrapper
wrapper("stats", NULL)
```

    ## # A tibble: 32 x 28
    ##       id name  link  abbreviation teamName locationName firstYearOfPlay teamStats shortName officialSiteUrl franchiseId active
    ##    <int> <chr> <chr> <chr>        <chr>    <chr>        <chr>           <list>    <chr>     <chr>                 <int> <lgl> 
    ##  1     1 New … /api… NJD          Devils   New Jersey   1982            <df [1 ×… New Jers… http://www.new…          23 TRUE  
    ##  2     2 New … /api… NYI          Islande… New York     1972            <df [1 ×… NY Islan… http://www.new…          22 TRUE  
    ##  3     3 New … /api… NYR          Rangers  New York     1926            <df [1 ×… NY Range… http://www.new…          10 TRUE  
    ##  4     4 Phil… /api… PHI          Flyers   Philadelphia 1967            <df [1 ×… Philadel… http://www.phi…          16 TRUE  
    ##  5     5 Pitt… /api… PIT          Penguins Pittsburgh   1967            <df [1 ×… Pittsbur… http://pittsbu…          17 TRUE  
    ##  6     6 Bost… /api… BOS          Bruins   Boston       1924            <df [1 ×… Boston    http://www.bos…           6 TRUE  
    ##  7     7 Buff… /api… BUF          Sabres   Buffalo      1970            <df [1 ×… Buffalo   http://www.sab…          19 TRUE  
    ##  8     8 Mont… /api… MTL          Canadie… Montréal     1909            <df [1 ×… Montréal  http://www.can…           1 TRUE  
    ##  9     9 Otta… /api… OTT          Senators Ottawa       1990            <df [1 ×… Ottawa    http://www.ott…          30 TRUE  
    ## 10    10 Toro… /api… TOR          Maple L… Toronto      1917            <df [1 ×… Toronto   http://www.map…           5 TRUE  
    ## # … with 22 more rows, and 16 more variables: venue.name <chr>, venue.link <chr>, venue.city <chr>, venue.id <int>,
    ## #   venue.timeZone.id <chr>, venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>, division.name <chr>,
    ## #   division.link <chr>, conference.id <int>, conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

``` r
wrapper("franchise", NULL)
```

    ## # A tibble: 39 x 8
    ##       id firstSeasonId fullName             lastSeasonId mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ##    <int>         <int> <chr>                       <int>            <int> <chr>      <chr>          <chr>        
    ##  1     1      19171918 Montréal Canadiens             NA                8 MTL        Canadiens      Montréal     
    ##  2     2      19171918 Montreal Wanderers       19171918               41 MWN        Wanderers      Montreal     
    ##  3     3      19171918 St. Louis Eagles         19341935               45 SLE        Eagles         St. Louis    
    ##  4     4      19191920 Hamilton Tigers          19241925               37 HAM        Tigers         Hamilton     
    ##  5     5      19171918 Toronto Maple Leafs            NA               10 TOR        Maple Leafs    Toronto      
    ##  6     6      19241925 Boston Bruins                  NA                6 BOS        Bruins         Boston       
    ##  7     7      19241925 Montreal Maroons         19371938               43 MMR        Maroons        Montreal     
    ##  8     8      19251926 Brooklyn Americans       19411942               51 BRK        Americans      Brooklyn     
    ##  9     9      19251926 Philadelphia Quakers     19301931               39 QUA        Quakers        Philadelphia 
    ## 10    10      19261927 New York Rangers               NA                3 NYR        Rangers        New York     
    ## # … with 29 more rows

### Basic exploratory data analysis

#### Combine data from two endpoints

``` r
#create new data frame goalie_all
goalie_all <- goalie(NULL)[ , c(1:8)]
goalie_all <- goalie_all %>% mutate(Type = "goalie")
#view the data frame
goalie_all
```

    ## # A tibble: 1,078 x 9
    ##       id activePlayer firstName franchiseId franchiseName       gameTypeId gamesPlayed lastName Type  
    ##    <int> <lgl>        <chr>           <int> <chr>                    <int>       <int> <chr>    <chr> 
    ##  1   235 FALSE        Don                15 Dallas Stars                 2         315 Beaupre  goalie
    ##  2   236 FALSE        Bob                28 Arizona Coyotes              2         281 Essensa  goalie
    ##  3   237 FALSE        Tony               11 Chicago Blackhawks           2         873 Esposito goalie
    ##  4   238 FALSE        Grant              25 Edmonton Oilers              2         423 Fuhr     goalie
    ##  5   239 FALSE        Ron                16 Philadelphia Flyers          2         489 Hextall  goalie
    ##  6   240 FALSE        Curtis             18 St. Louis Blues              2         280 Joseph   goalie
    ##  7   241 FALSE        Olie               24 Washington Capitals          2         711 Kolzig   goalie
    ##  8   242 FALSE        Mike               18 St. Louis Blues              2         347 Liut     goalie
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks            2         516 McLean   goalie
    ## 10   244 FALSE        Gilles             13 Cleveland Barons             2         250 Meloche  goalie
    ## # … with 1,068 more rows

``` r
#create new data frame skater_all
skater_all <-skater(NULL)[, c(1:2, 4:8, 10)]
skater_all <- skater_all %>% mutate(Type = "skater")
#view the data frame
skater_all
```

    ## # A tibble: 17,209 x 9
    ##       id activePlayer firstName franchiseId franchiseName        gameTypeId gamesPlayed lastName  Type  
    ##    <int> <lgl>        <chr>           <int> <chr>                     <int>       <int> <chr>     <chr> 
    ##  1 16888 FALSE        George              5 Toronto Maple Leafs           2        1188 Armstrong skater
    ##  2 16889 FALSE        Billy               2 Montreal Wanderers            2           2 Bell      skater
    ##  3 16890 FALSE        Johnny              6 Boston Bruins                 2        1436 Bucyk     skater
    ##  4 16891 FALSE        Jean                1 Montréal Canadiens            2        1125 Beliveau  skater
    ##  5 16892 FALSE        Ray                 6 Boston Bruins                 2        1518 Bourque   skater
    ##  6 16893 FALSE        Harold              9 Philadelphia Quakers          2         216 Darragh   skater
    ##  7 16894 FALSE        Herb                9 Philadelphia Quakers          2         216 Drury     skater
    ##  8 16895 FALSE        Bobby              16 Philadelphia Flyers           2        1144 Clarke    skater
    ##  9 16896 FALSE        Ken                23 New Jersey Devils             2        1283 Daneyko   skater
    ## 10 16897 FALSE        Gerry               2 Montreal Wanderers            2           4 Geran     skater
    ## # … with 17,199 more rows

``` r
#create new data frame goalie_skater_all
goalie_skater_all <- rbind(goalie_all, skater_all)
goalie_skater_all <- goalie_skater_all[order(goalie_skater_all$id), ]
#view the data frame
goalie_skater_all
```

    ## # A tibble: 18,287 x 9
    ##       id activePlayer firstName franchiseId franchiseName       gameTypeId gamesPlayed lastName Type  
    ##    <int> <lgl>        <chr>           <int> <chr>                    <int>       <int> <chr>    <chr> 
    ##  1   235 FALSE        Don                15 Dallas Stars                 2         315 Beaupre  goalie
    ##  2   236 FALSE        Bob                28 Arizona Coyotes              2         281 Essensa  goalie
    ##  3   237 FALSE        Tony               11 Chicago Blackhawks           2         873 Esposito goalie
    ##  4   238 FALSE        Grant              25 Edmonton Oilers              2         423 Fuhr     goalie
    ##  5   239 FALSE        Ron                16 Philadelphia Flyers          2         489 Hextall  goalie
    ##  6   240 FALSE        Curtis             18 St. Louis Blues              2         280 Joseph   goalie
    ##  7   241 FALSE        Olie               24 Washington Capitals          2         711 Kolzig   goalie
    ##  8   242 FALSE        Mike               18 St. Louis Blues              2         347 Liut     goalie
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks            2         516 McLean   goalie
    ## 10   244 FALSE        Gilles             13 Cleveland Barons             2         250 Meloche  goalie
    ## # … with 18,277 more rows

#### Create at least two new variables

Here we create three new variables loss\_rate, win\_rate, and tie\_rate

``` r
#create new data frame goalie_all
goalie_all <- goalie(NULL)[ , c(1:9, 28:29)]
#view the data frame
goalie_all
```

    ## # A tibble: 1,078 x 11
    ##       id activePlayer firstName franchiseId franchiseName       gameTypeId gamesPlayed lastName losses  ties  wins
    ##    <int> <lgl>        <chr>           <int> <chr>                    <int>       <int> <chr>     <int> <int> <int>
    ##  1   235 FALSE        Don                15 Dallas Stars                 2         315 Beaupre     125    45   126
    ##  2   236 FALSE        Bob                28 Arizona Coyotes              2         281 Essensa     114    32   116
    ##  3   237 FALSE        Tony               11 Chicago Blackhawks           2         873 Esposito    302   148   418
    ##  4   238 FALSE        Grant              25 Edmonton Oilers              2         423 Fuhr        117    54   226
    ##  5   239 FALSE        Ron                16 Philadelphia Flyers          2         489 Hextall     172    58   240
    ##  6   240 FALSE        Curtis             18 St. Louis Blues              2         280 Joseph       96    34   137
    ##  7   241 FALSE        Olie               24 Washington Capitals          2         711 Kolzig      293    63   301
    ##  8   242 FALSE        Mike               18 St. Louis Blues              2         347 Liut        133    52   151
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks            2         516 McLean      228    62   211
    ## 10   244 FALSE        Gilles             13 Cleveland Barons             2         250 Meloche     140    48    58
    ## # … with 1,068 more rows

``` r
#create three new variables loss_rate, win_rate, and tie_rate
goalie_all <- goalie_all %>% mutate(loss_rate = goalie_all$losses/goalie_all$gamesPlayed, 
                                    win_rate = goalie_all$wins/goalie_all$gamesPlayed, 
                                    tie_rate = goalie_all$ties/goalie_all$gamesPlayed)
#view the data frame
goalie_all
```

    ## # A tibble: 1,078 x 14
    ##       id activePlayer firstName franchiseId franchiseName       gameTypeId gamesPlayed lastName losses  ties  wins loss_rate
    ##    <int> <lgl>        <chr>           <int> <chr>                    <int>       <int> <chr>     <int> <int> <int>     <dbl>
    ##  1   235 FALSE        Don                15 Dallas Stars                 2         315 Beaupre     125    45   126     0.397
    ##  2   236 FALSE        Bob                28 Arizona Coyotes              2         281 Essensa     114    32   116     0.406
    ##  3   237 FALSE        Tony               11 Chicago Blackhawks           2         873 Esposito    302   148   418     0.346
    ##  4   238 FALSE        Grant              25 Edmonton Oilers              2         423 Fuhr        117    54   226     0.277
    ##  5   239 FALSE        Ron                16 Philadelphia Flyers          2         489 Hextall     172    58   240     0.352
    ##  6   240 FALSE        Curtis             18 St. Louis Blues              2         280 Joseph       96    34   137     0.343
    ##  7   241 FALSE        Olie               24 Washington Capitals          2         711 Kolzig      293    63   301     0.412
    ##  8   242 FALSE        Mike               18 St. Louis Blues              2         347 Liut        133    52   151     0.383
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks            2         516 McLean      228    62   211     0.442
    ## 10   244 FALSE        Gilles             13 Cleveland Barons             2         250 Meloche     140    48    58     0.56 
    ## # … with 1,068 more rows, and 2 more variables: win_rate <dbl>, tie_rate <dbl>

#### Create contingency tables

Table 1 below shows the relationship between the type of player and the
number of active players. As we can see from the table, there are more
non-active players than active players. This is true for both goalies
and skaters.

In the second table, we can find the number of teams for each
combination of division and time zone. We can see from the table that
teams in division Honda West and in division Scotia North are from four
different time zones, while teams in division MassMutual East fall into
only one time zone EDT.

``` r
#create table to summarize the relationship between player type and the number of active players
table(goalie_skater_all$Type, goalie_skater_all$activePlayer)
```

    ##         
    ##          FALSE  TRUE
    ##   goalie   929   149
    ##   skater 15242  1967

``` r
#create table to summarize the relationship between divisions and timeZones
table(stats(NULL)$division.name, stats(NULL)$venue.timeZone.tz)
```

    ##                   
    ##                    CDT EDT MDT MST PDT
    ##   Discover Central   3   5   0   0   0
    ##   Honda West         2   0   1   1   4
    ##   MassMutual East    0   8   0   0   0
    ##   Scotia North       1   3   2   0   1

#### Create numerical summaries

``` r
#quantiles and mean for wins in regular season
totals_regular <- team_totals(NULL) %>% filter(gameTypeId ==2) 
summary(totals_regular$wins)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1     113     678     929    1497    3473

``` r
#quantiles and mean for games played
goalie_skater_all %>% group_by(Type) %>% summarize(min = min(gamesPlayed), 
                                                   avg = round(mean(gamesPlayed),0), 
                                                   med = median(gamesPlayed), 
                                                   max = max(gamesPlayed))
```

    ## # A tibble: 2 x 5
    ##   Type     min   avg   med   max
    ##   <chr>  <int> <dbl> <dbl> <int>
    ## 1 goalie     1    89    38  1259
    ## 2 skater     1   117    54  1687

#### Creates plots

The bar plot below compares the number of players for each player type.
As we can see, non-active skaters are the most common, while active
goalies are the least common.

``` r
#create bar plot to compare number of players for each player type
g <- ggplot(goalie_skater_all, aes(x = Type))
g + geom_bar(aes(fill = as.factor(activePlayer)), position = "dodge") + 
  ggtitle("Numer of Players for Each Player Type") + 
  labs(x = "Player Type") + 
  scale_fill_discrete(name = "Active player")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Below is a histogram of the distribution of numerical variable wins. The
plot shows that the probability of winning decreases as the number of
wins increases. In other words, it is harder to win more games.

``` r
#create histogram to present the distribution of numerical variable wins.
g <- ggplot(goalie_all, aes(x = wins, ..density..))
g + geom_histogram(bins = 150) +
  labs(x = "Wins", y = "Density") +
  ggtitle("Histogram for Wins") +
  geom_density(col = "red", lwd = 3, adjust = 0.4)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Below is a boxplot showing the number of wins for each conference. The
Eastern conference has more wins.

``` r
#create a new dataset conf_totals
conference <- stats(NULL)[ , c("id", "conference.name")]
totals <- team_totals(NULL) %>% 
  filter(gameTypeId == 2, teamId %in% c(1:26, 28:30, 52:55)) %>% 
  select(c(14, 26, 27, 30))
totals <- totals %>% rename(id = teamId)
conference <- conference %>% rename(Conference = conference.name)
conf_totals<- merge(totals, conference, by = "id")
#view the dataset conf_totals
conf_totals
```

    ##    id losses              teamName wins Conference
    ## 1   1   1211     New Jersey Devils 1394    Eastern
    ## 2   2   1587    New York Islanders 1688    Eastern
    ## 3   3   2716      New York Rangers 2883    Eastern
    ## 4   4   1452   Philadelphia Flyers 2079    Eastern
    ## 5   5   1734   Pittsburgh Penguins 1903    Eastern
    ## 6   6   2403         Boston Bruins 3241    Eastern
    ## 7   7   1564        Buffalo Sabres 1805    Eastern
    ## 8   8   2302    Montréal Canadiens 3473    Eastern
    ## 9   9    940       Ottawa Senators  971    Eastern
    ## 10 10   2696   Toronto Maple Leafs 2873    Eastern
    ## 11 12    725   Carolina Hurricanes  827    Eastern
    ## 12 13    870      Florida Panthers  889    Eastern
    ## 13 14    947   Tampa Bay Lightning  985    Eastern
    ## 14 15   1467   Washington Capitals 1700    Eastern
    ## 15 16   2761    Chicago Blackhawks 2812    Western
    ## 16 17   2446     Detroit Red Wings 2891    Eastern
    ## 17 18    656   Nashville Predators  852    Western
    ## 18 19   1645       St. Louis Blues 1929    Western
    ## 19 20   1236        Calgary Flames 1497    Western
    ## 20 21    728    Colorado Avalanche 1007    Western
    ## 21 22   1337       Edmonton Oilers 1469    Western
    ## 22 23   1746     Vancouver Canucks 1649    Western
    ## 23 24    834         Anaheim Ducks  990    Western
    ## 24 25    738          Dallas Stars 1084    Western
    ## 25 26   1829     Los Angeles Kings 1754    Western
    ## 26 28    920       San Jose Sharks 1070    Western
    ## 27 29    698 Columbus Blue Jackets  678    Eastern
    ## 28 30    599        Minnesota Wild  759    Western
    ## 29 52    292         Winnipeg Jets  382    Western
    ## 30 53    262       Arizona Coyotes  214    Western
    ## 31 54     94  Vegas Golden Knights  173    Western

``` r
#create boxplot for number of wins for each conference
g <- ggplot(conf_totals, aes(x = Conference, y = wins)) 
g + geom_boxplot() + 
  geom_jitter(aes(color = Conference)) + 
  ggtitle("Boxplot for Wins") +
  labs(x = "Conference", y = "Wins")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The scatterplot below illustrates how wins and losses are related. We
can see that more wins are accompanied by more losses. This is true for
both conferences.

``` r
#create scatter plot to show the relationship between wins and losses
g <- ggplot(conf_totals, aes(x = wins, y = losses, group = Conference)) 
g+ geom_point(aes(color = Conference), position = "jitter") + 
   geom_smooth(aes(group = Conference), method = lm, formula = y ~ x, col = "green") + 
   ggtitle("Wins vs Losses")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
