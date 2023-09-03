library(shiny)
library(tidyverse)
library(ffscrapr)

.espn_week_checkmax <- function(conn) {
  url_query <- glue::glue(
    "https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId=0&view=mSettings"
  )
  
  settings <- espn_getendpoint_raw(conn, url_query)
  
  current_week <- settings %>%
    purrr::pluck("content", "status", "latestScoringPeriod")
  
  final_week <- settings %>%
    purrr::pluck("content", "status", "finalScoringPeriod")
  
  max_week <- min(current_week, final_week, na.rm = TRUE)
  
  return(max_week)
}
.espn_week_starter <- function(week, conn) {
  url_query <- glue::glue(
    "https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId={week}&view=mMatchupScore&view=mBoxscore&view=mSettings&view=mRosterSettings"
  )
  
  week_scores <- espn_getendpoint_raw(conn, url_query) %>%
    purrr::pluck("content", "schedule") %>%
    tibble::tibble() %>%
    purrr::set_names("x") %>%
    tidyr::hoist(1, "week" = "matchupPeriodId", "home", "away") %>%
    dplyr::filter(.data$week == .env$week) %>%
    tidyr::pivot_longer(c("home", "away"), names_to = NULL, values_to = "team") %>%
    tidyr::hoist("team", "starting_lineup" = "rosterForCurrentScoringPeriod", "franchise_id" = "teamId") %>%
    dplyr::select(-"team", -"x") %>%
    tidyr::hoist("starting_lineup", "franchise_score" = "appliedStatTotal", "entries") %>%
    tidyr::unnest_longer("entries") %>%
    tidyr::hoist("entries", "player_id" = "playerId", "lineup_id" = "lineupSlotId", "player_data" = "playerPoolEntry") %>%
    tidyr::hoist("player_data", "player_score" = "appliedStatTotal", "player") %>%
    dplyr::select(-"player_data") %>%
    tidyr::hoist("player",
                 "eligible_lineup_slots" = "eligibleSlots",
                 "player_name" = "fullName",
                 "pos" = "defaultPositionId",
                 "team" = "proTeamId",
    ) %>%
    dplyr::mutate(
      projected_score = ifelse(purrr::map_dbl(.data$player,
                                              ~.x %>%
                                                purrr::pluck("stats",
                                                             2,
                                                             "statSourceId",
                                                             .default = NA_real_)) == 1,
                               purrr::map_dbl(.data$player,
                                              ~.x %>%
                                                purrr::pluck("stats",
                                                             2, # assume stats list col returns actual as first list and projected as second
                                                             "appliedTotal",
                                                             .default = NA_real_) %>%
                                                round(1)),
                               purrr::map_dbl(.data$player,
                                              ~.x %>%
                                                purrr::pluck("stats",
                                                             1, # assume stats list col returns actual as first list and projected as second
                                                             "appliedTotal",
                                                             .default = NA_real_) %>%
                                                round(1))),
      player = NULL)
  
  return(week_scores)
}
.espn_lineupslot_map <- function() {
  c(
    "0" = "QB",
    "1" = "TQB",
    "2" = "RB",
    "3" = "RB/WR",
    "4" = "WR",
    "5" = "WR/TE",
    "6" = "TE",
    "7" = "OP",
    "8" = "DT",
    "9" = "DE",
    "10" = "LB",
    "11" = "DL",
    "12" = "CB",
    "13" = "S",
    "14" = "DB",
    "15" = "DP",
    "16" = "DST",
    "17" = "K",
    "18" = "P",
    "19" = "HC",
    "20" = "BE",
    "21" = "IR",
    "22" = "XYZ",
    "23" = "RB/WR/TE",
    "24" = "ER",
    "25" = "Rookie",
    "QB" = 0,
    "TQB" = 1,
    "RB" = 2,
    "RB/WR" = 3,
    "WR" = 4,
    "WR/TE" = 5,
    "TE" = 6,
    "OP" = 7,
    "DT" = 8,
    "DE" = 9,
    "LB" = 10,
    "DL" = 11,
    "CB" = 12,
    "S" = 13,
    "DB" = 14,
    "DP" = 15,
    "DST" = 16,
    "K" = 17,
    "P" = 18,
    "HC" = 19,
    "BE" = 20,
    "IR" = 21,
    "XYZ" = 22,
    "RB/WR/TE" = 23,
    "ER" = 24,
    "Rookie" = 25
  )
}
.espn_pos_map <- function() {
  c(
    "1" = "QB",
    "2" = "RB",
    "3" = "WR",
    "4" = "TE",
    "5" = "K",
    "7" = "P",
    "9" = "DT",
    "10" = "DE",
    "11" = "LB",
    "12" = "CB",
    "13" = "S",
    "14" = "HC",
    "16" = "DST",
    "QB" = 1,
    "RB" = 2,
    "WR" = 3,
    "TE" = 4,
    "K" = 5,
    "P" = 7,
    "DT" = 9,
    "DE" = 10,
    "LB" = 11,
    "CB" = 12,
    "S" = 13,
    "HC" = 14,
    "DST" = 16
  )
}
.espn_team_map <- function() {
  c(
    "0" = "FA",
    "1" = "ATL",
    "2" = "BUF",
    "3" = "CHI",
    "4" = "CIN",
    "5" = "CLE",
    "6" = "DAL",
    "7" = "DEN",
    "8" = "DET",
    "9" = "GBP",
    "10" = "TEN",
    "11" = "IND",
    "12" = "KCC",
    "13" = "OAK",
    "14" = "LAR",
    "15" = "MIA",
    "16" = "MIN",
    "17" = "NEP",
    "18" = "NOS",
    "19" = "NYG",
    "20" = "NYJ",
    "21" = "PHI",
    "22" = "ARI",
    "23" = "PIT",
    "24" = "LAC",
    "25" = "SFO",
    "26" = "SEA",
    "27" = "TBB",
    "28" = "WAS",
    "29" = "CAR",
    "30" = "JAC",
    "33" = "BAL",
    "34" = "HOU",
    "FA" = "0",
    "ATL" = "1",
    "BUF" = "2",
    "CHI" = "3",
    "CIN" = "4",
    "CLE" = "5",
    "DAL" = "6",
    "DEN" = "7",
    "DET" = "8",
    "GBP" = "9",
    "TEN" = "10",
    "IND" = "11",
    "KCC" = "12",
    "OAK" = "13",
    "LAR" = "14",
    "MIA" = "15",
    "MIN" = "16",
    "NEP" = "17",
    "NOS" = "18",
    "NYG" = "19",
    "NYJ" = "20",
    "PHI" = "21",
    "ARI" = "22",
    "PIT" = "23",
    "LAC" = "24",
    "SFO" = "25",
    "SEA" = "26",
    "TBB" = "27",
    "WAS" = "28",
    "CAR" = "29",
    "JAC" = "30",
    "BAL" = "33",
    "HOU" = "34"
  )
}

ff_starters.espn_conn <- function(conn, weeks = 1:17, ...) {
  if (conn$season < 2018) stop("Starting lineups not available before 2018")
  
  checkmate::assert_numeric(weeks)
  
  max_week <- .espn_week_checkmax(conn)
  
  run_weeks <- weeks[weeks < max_week]
  
  if (length(run_weeks) == 0) {
    warning(
      glue::glue(
        "ESPN league_id {conn$league_id} does not have lineups for ",
        "{conn$season} weeks {paste(min(weeks),max(weeks), sep = '-')}."
      ),
      call. = FALSE
    )
    
    return(NULL)
  }
  
  starters <- purrr::map_dfr(run_weeks, .espn_week_starter, conn) %>%
    dplyr::mutate(
      lineup_slot = .espn_lineupslot_map()[as.character(.data$lineup_id)] %>% unname(),
      pos = .espn_pos_map()[as.character(.data$pos)] %>% unname(),
      team = .espn_team_map()[as.character(.data$team)] %>% unname()
    ) %>%
    dplyr::arrange(.data$week, .data$franchise_id, .data$lineup_id) %>%
    dplyr::left_join(
      ff_franchises(conn) %>% dplyr::select("franchise_id", "franchise_name"),
      by = "franchise_id"
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "week",
      "franchise_id",
      "franchise_name",
      "franchise_score",
      "lineup_slot",
      "player_score",
      "projected_score",
      "player_id",
      "player_name",
      "pos",
      "team",
      "eligible_lineup_slots"
    ))) %>%
    mutate(season = as.numeric(conn$season))
  
  return(starters)
}



# User interface ----
ui <- fluidPage(
  titlePanel("Fantasy Points Over Average"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore how you fared in points over average by adjusting the sliders.",
               br(),
               br(),
               "Use a correct league ID"),
      column(width = 6,
             textInput("id", h4("Enter League ID:")),
             br(),
             h4("Include Seasons:"),
             div(style = "display: flex; align-items: center;",
                 checkboxInput("box_2020", "2020"),
                 div(style = "margin-left: 15px;",
                 conditionalPanel(
                   condition = "input['box_2020']",
                   textInput("weeks_2020", "Number of Weeks", width = "120px")
                 ))
             ),
             div(style = "display: flex; align-items: center;",
                 checkboxInput("box_2021", "2021"),
                div(style = "margin-left: 15px;",
                 conditionalPanel(
                   condition = "input['box_2021']",
                   textInput("weeks_2021", "Number of Weeks", width = "120px")
                 ))
             ),
             div(style = "display: flex; align-items: center;",
                 checkboxInput("box_2022", "2022"),
                 div(style = "margin-left: 15px;",
                 conditionalPanel(
                   condition = "input['box_2022']",
                   textInput("weeks_2022", "Number of Weeks", width = "120px")
                 ))
             ),
             actionButton("go_button", "Go"),
             br(),
             br(),
             sliderInput("range",
                         label = "Include Games:",
                         min = 0, max = 41, value = c(0, 41))
            )
        ),
    mainPanel(plotOutput("map"))
    )
  )



# Server ---------------------------------
server <- function(input, output) {

  observeEvent(input$go_button, {
    
  total_scoring <- data.frame()
  
  if (input$box_2020) {
    league_conn <- espn_connect(season = 2020, league_id = input$id)
    
    withProgress(message = "Loading 2020 Data...", {
      for (i in 1:as.numeric(input$weeks_2020)) {
        
        total_scoring <- total_scoring %>%
          rbind(ff_starters.espn_conn(league_conn, weeks = i))
        
        incProgress(1/as.numeric(input$weeks_2020))
      }
    })
  } 
  
  if (input$box_2021) {
    league_conn <- espn_connect(season = 2021, league_id = input$id)
    
    withProgress(message = "Loading 2021 Data...", {
      for (i in 1:as.numeric(input$weeks_2021)) {
        
        total_scoring <- total_scoring %>%
          rbind(ff_starters.espn_conn(league_conn, weeks = i))
        
        incProgress(1/as.numeric(input$weeks_2021))
      }
    })
  } 
  
  if (input$box_2022) {
    league_conn <- espn_connect(season = 2022, league_id = input$id)
    
    withProgress(message = "Loading 2022 Data...", {
      for (i in 1:as.numeric(input$weeks_2022)) {
        
        total_scoring <- total_scoring %>%
          rbind(ff_starters.espn_conn(league_conn, weeks = i))
        
        incProgress(1/as.numeric(input$weeks_2022))
      }
    })
  } 
  
  teams <- ff_franchises(league_conn) %>%
              select(franchise_id, user_name)
  
  total_scoring <- total_scoring %>%
    select(-eligible_lineup_slots)
    
  output$map <- renderPlot({
    weekly_scoring <- total_scoring %>%
      mutate(projected_score = ifelse(is.na(projected_score), 0, projected_score)) %>%
      group_by(season, week, franchise_id) %>%
      summarise(proj_score = sum(ifelse(lineup_slot != "BE", projected_score, 0)),
                team_score = sum(ifelse(lineup_slot != "BE", player_score, 0))) %>%
      left_join(teams, by = "franchise_id") %>%
      group_by(season, week) %>%
      mutate(average_score = mean(team_score)) %>%
      group_by(franchise_id) %>%
      mutate(ptsOA = team_score - average_score,
             game_num = row_number(),
             cum_ptsOA = cumsum(ptsOA),
             final = last(cum_ptsOA)) %>%
      filter(game_num >= input$range[1], game_num <= input$range[2]) %>%
      mutate(seg_ptsOA = cumsum(ptsOA))

      weekly_scoring %>%
      ggplot(aes(game_num, seg_ptsOA, group = user_name))+
      geom_smooth(aes(color = reorder(user_name, desc(final))), se = FALSE)+
      xlab("Game Number")+ylab("Fantasy Points over Average")+labs(color = "Team Owner")+
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", alpha=0.5)+
      geom_vline(xintercept = weekly_scoring$game_num[which(weekly_scoring$season == 2020)[1]], color = "black", linetype = "dashed", alpha=0.2)+
      geom_vline(xintercept = weekly_scoring$game_num[which(weekly_scoring$season == 2021)[1]], color = "black", linetype = "dashed", alpha=0.2)+
      geom_vline(xintercept = weekly_scoring$game_num[which(weekly_scoring$season == 2022)[1]], color = "black", linetype = "dashed", alpha=0.2)+
      annotate("text", x = weekly_scoring$game_num[which(weekly_scoring$season == 2021)[1]]-.5, y = 475, label = "Start of", col = "grey", size = 4)+
      annotate("text", x = weekly_scoring$game_num[which(weekly_scoring$season == 2021)[1]]-.5, y = 455, label = "2021 Games", col = "grey", size = 4)+
      annotate("text", x = weekly_scoring$game_num[which(weekly_scoring$season == 2022)[1]]-.5, y = 475, label = "Start of", col = "grey", size = 4)+
      annotate("text", x = weekly_scoring$game_num[which(weekly_scoring$season == 2022)[1]]-.5, y = 455, label = "2022 Games", col = "grey", size = 4)
  })
  })
}

# Run app ----
shinyApp(ui, server)
