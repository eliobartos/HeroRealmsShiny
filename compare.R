library(dplyr)
source("priors.R")
#For players returns IDs of matches played with specific classes
get_ids = function(data, players, classes = 'All') {
  
  if(length(classes) == 1 && classes == 'All') {
    out = data %>% 
      filter(name %in% players) %>% 
      select(id) %>% 
      unlist()
  } else {
    out = data %>% 
      filter(name %in% players, class %in% classes) %>% 
      select(id) %>% 
      unlist()
  }
  return(unique(out))
}

# Get all matches for player played with classes
get_match_data_for_player = function(data, player, classes = "All") {
  data %>% 
    filter(id %in% get_ids(data, player, classes))
}

# After we have one player of insterest filter opponents
filter_vs = function(data, vs_players, vs_classes) {
  data %>% 
    filter(id %in% get_ids(data, vs_players, vs_classes))
}

# Filter data for specific player and classes and then get this
# type = c("my class", "vs_players", "vs_classes")
get_compare_table = function(data, player, type = "my_class") {
  
  if(type == "my_class") {
    data = data %>% 
      filter(name == player) 
  } else {
    data = data %>% 
      filter(name != player)
  } 
  
  if(type == "vs_players") {
    group_var = "name"
  } else {
    group_var = "class"
  }

  if(type == "my_class") {
    out = data %>% 
      group_by_(group_var) %>% 
      summarise(
        played = n(),
        win = sum(winner),
        win_rate = win/played,
        post_alpha = win + priors$compare$alpha,
        post_beta = played - win + priors$compare$beta,
        bayes_win_rate = post_alpha/(post_alpha + post_beta)
      )  
  } else {
    out = data %>% 
      group_by_(group_var) %>% 
      summarise(
        played = n(),
        win = played - sum(winner),
        win_rate = win/played,
        post_alpha = win + priors$compare$alpha,
        post_beta = played - win + priors$compare$beta,
        bayes_win_rate = post_alpha/(post_alpha + post_beta)
      )
  }
  
  if(group_var == 'name') {
    out = out %>% 
      select(name, played, win, win_rate, bayes_win_rate)
    
    colnames(out) = c("Name", "Played", "Won", "Win Rate", "Bayes Win Rate")  
  } else {
    out = out %>% 
      select(class, played, win, win_rate, bayes_win_rate)
    
    colnames(out) = c("Class", "Played", "Won", "Win Rate", "Bayes Win Rate")  
  }
  out = out %>% 
    arrange(desc(`Bayes Win Rate`))
  return(out)
}


get_top_pick = function(data) {
  out = data %>% 
    group_by(class) %>% 
    summarise(played = n(),
              won = sum(winner)) %>% 
    arrange(desc(played), desc(won)) %>% 
    select(class) %>% 
    unlist()
  
  if(length(out) >= 1) {
    return(out[[1]])
  } else {
    return(NULL)
  }
}
