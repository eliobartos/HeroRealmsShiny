library(dplyr)

# Function validates data and then writes it on dropbox
validate_data = function(p1_name, p1_class, p2_name, p2_class, winner, password) {
  
  success_text = "Game added. Thank you!"
  
  if(password != 'Gricka') {
    notif_text = "Incorrect password!"
  } else if (p1_name == "") {
    notif_text = "Enter Player 1 name!"
  } else if (p1_class == "") {
    notif_text = "Enter Player 1 class!"
  } else if (p2_name == "") {
    notif_text = "Enter Player 2 name!"
  } else if (p2_class == "") {
    notif_text = "Enter Player 2 class!"
  } else if (winner == "") {
    notif_text = "Enter winner!"
  } else if (p1_name == p2_name) {
    notif_text = "Player names can't be the same!"
  } else if (p1_class == p2_class) {
    notif_text = "Players classes can't be the same!"
  } else if (winner != p1_name && winner != p2_name) {
    notif_text = "Winner must be one of two players!"
  } else {
    notif_text = success_text
  }
  
  if(notif_text != success_text) {
    showNotification(notif_text)  
  } else {
    showNotification("Adding game, please wait...")
    data = create_data(p1_name, p1_class, p2_name, p2_class, winner)
    
    # Save file to dropbox
    # saveDataToDrop(data)
    #Insert rows into mysql database
    db_insert_into(pool, "hero_realms_data", values = data)
    
    showNotification(notif_text,
                     action = a(href = "javascript:location.reload();", "Reload page"))  
  }
  
}

# From input data creates structured data we want to save
create_data = function(p1_name, p1_class, p2_name, p2_class, winner) {
  data_for_hash = data.frame(p1_name = p1_name,
                             p1_class = p1_class,
                             p2_name = p2_name,
                             p2_class = p2_class,
                             winner = winner)
  
  match_id = digest::digest(data_for_hash)
  
  data = data.frame(
    time = rep(Sys.time(), 2),
    id = rep(match_id, 2),
    name = c(p1_name, p2_name),
    class = c(p1_class, p2_class),
    winner = as.integer(c(p1_name == winner, p2_name == winner))
  )
  
  return(data)
}



priors = list()
priors$overall$alpha = 10
priors$overall$beta = 10


get_overall = function(data_all, group_var = "name") {
  
  overall_data = data_all %>% 
    group_by_(group_var) %>% 
    summarise(played = n(),
              win = sum(winner),
              win_rate = win/played,
              post_alpha = win + priors$overall$alpha,
              post_beta = played - win + priors$overall$beta,
              bayes_win_rate = post_alpha/(post_alpha + post_beta)) %>% 
    arrange(desc(bayes_win_rate))
  
  samples = list()
  for(i in 1:nrow(overall_data)) {
    samples[[overall_data[[group_var]][[i]]]] = rbeta(10000, overall_data$post_alpha[[i]], overall_data$post_beta[[i]]) 
  }
  samples = data.frame(samples)
  samples$max = apply(samples, 1, which.max)

  res = samples %>% 
    group_by(max) %>% 
    summarise(n = n()) %>% 
    mutate(prob = n/sum(n))
  
  overall_data$prob_best = res$prob  
  
  if(group_var == 'name') {
    overall_data = overall_data %>% 
      select(name, played, win, win_rate, bayes_win_rate, prob_best)
    
    colnames(overall_data) = c("Name", "Played", "Won", "Win Rate", "Bayes Win Rate", "Is Best %")  
  } else {
    overall_data = overall_data %>% 
      select(class, played, win, win_rate, bayes_win_rate, prob_best)
    
    colnames(overall_data) = c("Class", "Played", "Won", "Win Rate", "Bayes Win Rate", "Is Best %")  
  }
  
  return(overall_data)
}


