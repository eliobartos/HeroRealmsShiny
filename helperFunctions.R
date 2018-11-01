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
    saveData(data)
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
    winner = c(p1_name == winner, p2_name == winner)
  )
  
  return(data)
}

