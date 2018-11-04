# Format tables on compare tab
format_compare_data = function(data) {
  formattable(data, 
              align = c('l', 'r', 'r', 'c', 'c', 'c'),
              list(`Win Rate` = percent,
                   `Bayes Win Rate` =my_color_tile("transparent", "coral"),
                   `Played` = color_bar("orange"))
  )
}

# Format tables on overview tab
format_overall_data = function(data) {
  formattable(data, 
              align = c('l', 'r', 'r', 'c', 'c', 'c'),
              list(`Win Rate` = percent,
                   `Bayes Win Rate` = my_color_tile("transparent", "coral"),
                   `Is Best %` = my_color_tile("transparent", "skyblue"),
                   `Played` = color_bar("orange"))
  )
}


# Custom formatting functons ----------------------------------------------

# Colorbar with percentage
my_colorbar <- function(color = "lightgray", fun = "comma", digits = 0) {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) style(
              display = "inline-block",
              direction = "rtl",
              "border-radius" = "4px",
              "padding-right" = "2px",
              "background-color" = csscolor(color),
              width = percent(proportion(as.numeric(y), na.rm = TRUE))
            )
  )
}

# Color tile with percentage
my_color_tile = function(...) {
  formatter("span", x ~ percent(x, digits = 2), 
            style = function(x) style(display = "block",
                                      padding = "0 4px", 
                                      `border-radius` = "4px", 
                                      `background-color` = csscolor(gradient(as.numeric(x),...))
                                      )
            )
}
