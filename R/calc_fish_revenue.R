#' computes most frequently caught fish, state revenue from fisheries, total fisheries revenue & plots revenue per state

#' @param fish_caught 
#'
#' @param fish_prices 
#' @param plot 
#'
#' @return lists of answers and plot

calc_fish_revenue = function(fish_caught, fish_prices, plot) {
  # returns names of location and most frequently caguht fish:
  most_fish = list(colnames(fish_caught), rownames(fish_caught)[apply(fish_caught, 2, which.max)])
  # calculates total revenue for each state
  state_rev = colSums(fish_prices$prices * fish_caught)
  # total fisheries sum:
  total_rev = sum(fish_prices$prices * fish_caught)
  # check that all fish species in the catch table match a price in the price table
  #make data frame so can plot
  state_rev_df = data.frame(state_rev) %>% 
    rownames_to_column(var = "state") # turn row names into a column to make it easier to plot
  # plot if the user says plot = TRUE
  if (plot) {
    # write out the total revenue:
    plottitle = sprintf("The total fishery revenue is %d dollars", total_rev)
    
    graph = ggplot(state_rev_df, aes(x = state, y = state_rev, fill = state)) + 
    geom_col() +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = plottitle,
         x = "Locations Caught",
         y = "Total Revenus ($)") +
    theme_classic()
  
  
  }
  
  return(list(mostfrequent = most_fish, 
              Total_revenue = total_rev,
              State_revenue= state_rev,
              Total_rev_print = print,
              plot = graph))
  
}
