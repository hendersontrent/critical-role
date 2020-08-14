
# Define server function

shinyServer <- function(input, output, session) {
  
  #------------------------Move from landing page to additional tabs--------------  
  observe({
    if(input$button_one == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab1)
    })
  })
  
  observe({
    if(input$button_two == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab2)
    })
  })
  
  observe({
    if(input$button_three == 0){
      return()
    }
    isolate({
      updateTabsetPanel(session, "page_tab", select = navtab3)
    })
  })
  
  #------------------------DATA PREP---------------------------------------
  
  damage_clean <- cleaner(damage) %>%
    rename(damage = value)
  
  healing_clean <- cleaner(healing) %>%
    rename(healing = value)
  
  dam_heals <- damage_clean %>%
    left_join(healing_clean, by = c("episode" = "episode", "character" = "character"))
  
  # Aggregate to episode sums
  
  raw_data <- dam_heals %>%
    gather(key = variable, value = value, c(damage, healing)) %>%
    mutate(variable = str_to_sentence(variable)) %>%
    group_by(episode, variable) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(variable) %>%
    mutate(n = n(), sd = sd(value)) %>%
    ungroup()
  
  #------------------------ANALYSIS TAB------------------------------------
  
  # XX
  
  #------------------------STATE SPACE TAB---------------------------------
  
  full_models <- eventReactive(input$go_ss, {
    
    the_vars <- unique(raw_data$variable)
    
    some_list <- list()
    
    for(i in the_vars){
      
      shorter <- raw_data %>%
        filter(variable == i)
      
      d1 <- list(
        mu_start = first(shorter$value),
        n_eps = nrow(shorter),
        y_values = shorter$value,
        sigma = unique(shorter$sd)
      )
      
      mod <- stan(model_code = ss_mod_spec, data = d1, iter = 4000, control = list(max_treedepth = 20))
      
      ex <- as.data.frame(rstan::extract(mod, "mu"))
      
      outs <- ex %>%
        gather(key = episode, value = value, 1:105) %>%
        mutate(episode = gsub("mu.", "\\1", episode)) %>%
        mutate(episode = as.numeric(episode)) %>%
        group_by(episode) %>%
        summarise(mean = mean(value),
                  upper = quantile(value, 0.975),
                  lower = quantile(value, 0.025)) %>%
        ungroup() %>%
        mutate(variable = i)
      
      some_list[[i]] <- outs
      
    }
    
    full_models <- rbindlist(some_list, use.names = TRUE)
    
    return(full_models)
  })
  
  output$ss_model <- renderPlot({
    
    validate(
      need(full_models(), "Press 'Run Model'."
      )
    )
    
    p <- raw_data %>%
      ggplot(aes(x = episode)) +
      geom_ribbon(data = full_models(), 
                  aes(x = episode, ymin = lower, ymax = upper, fill = variable), alpha = 0.4) +
      geom_line(data = full_models(), 
                aes(x = episode, y = mean, colour = variable), size = 1.1) +
      geom_point(data = raw_data, aes(x = episode, y = value), size = 2, colour = "black") +
      labs(x = "Episode",
           y = "Episode Sum Value") +
      theme_bw() +
      scale_colour_manual(values = ss_palette) +
      scale_fill_manual(values = ss_palette) +
      guides(fill = FALSE) +
      theme(legend.position = "none",
            panel.grid.minor =  element_blank(),
            strip.background = element_rect(fill = "#E7F2F8"),
            strip.text = element_text(face = "bold", colour = "black"),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()) +
      facet_grid(variable ~.)
    print(p)
  })
  
}
