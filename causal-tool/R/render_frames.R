render_frames <- function(dat){
  # calculate the points in each frame for the animation
  # interpolate the points between the frames
  # returns a dataset with identifiers for each frame
  
  # create second frame by summarize the y axis
  frame_two <- dat %>%
    mutate(y_0 = mean(y_0),
           y_1 = mean(y_1)) %>% 
    ungroup()
  
  # create third frame
  frame_three <- frame_two %>% 
    mutate(x = mean(x))  
  
  # interpolate between first and second frames
  one_to_two <- map_df(1:nrow(dat), function(i){
    interp_x <- approx(
      x = c(dat$x[[i]], frame_two$x[[i]]),
      n = 15,
      ties = 'ordered'
    )$y
    
    interp_y0 <- approx(
      x = c(dat$y_0[[i]], frame_two$y_0[[i]]),
      n = 15,
      ties = 'ordered'
    )$y
    
    interp_y1 <- approx(
      x = c(dat$y_1[[i]], frame_two$y_1[[i]]),
      n = 15,
      ties = 'ordered'
    )$y
    
    tibble(x = interp_x, y_0 = interp_y0, y_1 = interp_y1, 
           frame = 1:15, index = i)
  })
  
  # interpolate between second and third frames
  two_to_three <- map_dfr(1:nrow(frame_two), function(i){
    interp_x <- approx(
      x = c(frame_two$x[[i]], frame_three$x[[i]]),
      n = 5,
      ties = 'ordered'
    )$y
    
    interp_y0 <- approx(
      x = c(frame_two$y_0[[i]], frame_three$y_0[[i]]),
      n = 5,
      ties = 'ordered'
    )$y
    
    interp_y1 <- approx(
      x = c(frame_two$y_1[[i]], frame_three$y_1[[i]]),
      n = 5,
      ties = 'ordered'
    )$y
    
    tibble(x = interp_x, y_0 = interp_y0, y_1 = interp_y1, 
           frame = 16:20, index = i)
  })
  
  # combine and add z back in based on the index
  interp_data <- one_to_two %>% 
    bind_rows(two_to_three) %>% 
    left_join(dat %>% select(index, z, Y), by = 'index')
  
  return(interp_data)
}