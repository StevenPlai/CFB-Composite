change_pos <- function(x) glue("+{x}")
change_neg <- function(x) "-x"
no_change <- function(x) "x"
CFBWeek <- function() {
  date <- Sys.time()
  month <- as.numeric(date %>% stri_sub(6,7))
  mday <- as.numeric(date %>% stri_sub(9,10))
  hour <- as.numeric(date %>% stri_sub(12,13))
  if(month<9){0} else{
    if(month==9){
      if(mday<5){1} else{
        if(mday==5){
          if(hour>3){2}else{1}}else{
            if(mday<12){2} else{
              if(mday==12){
                if(hour>3){3}else{2}}else{
                  if(mday<19){3} else{
                    if(mday==19){
                      if(hour>3){4}else{3}}else{
                        if(mday<26){4} else{
                          if(mday==26){
                            if(hour>3){5}else{4}}else{5}}}}}}}}}else{
                              if(month==10){
                                if(mday<3){5} else{
                                  if(mday==3){
                                    if(hour>3){6}else{5}}else{
                                      if(mday<10){6} else{
                                        if(mday==10){
                                          if(hour>3){7}else{6}}else{
                                            if(mday<17){7} else{
                                              if(mday==17){
                                                if(hour>3){8}else{7}}else{
                                                  if(mday<24){8} else{
                                                    if(mday==24){
                                                      if(hour>3){9}else{8}}else{
                                                        if(mday<31){9}else{
                                                          if(mday==31){
                                                            if(hour>3){10}else{9}}else{10}}}}}}}}}}}else{
                                                              if(month==11){
                                                                if(mday<7){10}else{
                                                                  if(mday==7){
                                                                    if(hour>3){11}else{10}}else{
                                                                      if(mday<14){11}else{
                                                                        if(mday==14){
                                                                          if(hour>3){12}else{11}}else{
                                                                            if(mday<21){12}else{
                                                                              if(mday==21){
                                                                                if(hour>3){13}else{12}}else{
                                                                                  if(mday<28){13}else{
                                                                                    if(mday==28){
                                                                                      if(hour>3){14}else{13}}else{14}}}}}}}}}else{
                                                                                        if(month==12){
                                                                                          if(mday<5){14}else{
                                                                                            if(mday==5){
                                                                                              if(hour>3){15}else{14}}else{
                                                                                                if(mday<12){15}else{
                                                                                                  if(mday<19){16}else{
                                                                                                    if(mday==19){
                                                                                                      if(hour>3){17}else{17}}else{17}}}}}}}}}}}


gt_sparkline <- function( gt_object, column, type = "sparkline",
                          line_color = "black", range_colors = c("red", "blue"),
                          fill_color = "grey", bw = NULL, same_limit = TRUE
) {
  
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  # convert tidyeval column to bare string
  col_bare <- rlang::enexpr(column) %>% rlang::as_string()
  # segment data with bare string column name
  data_in <- gt_object[["_data"]][[col_bare]]
  
  stopifnot("Specified column must contain list of values" = class(data_in) %in% "list")
  stopifnot("You must supply two colors for the max and min values." = length(range_colors) == 2L)
  stopifnot("You must indicate the `type` of plot as one of 'sparkline', 'histogram' or 'density'." = isTRUE(type %in% c("sparkline", "histogram", "density")))
  
  # convert to a single vector
  data_in <- unlist(data_in)
  # range to be used for plotting if same axis
  total_rng <- grDevices::extendrange(data_in, r = range(data_in, na.rm = TRUE), f = 0.02)
  
  plot_fn_spark <- function(x) {
    t <- gsub("[^0-9.-\\,]", "", x)
    t <- gsub("[<=>]", "", t)
    t <- gsub("[//]", "", t)
    vals <- strsplit(t, split = ",") %>%
      unlist() %>% as.double()
    
    max_val <- max(vals, na.rm = TRUE)
    min_val <- min(vals, na.rm = TRUE)
    
    x_max <- vals[vals == max_val]
    x_min <- vals[vals == min_val]
    
    point_data <- dplyr::tibble(
      x = c(
        c(1:length(vals))[vals == min_val],
        c(1:length(vals))[vals == max_val]
      ),
      y = c(x_min, x_max),
      colors = c(rep(range_colors[1], length(x_min)),
                 rep(range_colors[2], length(x_max)))
    )
    
    input_data <- dplyr::tibble(
      x = 1:length(vals),
      y = vals
    )
    
    if (type == "sparkline") {
      plot_base <- ggplot(input_data) +
        theme_void()
      
      if (isTRUE(same_limit)) {
        plot_base <- plot_base +
          scale_y_continuous(expand = expansion(mult = 0.05)) +
          coord_cartesian(clip = "off", ylim = total_rng,
                          xlim = c(0, length(vals) + 0.5))
      } else {
        plot_base <- plot_base +
          scale_y_continuous(expand = expansion(mult = 0.05)) +
          coord_cartesian(clip = "off", ylim = grDevices::extendrange(vals),
                          xlim = c(0, length(vals) + 0.5))
      }
      
      plot_out <- plot_base +
        geom_line(aes(x = x, y = y, group = 1), size = 0.5,
                  color = line_color) +
        geom_point(
          data = point_data,
          aes(x = x, y = y, color = I(colors), group = 1),
          size = 0.5
        )
      
    } else if (type == "histogram") {
      plot_base <- ggplot(input_data) +
        theme_void()
      
      if (isTRUE(same_limit)) {
        
        if(is.null(bw)){
          bw <- 2 * IQR(data_in, na.rm = TRUE) / length(data_in)^(1 / 3)
        } else {
          bw <- bw
        }
        
        plot_out <- plot_base +
          geom_histogram(
            aes(x = y),
            color = line_color,
            fill = fill_color,
            binwidth = bw
          ) +
          scale_x_continuous(expand = expansion(mult = 0.2)) +
          coord_cartesian(
            clip = "off",
            xlim = grDevices::extendrange(
              data_in, r = range(data_in, na.rm = TRUE), f = 0.02))
        
      } else {
        if(is.null(bw)){
          bw <- 2 * IQR(vals, na.rm = TRUE) / length(vals)^(1 / 3)
        } else {
          bw <- bw
        }
        
        plot_out <- plot_base +
          geom_histogram(
            aes(x = y),
            color = line_color,
            fill = fill_color,
            binwidth = bw
          ) +
          coord_cartesian(clip = "off",
                          xlim = grDevices::extendrange(
                            vals, r = range(vals, na.rm = TRUE), f = 0.02
                          ))
      }
    } else if (type == "density") {
      
      if (isTRUE(same_limit)) {
        if(is.null(bw)){
          bw <- bw.nrd0(data_in)
        } else {
          bw <- bw
        }
        
        total_rng <- density(data_in, bw = bw)[["x"]]
        
        density_calc <- density(input_data[["y"]], bw = bw)
        density_range <- density_calc[["x"]]
        
        density_df <- dplyr::tibble(
          x = density_calc[["x"]],
          y = density_calc[["y"]]
        )
        
        plot_base <- ggplot(density_df) +
          theme_void()
        
        
        plot_out <- plot_base +
          geom_area(aes(x = x, y = y),
                    color = line_color,
                    fill = fill_color) +
          xlim(range(density_range)) +
          coord_cartesian(xlim = range(total_rng, na.rm = TRUE),
                          expand = TRUE, clip = "off")
      } else {
        if(is.null(bw)){
          bw <- bw.nrd0(vals)
        } else {
          bw <- bw
        }
        
        total_rng <- density(data_in, bw = bw)[["x"]]
        
        density_calc <- density(input_data[["y"]], bw = bw)
        density_range <- density_calc[["x"]]
        
        density_df <- dplyr::tibble(
          x = density_calc[["x"]],
          y = density_calc[["y"]]
        )
        
        plot_base <- ggplot(density_df) +
          theme_void()
        
        plot_out <- plot_base +
          geom_area(aes(x = x, y = y),
                    color = line_color,
                    fill = fill_color) +
          xlim(range(density_range)) +
          coord_cartesian(xlim = range(total_rng, na.rm = TRUE),
                          expand = TRUE, clip = "off")
      }
    }
    
    out_name <- file.path(
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".svg")
    )
    
    ggsave(
      out_name,
      plot = plot_out,
      dpi = 20,
      height = 0.15,
      width = 0.9
    )
    
    img_plot <- out_name %>%
      readLines() %>%
      paste0(collapse = "") %>%
      gt::html()
    
    on.exit(file.remove(out_name))
    
    img_plot
  }
  
  text_transform(
    gt_object,
    locations = cells_body(columns = {{ column }}),
    fn = function(x) {
      lapply(
        x,
        plot_fn_spark
      )
    }
  )
}
