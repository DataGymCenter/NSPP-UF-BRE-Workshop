###############
sankey_lyy <- function(x, cat_cols, num_col, title = NULL){
  `%>%` <- magrittr::`%>%`
  index <- NULL

  # Error handeling
  if(!is.data.frame(x)){
    stop("The input object is not a valid data.frame")
  } else if(!all(c(cat_cols, num_col) %in% names(x))){
    stop("One or more column names does not matching to the names of the input object")
  } else if(length(num_col) != 1){
    stop("Cannot define more than one numeric column with the num_col argument")
  } else if(length(cat_cols) < 2){
    stop("Must define at least two categorical columns with the cat_cols argument")
  } else if(!is.null(title) && !is.character(title)){
    stop("The title argument is not valid character object")
  }


  map <- function(x, cat_cols){
    unique_cat <- map_df <- NULL
    x <- as.data.frame(x)
    for(i in cat_cols){
      unique_cat <- c(unique_cat, base::unique(x[, i]))
    }

    map_df <- base::data.frame(cat = unique_cat,
                               index = 0:(length(unique_cat) - 1),
                               stringsAsFactors = FALSE)
    return(map_df)
  }

  map <- map(x, cat_cols)
  df <- lapply(1:(base::length(cat_cols) - 1), function(i){
    df <- NULL
    df <- x %>%
      dplyr::group_by_(s = cat_cols[i], t = cat_cols[i + 1]) %>%
      dplyr::summarise_(.dots = stats::setNames(paste("sum(", num_col, ",na.rm = TRUE)", sep = ""), "total")) %>%
      dplyr::left_join(map %>% dplyr::select(s = cat, source = index), by = "s") %>%
      dplyr::left_join(map %>% dplyr::select(t = cat, target = index), by = "t")

    return(df)
  }) %>% dplyr::bind_rows()


  p <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    #valueformat = ".0f",
    textfont = list(color = 'black', size = 25),
    node = list(
      label = map$cat,
      # color = c("blue", "green", "red", "purple", "yellow", "black"),
      pad = 15,
      thickness = 30,
      line = list(
        color = "black",
        width = 0.3

      )
    ),

    link = list(
      source = df$source,
      target = df$target,
      value =  df$total
      #value =  df$data[[1]]$link$value,
    )
  )

  if(!is.null(title)){
    p <- p %>% plotly::layout(title = title)
  }

  return(p)
}
