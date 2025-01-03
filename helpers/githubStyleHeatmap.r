#' Calendar Heatmap
#' 
#' Creates a colour coded calendar visualising time series data
#' 
#' @param dates A vector containing the dates in `Date` format.
#' @param values A vector containing the corresponding values as numeric.
#' @param title Main plot title (optional).
#' @param subtitle Main plot subtitle (optional).
#' @param legendtitle Legend title (optional).
#'   
#' @return ggplot object
calendarHeatmap <- function(dates, values, title = "", subtitle = "", legendtitle = ""){
  
  # Parameter checks
  if(missing(dates)){
    stop("Need to specify a dates vector.")
  }
  if(missing(values)){
    stop("Need to specify a values vector.")
  }
  if(!is.Date(dates)){
    stop("dates vector need to be in Date format.")
  }
  if(length(dates) != length(values)){
    stop("dates and values need to have the same length.")
  }
  
  
  # load required packages
  require(ggplot2)
  
  my_theme <- function() {
    
    # Colors
    color.background = "white"
    color.text = "#22211d"
    
    # Begin construction of chart
    theme_bw(base_size=15) +
      
      # Format background colors
      theme(panel.background = element_rect(fill=color.background, color=color.background)) +
      theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
      theme(panel.border     = element_rect(color=color.background)) +
      theme(strip.background = element_rect(fill=color.background, color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks       = element_blank()) +
      
      # Format the legend
      theme(legend.position = "top") +
      theme(legend.text = element_text(size = 8, color = color.text)) +
      theme(legend.title = element_text(size = 10, face = "bold", color = color.text)) +
      
      # Format title and axis labels
      theme(plot.title       = element_blank()) +
      theme(axis.text.x      = element_text(size=12, color="black")) +
      theme(axis.text.y      = element_text(size=12, color="black")) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=10, hjust = 0, color = color.text)) +
      theme(axis.text.y      = element_text(size=10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) + 
      
      # Plot margins
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
  }
  
  # create empty calendar
  current_year <- format(Sys.Date(), "%Y")
  min.date <- as.Date(paste0(current_year, "-01-01"))
  max.date <- as.Date(paste0(current_year, "-12-31"))
  df <- data.frame(date = seq(min.date, max.date, by="days"), value = NA)
  na.value.forplot <- 'white'
  # fill in values
  df$value[match(dates, df$date)] <- values
  
  df$month <- as.numeric(format(df$date, "%m"))
  df$doy   <- as.numeric(format(df$date, "%j"))
  df$dow <- as.numeric(format(df$date, "%w"))
  df$woy <- as.numeric(format(df$date, "%U")) + 1
  df$woy[df$month == 12 & df$woy == 1] <- 53  # Handle last week of year correctly
  
  df$dowmapped <- ordered(df$dow, levels = 6:0)
  levels(df$dowmapped) <- rev(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  g <- ggplot(df, aes(woy, dowmapped, fill = value)) + 
    geom_tile(colour = "darkgrey") + 
    theme(aspect.ratio = 1/4) +  # Control aspect ratio through theme
    scale_x_continuous(
      expand = c(0, 0),
      breaks = function(x) {
        # Calculate month breaks dynamically
        sapply(1:12, function(m) {
          first_day <- as.Date(paste0(current_year, "-", m, "-01"))
          as.numeric(format(first_day, "%U")) + 1
        })
      },
      limits = c(0, 54),  # Ensure consistent width
      labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    ) + 
    my_theme() +
    scale_fill_gradientn(colours = c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384"), na.value = "white",
                         name = legendtitle,
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(75, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5
                         )) +
    labs(x = NULL, 
         y = NULL, 
         title = title, 
         subtitle = subtitle)
  
  my.lines<-data.frame(x=numeric(), 
                       y=numeric(), 
                       xend=numeric(), 
                       yend=numeric(), 
                       year=character())
  
  y.start <- df$dow[1]
  x.start <- df$woy[1]
  
  x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5)
  y.top.left <- 7.5
  x.top.right <- df$woy[nrow(df)] + 0.5
  y.top.right <- 7.5
  
  x.mid.left01 <- x.start - 0.5
  y.mid.left01 <- 7.5 - y.start
  x.mid.left02 <- x.start + 0.5
  y.mid.left02 <- 7.5 - y.start
  
  x.bottom.left <- x.start - 0.5
  y.bottom.left <- 0.5
  x.bottom.right <- ifelse(y.start == 6, df$woy[nrow(df)] + 0.5, df$woy[nrow(df)] - 0.5)
  y.bottom.right <- 0.5
  
  my.lines<-rbind(my.lines,
                  data.frame(x    = c(x.top.left, x.bottom.left, x.mid.left01, x.top.left, x.bottom.left), 
                             y    = c(y.top.left, y.bottom.left, y.mid.left01, y.top.left, y.bottom.left),
                             xend = c(x.top.right, x.bottom.right, x.mid.left02, x.mid.left02, x.mid.left01), 
                             yend = c(y.top.right, y.bottom.right, y.mid.left02, y.mid.left02, y.mid.left01), 
                             year = current_year))
  
  # lines to separate months
  for (j in 1:12)  {
    df.subset.month <- max(df$doy[df$month == j])
    x.month <- df$woy[df.subset.month]
    y.month <- df$dow[df.subset.month]
    
    x.top.mid <- x.month + 0.5
    y.top.mid <- 7.5
    
    x.mid.mid01 <- x.month - 0.5
    y.mid.mid01 <- 7.5 - y.month - 1
    x.mid.mid02 <- x.month + 0.5
    y.mid.mid02 <- 7.5 - y.month - 1
    
    x.bottom.mid <- ifelse(y.month == 6, x.month + 0.5, x.month - 0.5)
    y.bottom.mid <- 0.5
    
    my.lines<-rbind(my.lines,
                    data.frame(x    = c(x.top.mid, x.mid.mid01, x.mid.mid01), 
                               y    = c(y.top.mid, y.mid.mid01, y.mid.mid01),
                               xend = c(x.mid.mid02, x.mid.mid02, x.bottom.mid), 
                               yend = c(y.mid.mid02, y.mid.mid02, y.bottom.mid), 
                               year = current_year))
    
  }
  
  # add lines
  g <- g + geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), lineend = "square", color = "black", inherit.aes=FALSE)
  
  return(g)
}
