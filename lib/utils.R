theme_Publication <- function(base_size=8, base_family=switch(Sys.info()[['sysname']], Windows = "ArialMT", Darwin = "Arial")) {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.2), hjust = 0.5),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold", size = rel(1), color = "black"),
                axis.title.y = element_text(angle=90, vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(size = rel(0.9), color = "black"),
                axis.line = element_line(color = "black", size = 0.2),
                axis.ticks = element_line(color = "black", size = 0.2),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.size= unit(0.4, "cm"),
                legend.title = element_text(face="italic"),
                legend.text=element_text(size = rel(0.9)),
                plot.margin=unit(c(1,0.5,0.5,0.5),"lines"),
                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold")
        ))
}

merge_iter <- function(x, by){
    # This function is used to iteratively merge dataframe of the same structure from a list
    #
    # Arguments:
    #   x, the list storing the dataframes
    #   by, either a single value indicating the column that will be used as key column by all dataframes; or a vector of the same length of x, which stores the number of key column.
    #
    # Return:
    #   A merged dataframe.
    ll <- length(x)
    if (length(by) > 1 & length(by) != ll){
        return("Error in by")
    } else if (length(by)==1){
        if (length(x)<=2){
            t <- merge(x[[1]], x[[2]], by = by)
            return(t)
        } else {
            t <- merge(x[[1]], merge_iter(x[-1], by = by), by.x = by, by.y = 1)
            return(t)
        }
    } else {
        # TBA
    }
    
}
