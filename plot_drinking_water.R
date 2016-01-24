rs(list = ls())
require(ggplot2)
require(RColorBrewer)
require(dplyr)

source('seads_data_utils.R')

load_if_necessary()




extract_land_df <- function(ds, idx) {
  land_q <- extract_q(df, section == "Drinking water") #Tab == 3
  df <- filter_quantitative(extract_data_frame(ds, land_q,
                                               include_site_no = TRUE,
                                               include_irr_type = TRUE,
                                               include_aid = TRUE), land_q)
  df <- fix_site_numbering(df, site_religion)
  
  agro <- aggregate(land_agriculture_3.1.1 ~ land_agriculture_3.1.3 + site_no, data = df, FUN = sum)
  names(agro) <- sub('land_agriculture_3.1.1','land.holdings',names(agro), fixed=TRUE)
  names(agro) <- sub('land_agriculture_3.1.3','land.tenure',names(agro), fixed=TRUE)
  homestead <- aggregate(land_homestead_3.2.1 ~ land_homestead_3.2.3 + site_no, data = df, FUN = sum)
  names(homestead) <- sub('land_homestead_3.2.1','land.holdings',names(homestead), fixed=TRUE)
  names(homestead) <- sub('land_homestead_3.2.3','land.tenure',names(homestead), fixed=TRUE)
  invisible(list(agricultural = agro, homestead = homestead))
}

plot_land <- function(df, title, fill_plot = TRUE, max_levels =NULL, palettes = NULL) {
  if (is.null(palettes)) palettes <- c('Accent', 'Set2')
  if(is.null(max_levels)) {
    max_levels <- sum(brewer.pal.info[palettes,'maxcolors'])
  }
  
  specials <- special_values()
  
  ordered_levels <- levels(df$land.tenure)
  ordered_levels <- ordered_levels[! ordered_levels %in% specials ]
  level_order <- order(unlist(lapply(ordered_levels,
                                     function(x) sum(df$land.holdings[df$land.tenure == x], na.rm=T))),
                       decreasing = T)
  if (length(level_order) > max_levels - length(specials)) {
    level_order <- level_order[1:(max_levels - length(specials) - 1)]
    ordered_levels <- c(ordered_levels[level_order], 'OTHER', specials)
  } else {
    ordered_levels <- c(ordered_levels[level_order], specials)
  }
  
  fdf <- df[,c('site_no','land.holdings','land.tenure')]
  fdf <- fdf[! is.na(fdf$land.tenure),]
  fdf$land.tenure <- factor(fdf$land.tenure, levels = c(levels(fdf$land.tenure), 'OTHER'))
  fdf$land.tenure[! fdf$land.tenure %in% ordered_levels] <- 'OTHER'
  
  fdf$land.tenure <- factor(fdf$land.tenure, levels = ordered_levels, ordered=TRUE)
  
  fdf <- fdf[with(fdf, order(site_no, land.tenure)),]
  
  pal <- expand_palette(length(ordered_levels), palettes)
  
  if (fill_plot) {
    ylab = "Land holdings (percentage of total area)"
    fdf <- fdf %>% group_by(site_no) %>%
      mutate(land.holdings = 100 * land.holdings / sum(land.holdings))
  } else {
    fdf$land.holdings <- fdf$land.holdings / 100
    ylab = "Land holdings (hectares)"
  }
  
  
  p <- ggplot(fdf, aes(x = site_no, y = land.holdings, fill = land.tenure))
  #  p <- p + stat_summary(aes(y=land.holdings, fill=land.tenure), fun.y=sum, position=pos, geom="bar")
  p <- p + geom_bar(stat = "identity", position = 'stack')
  p <- p + scale_fill_manual(values = pal, guide = guide_legend(reverse=TRUE), name = "Land Tenure")
  p <- p + labs(x = "Site Number (from most to least Muslim)",
                y = ylab, title = title)
  p <- p + theme_classic(base_size = 20)
  p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))
  p
}

if (TRUE) {
  land <- extract_land_df(mar_survey_data, questionnaire)
  print("Generating Output")
  output_path <- 'output'
  if (! dir.exists(output_path)) dir.create(output_path)
  graphics.off()
  png(filename=file.path(output_path,"agro_land_area.png"), width=1024,height=1024, bg="white", type="cairo-png")
  print(plot_land(land$agricultural, 'Agricultural Land Holdings', FALSE))
  dev.off()
  png(filename=file.path(output_path,"agro_land_pct.png"), width=1024,height=1024, bg="white", type="cairo-png")
  print(plot_land(land$agricultural, 'Agricultural Land Holdings', TRUE))
  dev.off()
  png(filename=file.path(output_path,"home_land_area.png"), width=1024,height=1024, bg="white", type="cairo-png")
  print(plot_land(land$homestead, 'Homestead Land Holdings', FALSE))
  dev.off()
  png(filename=file.path(output_path,"home_land_pct.png"), width=1024,height=1024, bg="white", type="cairo-png")
  print(plot_land(land$homestead, 'Homestead Land Holdings', TRUE))
  dev.off()
  write.csv(land$agricultural[,c('site_no','land.tenure','land.holdings')],
            file=file.path(output_path,'agro_land.csv'), row.names=FALSE)
  write.csv(land$homestead[,c('site_no','land.tenure','land.holdings')],
            file=file.path(output_path,'home_land.csv'), row.names=FALSE)
}

plot_land_count <- function(df, title, fill_plot = TRUE, max_levels =NULL, palettes = NULL) {
  if (is.null(palettes)) palettes <- c('Accent', 'Set2')
  if(is.null(max_levels)) {
    max_levels <- sum(brewer.pal.info[palettes,'maxcolors'])
  }
  
  specials <- special_values()
  
  ordered_levels <- levels(df$land.tenure)
  ordered_levels <- ordered_levels[! ordered_levels %in% specials ]
  level_order <- order(unlist(lapply(ordered_levels,
                                     function(x) sum(df$land.holdings[df$land.tenure == x], na.rm=T))),
                       decreasing = T)
  if (length(level_order) > max_levels - length(specials)) {
    level_order <- level_order[1:(max_levels - length(specials) - 1)]
    ordered_levels <- c(ordered_levels[level_order], 'OTHER', specials)
  } else {
    ordered_levels <- c(ordered_levels[level_order], specials)
  }
  
  fdf <- df[,c('site_no','land.holdings','land.tenure')]
  fdf <- fdf[! is.na(fdf$land.tenure),]
  fdf$land.tenure <- factor(fdf$land.tenure, levels = c(levels(fdf$land.tenure), 'OTHER'))
  fdf$land.tenure[! fdf$land.tenure %in% ordered_levels] <- 'OTHER'
  
  fdf$land.tenure <- factor(fdf$land.tenure, levels = ordered_levels, ordered=TRUE)
  
  fdf <- fdf[with(fdf, order(site_no, land.tenure)),]
  
  pal <- expand_palette(length(ordered_levels), palettes)
  
  if (fill_plot) {
    ylab = "Households (percentage of total households)"
    fdf <- fdf %>% group_by(site_no) %>%
      mutate(land.holdings = 100 * land.holdings / sum(land.holdings),
             land.households = 100 / n())
  } else {
    fdf$land.holdings <- fdf$land.holdings / 100
    fdf$land.households <- 1
    ylab = "Households"
  }
  
  
  p <- ggplot(fdf, aes(x = site_no, y = land.households, fill = land.tenure))
  #  p <- p + stat_summary(aes(y=land.holdings, fill=land.tenure), fun.y=sum, position=pos, geom="bar")
  p <- p + geom_bar(stat = "identity", position = 'stack')
  p <- p + scale_fill_manual(values = pal, guide = guide_legend(reverse=TRUE), name = "Land Tenure")
  p <- p + labs(x = "Site Number (from most to least Muslim)",
                y = ylab, title = title)
  p <- p + theme_classic(base_size = 20)
  p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))
  p
}

