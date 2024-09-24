
# summary table by species average cover in the parcel and totals - including whether
# or not above 10% cover goal.

# added group_by year (e.g. so 2024 can be added)
# parcel_species_tbl_create_gt <- function(data,p,min_cov){
#   data %>%
#     filter(parcel == p) %>%
#     group_by(year, Lifecycle,CommonName,Species) %>%
#     summarise(Percent_Cover = round(sum(cover)*(1/200)*(100)*(1/20),6)) %>%
#     arrange(desc(Percent_Cover)) %>%
#     gt(groupname_col = c("Lifecycle")) %>%
#     summary_rows(
#       groups = TRUE,
#       columns = Percent_Cover,
#       fns = list(
#         total= "sum",
#         n = ~n()),
#       decimals = 1
#     )%>%
#     gt_color_box(columns = Percent_Cover, domain = min_cov:12.00,
#                  palette = c("orange", "green","darkgreen"))
# }
#
# parcel_species_tbl_create_gt <- function(data, p, min_cov) {
#   data %>%
#     filter(parcel == p) %>%
#     group_by(Lifecycle, CommonName, Species, year) %>%
#     summarise(Percent_Cover = round(sum(hits) * (1/200) * 100 * (1/20), 6)) %>%
#     arrange(desc(Percent_Cover)) %>%
#     pivot_wider(names_from = year, values_from = Percent_Cover) %>%
#     # Ensure 2022 appears before 2024
#     select(CommonName, Species, `2022`, `2024`) %>%
#     gt(groupname_col = "Lifecycle") %>%
#     summary_rows(
#       groups = TRUE,
#       columns = c(`2022`, `2024`),
#       fns = list(
#         total = "sum",
#         n = ~n()
#       ),
#       decimals = 7
#     ) %>%
#     gt_color_box(columns = c(`2022`, `2024`), domain = min_cov:12.00,
#                  palette = c("orange", "green", "darkgreen"))
# }


#
# # Adjusted function to include number_species and display cover values to the hundredths place
# parcel_species_tbl_create_gt <- function(data, p, min_cov) {
#   data %>%
#     filter(parcel == p) %>%
#     group_by(Lifecycle, CommonName, Species, year) %>%
#     summarise(Percent_Cover = round(sum(hits) * (1/200) * 100 * (1/20), 6)) %>%
#     arrange(desc(Percent_Cover)) %>%
#     pivot_wider(names_from = year, values_from = Percent_Cover) %>%
#     # Ensure 2022 appears before 2024
#     select(CommonName, Species, `2022`, `2024`) %>%
#     gt(groupname_col = "Lifecycle") %>%
#     summary_rows(
#       groups = TRUE,
#       columns = c(`2022`, `2024`),
#       fns = list(
#         number_species = ~n()  # Count the number of species
#       ),
#       decimals = 2  # Ensure summary row shows values to hundredths place
#     ) %>%
#     gt_color_box(columns = c(`2022`, `2024`), domain = min_cov:12.00,
#                  palette = c("orange", "green", "darkgreen")) %>%
#     fmt_number(
#       columns = c(`2022`, `2024`),
#       decimals = 2  # Ensure values show to the hundredths place
#     )
# }
#



# Adjusted function to include number_species and display cover values to the hundredths place
parcel_species_tbl_create_gt <- function(data, p, min_cov) {
  data %>%
    filter(parcel == p) %>%
    group_by(Lifecycle, CommonName, Species, year) %>%
    summarise(Percent_Cover = round(sum(hits) * (1/200) * 100 * (1/20), 6)) %>%
    arrange(desc(Percent_Cover)) %>%
    pivot_wider(names_from = year, values_from = Percent_Cover) %>%
    # Ensure 2022 appears before 2024
    select(CommonName, Species, `2022`, `2024`) %>%
    gt(groupname_col = "Lifecycle") %>%
    summary_rows(
      groups = TRUE,
      columns = c(`2022`, `2024`),
      fns = list(
        total = "sum",  # Sum of cover values
        number_species = ~n()  # Count the number of species
      ),
      decimals = 2  # Ensure summary row shows values to hundredths place
    ) %>%
    gt_color_box(columns = c(`2022`, `2024`), domain = min_cov:12.00,
                 palette = c("orange", "green", "darkgreen")) %>%
    fmt_number(
      columns = c(`2022`, `2024`),
      decimals = 2  # Ensure values show to the hundredths place
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_summary(rows = "number_species")  # Make the number_species row bold
    )
}


# get the mean cover values for a parcel
mean_cover_value <- function(data,p){
  data %>% filter(parcel == p) %>%
    group_by(year) %>%
    filter(Lifecycle == 'Perennial') %>%
    summarise(Percent_Cover = round(sum(hits)*(1/200)*(100)*(1/20),0))
}



bubble_maps_with_parcels <- function(data, parcels) {
  ggplot(data) +
    # Adding parcel boundaries with no fill and a black outline
    geom_sf(data = parcels, fill = NA, color = "black", size = 0.5) +
    # Mapping the size of the points to the percent cover
    geom_sf(aes(size = percent_cover)) +
    # Set coordinate system for spatial data
    coord_sf() +
    # Set the range of sizes for the points to make differences clear
    scale_size(range = c(0.5, 5)) +
    # Facet by CommonName and year_compare to see changes per species over years
    facet_grid(species ~ year_compare) +
    # Adjusting theme to clean up non-essential plot elements
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),
      legend.position = "right",
      strip.text.y = element_text(angle = 0, hjust = 0.5)
    )
}

# Modify the bubble_maps_species function to rotate facet labels
bubble_maps_species <- function(data, lifeform, legend_position){
  ggplot(data %>% filter(Lifeform %in% lifeform)) +
    geom_sf(aes(size = percent_cover)) +
    coord_sf() +
    scale_size(range = c(.5,5)) +  # Adjust the point size range (min, max)
    # facet_grid(Lifeform + CommonName ~ Veg_Type + year) +
    # facet_grid(Lifeform + CommonName ~ year) +
    # facet_grid(Veg_Type+Lifecycle+Lifeform ~ year) +
    facet_grid(Lifeform+Lifecycle +CommonName ~ year) +


    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),  # Remove titles
      legend.position = legend_position,      # Remove legend
      strip.text.y = element_text(angle = 0, hjust = 0.5)  # Rotate y-axis facet labels to be horizontal
    )
}


# Modify the bubble_maps_species function to rotate facet labels
bubble_maps_species2 <- function(data){
  data %>% ggplot()+
    geom_sf(aes(size = percent_cover)) +
    coord_sf() +
    scale_size(range = c(.5,5)) +  # Adjust the point size range (min, max)
    # facet_grid(Lifeform + CommonName ~ Veg_Type + year) +
    # facet_grid(Lifeform + CommonName ~ year) +
    # facet_grid(Veg_Type+Lifecycle+Lifeform ~ year) +
    # facet_grid(CommonName+parcel ~ year) +
    facet_grid(CommonName~ year) +


    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),  # Remove titles
      legend.position = "right",      # Remove legend
      strip.text.y = element_text(angle = 0, hjust = 0.5)  # Rotate y-axis facet labels to be horizontal
    )
}


calculate_cover_difference_long <- function(data) {
  # Remove geometry temporarily to handle pivoting
  data_no_geometry <- st_drop_geometry(data)

  # Group by necessary columns and calculate the mean percent_cover
  cover_diff_data <- data_no_geometry %>%
    group_by(parcel, transect, species) %>%
    summarise(
      cover_2022 = mean(percent_cover[year == 2022], na.rm = TRUE),
      cover_2024 = mean(percent_cover[year == 2024], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Replace NA with 0 in cover_2022 and cover_2024
    mutate(
      cover_2022 = replace_na(cover_2022, 0),
      cover_2024 = replace_na(cover_2024, 0),
      cover_diff = cover_2024 - cover_2022
    ) %>%
    # Ensure unique records
    distinct(parcel, transect, species, cover_2022, cover_2024, cover_diff)

  # Pivot longer to combine cover_2022, cover_2024, and cover_diff into a single column
  cover_diff_long <- cover_diff_data %>%
    pivot_longer(
      cols = starts_with("cover_"),  # Select columns that start with "cover_"
      names_to = "year_compare",      # New column to store the names
      values_to = "percent_cover"     # New column to store the values
    )

  # Join geometry back into the dataset
  result_with_geometry <- left_join(cover_diff_long,
                                    distinct(select(data, parcel, transect, geometry)),
                                    by = c("parcel", "transect"))

  return(result_with_geometry)
}


calculate_cover_difference <- function(data) {
  # Remove geometry temporarily to handle pivoting
  data_no_geometry <- st_drop_geometry(data)

  # Group by necessary columns and calculate the mean percent_cover
  cover_diff_data <- data_no_geometry %>%
    group_by(parcel, transect, species, year) %>%
    summarise(percent_cover = mean(percent_cover, na.rm = TRUE), .groups = 'drop') %>%

    # Pivot wider to compare 2022 and 2024 data
    pivot_wider(
      names_from = year,
      values_from = percent_cover,
      names_prefix = "cover_"
    ) %>%

    # Replace NAs with 0 in both cover_2022 and cover_2024
    mutate(cover_2022 = replace_na(cover_2022, 0),
           cover_2024 = replace_na(cover_2024, 0)) %>%

    # Calculate the difference between 2024 and 2022
    mutate(cover_diff = cover_2024 - cover_2022) %>%

    # Ensure unique records
    distinct(parcel, transect, species, cover_2022, cover_2024, cover_diff)

  # Join geometry back into the dataset
  result_with_geometry <- left_join(cover_diff_data,
                                    distinct(select(data, parcel, transect, geometry)),
                                    by = c("parcel", "transect"))

  return(result_with_geometry)
}

# Now define the plotting function with parcel boundaries
bubble_maps_cover_difference <- function(data, parcels) {
  data %>%
    ggplot(aes(size = abs(cover_diff), color = cover_diff > 0)) +
    geom_sf(data = parcels, fill = NA, color = "black", linetype = "solid") +  # Add parcel boundaries
    geom_sf() +  # Plot the cover differences
    coord_sf() +
    scale_size(range = c(0.5, 5)) +  # Adjust the point size range
    scale_color_manual(values = c("red", "green"), labels = c("Decrease", "Increase")) +  # Red for decrease, green for increase
    facet_grid(year ~ species) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(),
      legend.position = "right",
      strip.text.y = element_text(angle = 0, hjust = 0.5)
    ) +
    labs(size = "Absolute Cover Difference", color = "Cover Change")
}


#
# stacked_species_composition_transect <- function(data,p){
#   data %>%filter(parcel == p, Lifecycle == "Perennial") %>%  ggplot(aes(fill =species,
#                                                                         y = tcov, x = transect))+
#     geom_bar(position = "stack", stat = "identity")+
#     ggtitle("Perennial species composition by transect")+
#     theme(plot.title = element_text(hjust = 0.5))+
#     facet_wrap(~year)
# }


stacked_species_composition_transect <- function(data,p){
  data %>%filter(parcel == p, Lifecycle == "Perennial") %>%  ggplot(aes(fill =species,
                                                                        y = tcov, x = transect))+
    geom_bar(position = "stack", stat = "identity")+
    ggtitle("Perennial species composition by transect")+
    theme(plot.title = element_text(hjust = 0.5))
}


# added group by year
# function to summarise transects to parcel
summarise_to_parcel <- function(x){
  p <- x %>% group_by(parcel,year)%>% summarise(
    Cover=mean(Cover),
    Shrub=mean(Shrub),
    Herb=mean(Herb),
    Grass=mean(Grass),
    TLC=mean(tot.live.cover),
    n.transects = n())

  return(p)
}


# function to summarise totals of functional type cover at the transect level
# added group by year
summarise_reveg_to_transect <- function(x){
  # at the transect level sum cover at the lifecycle, lifeform level
  tran.sums <- x %>%
    group_by(parcel,year,transect,Lifecycle,Lifeform)%>%
    summarise(Cover=sum(hits)/200 *100)# sum cover (total number of hits) and divide by 200 possible hits - multiply it by 100 to scale from 0-100.

  # summarise for each e.g. lifecycle/lifeform annual/perennial grass

  # create new dataframe with summary of all lifecycle cover
  # this will be joined to the wide dataframe below
  tran.tlc <- tran.sums %>%
    group_by(parcel,year, transect) %>% # sum all cover across all lifecycle/lifeform
    summarise(tot.live.cover = sum(Cover))

  # spread the perennial lifeforms into columns
  pft.wide<-tran.sums %>% filter(Lifecycle == 'Perennial') %>% select(-Lifecycle) %>%  spread(Lifeform,Cover)
  # change na to 0
  pft.wide[is.na(pft.wide)] <- 0

  # create total cover variable
  pft.wide <- pft.wide %>%
    mutate(Cover= Grass + Herb + Shrub  + Tree)

  # now join the tlc from above
  pft.wide.wtot.cov <- pft.wide %>%
    left_join(tran.tlc, by = c("parcel",'transect','year'))

  # make a few more columns for percentage of the total for each functional group
  full <- pft.wide.wtot.cov %>%
    mutate(pShrubTran = Shrub / Cover,
           pGrassTran = Grass / Cover,
           pHerbTran = Herb / Cover) %>%
    mutate_if(is.numeric, ~replace_na(., 0))

  return(full)
}
