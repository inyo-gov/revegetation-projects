
# summary table by species average cover in the parcel and totals - including whether
# or not above 10% cover goal.

parcel_species_tbl_create_gt <- function(data,p,min_cov){
  data %>%
    filter(parcel == p) %>%
    group_by(Lifecycle,CommonName,Species) %>%
    summarise(Percent_Cover = round(sum(cover)*(1/200)*(100)*(1/20),6)) %>%
    arrange(desc(Percent_Cover)) %>%
    gt(groupname_col = c("Lifecycle")) %>%
    summary_rows(
      groups = TRUE,
      columns = Percent_Cover,
      fns = list(
        total= "sum",
        n = ~n()),
      decimals = 1
    )%>%
    gt_color_box(columns = Percent_Cover, domain = min_cov:12.00,
                 palette = c("orange", "green","darkgreen"))
}


# get the mean cover values for a parcel
mean_cover_value <- function(data,p){
  data %>% filter(parcel == p) %>%
    filter(Lifecycle == 'Perennial') %>%
    summarise(Percent_Cover = round(sum(cover)*(1/200)*(100)*(1/20),0))
}

# bubble maps
bubble_maps_species <- function(data){
  ggplot()+
    geom_sf(data=data, aes(size=percent_cover))+

    coord_sf()+
    facet_wrap(Provenance~Veg_Type+CommonName+Species)
}


stacked_species_composition_transect <- function(data,p){
  data %>%filter(parcel == p, Lifecycle == "Perennial") %>%  ggplot(aes(fill =species,
                                                                        y = tcov, x = transect))+
    geom_bar(position = "stack", stat = "identity")+
    ggtitle("Perennial species composition by transect")+
    theme(plot.title = element_text(hjust = 0.5))
}



# function to summarise transects to parcel
summarise_to_parcel <- function(x){
  p <- x %>% group_by(parcel)%>% summarise(
    Cover=mean(Cover),
    Shrub=mean(Shrub),
    Herb=mean(Herb),
    Grass=mean(Grass),
    TLC=mean(tot.live.cover),
    n.transects = n())

  return(p)
}


# function to summarise totals of functional type cover at the transect level

summarise_reveg_to_transect <- function(x){
  # at the transect level sum cover at the lifecycle, lifeform level
  tran.sums <- x %>% group_by(parcel,transect,Lifecycle,Lifeform)%>%
    summarise(Cover=sum(cover)/200 *100)# sum cover (total number of hits) and divide by 200 possible hits - multiply it by 100 to scale from 0-100.

  # summarise for each e.g. lifecycle/lifeform annual/perennial grass

  # create new dataframe with summary of all lifecycle cover
  # this will be joined to the wide dataframe below
  tran.tlc <- tran.sums %>%
    group_by(parcel,transect) %>% # sum all cover across all lifecycle/lifeform
    summarise(tot.live.cover = sum(Cover))

  # spread the lifeforms into columns
  pft.wide<-tran.sums %>% filter(Lifecycle == 'Perennial') %>% select(-Lifecycle) %>%  spread(Lifeform,Cover)
  # change na to 0
  pft.wide[is.na(pft.wide)] <- 0

  # create total cover variable
  pft.wide <- pft.wide %>%
    mutate(Cover= Grass + Herb + Shrub  + Tree)

  # now join the tlc from above
  pft.wide.wtot.cov <- pft.wide %>%
    left_join(tran.tlc, by = c("parcel",'transect'))

  # make a few more columns for percentage of the total for each functional group
  full <- pft.wide.wtot.cov %>%
    mutate(pShrubTran = Shrub / Cover,
           pGrassTran = Grass / Cover,
           pHerbTran = Herb / Cover) %>%
    mutate_if(is.numeric, ~replace_na(., 0))

  return(full)
}
