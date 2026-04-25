#library(tarr)
#library(sp)
library(tidyverse)
library(sf)
#library(lubridate)
#library(purrr)
#library(tidycensus)
#library(arrow)

#devtools::install()
tarr.filters
begins <- seq(mdy("04-14-1920"), mdy("04-14-2020"), by = 1)
ends <- begins + years(x = as.integer(rnorm(n = length(begins), mean = 38, sd = 15)))
system.time(
ages <- age.calc(begins, ends, .unit = "year")
)
hist(ages)
rm(begins, ends, ages)

synthetic_outbreak |> glimpse()

rm(list=ls())
#devtools::load_all()

paths

#---- baseNbsProcess read test ---
red <- nbsCSVRead()                   # test reading and classifying csv files from  NBS reports
range(red$Event.Date, na.rm = T)
diseases <- baseData(.chooseNew = F)  # records in base.rdata should be returned
diseases <- baseData(.chooseNew = T)  # records in base.rdata plus selected csv file records should be returned
range(diseases$Event.Date)
table(is.na(diseases$Investigation.ID.trisano))

# Look at TriSano cases
trs <- diseases %>%
  filter(!is.na(diseases$Investigation.ID.trisano),
         year(Event.Date) == 2019, Event.Date < mdy("11/10/2019"))
clipr::write_clip(trs)
range(trs$Event.Date)
table(trs$Condition)

rm(diseases,red, trs)

trs <- triSano_import()
table(trs$Condition)

nbs <- nbs_import()
table(nbs$Condition)
glimpse(nbs)

#--- paths test ----
?paths
file.path(paths$data, "base.rdata") %>% file.exists()
load(file.path(paths$data, "base.rdata"))
#pathFile("data", "base.rdata", exists = T)
#paths$data
#levels(dis$Condition.Description)
mps <- dis[year(dis$Event.Date) %in% c(2017,2018, 2019) &
  grepl("Typhus",dis$Condition, ignore.case = TRUE) & dis$Case.Status %in% c("Confirmed","Probable"),] %>%
  droplevels()

table(mps$MMWR.Year)
table(mps$Condition)
rm(dis)

names(mps)[grepl("Age|Name",names(mps), ignore.case=T)]

mps[,"City"]  <- as.character(mps[,"City"]) # convert factor to character
mps[5,"City"] <- "Tarrant County"
addrs <- makeAddress(mps)
addrs[1:10]
rm(addrs)
mps$City


#---- geo functions test -----
pts_sf <- synthetic_outbreak |> 
  filter(condition == "fooflu") |> 
  st_as_sf(x = _, coords = c("lon", "lat"), crs = 4326)# simple feature points
  
#border_fn <- "../GIS/Shape Files/Tarrant/Border/Tarrant Border.rds"

#tarr.border    <- readRDS(border_fn)
tarr.border <- load_tarrant_spatial("tarrant_border")
tarr.border <- st_transform(tarr.border, st_crs(pts_sf))

plot(st_geometry(tarr.border))
plot(st_geometry(pts_sf), add = TRUE)

box <- st_bbox(tarr.border)  %>%
    matrix(ncol= 2, nrow = 2, byrow = F, dimnames = list(c("x","y"), c("min","max")))
box[, "max"] <- box[, "max"] + .005
box[, "min"] <- box[, "min"] - .005

tam_fn <- paste0(paths$spatial,"/Tarrant_County_Toner.rda")

if(file.exists(tam_fn)){
  load(tam_fn)
} else {
  #TMap <- ggmap::get_map("Tarrant County", zoom = 10, maptype =  "hybrid", source = "osm", color = "bw")
  TMap <- ggmap::get_stamenmap(bbox = box, zoom = 11, maptype = "toner-lite")
  #save(file = tam_fn,TMap)
}
#rm(tam_fn)

TarrMap <- ggmap::ggmap(TMap,darken = c(.4,"White"), maprange = FALSE) + 
  ggplot2::coord_cartesian(xlim = box["x", ], ylim = box["y", ]) 

rm(box)
TarrMap

date_interval <-  min(pts_sf$onset, na.rm=T) %--%  max(pts_sf$onset, na.rm=T)
date_interval

#undebug(scan_cluster)
clust <- scan_cluster(points = pts_sf, minPts = 5, eps = set_units(5280, "feet") * 1.75)
attr(clust, "scan")
class(clust)
plot(clust)
table(clust$cluster)


scan_params <- function(clust){attr(clust, "scan")}
cluster_count <- function(clust) scan_params(clust)$cluster_count
cluster_count(clust)

units(scan_params(clust)$eps)["numerator"]

pal <- c("grey60", viridis::viridis_pal(option = "A")(cluster_count(clust)))
pal

limited <- clust[clust$cluster > 0,]
plot(clust["cluster"], pch = 16, 
     col = c("grey60", viridis::viridis_pal()(5))[clust$cluster+1])

dbscan::hullplot(x = st_coordinates(clust)[clust$cluster > 0,], 
                 cl = clust$cluster[clust$cluster > 0], 
                 cex = 1, 
                 pch = 20,
                 col = pal[clust$cluster+1])

plot(clust, col = pal[clust$cluster+1], add = TRUE)




units::valid_udunits() |> View()

st_crs(2276)$units 
st_crs(2276) |> View()
ft <- units::make_units(feet)
mile <- units::set_units(5280, feet)
units(mile) <- units::as_units("km")
mile

plot(st_geometry(tarr.border))
plot(st_geometry(clust), pch = as.integer(factor(clust$point_type)) + 20, col = clust$cluster, bg = clust$cluster, add = TRUE)
rm(tarr.border)

cls_sf_prj <- st_transform(x = cls_sf, crs = "epsg:2276")
tmp <- optics(st_coordinates(cls_sf_prj), eps = 5280 * 20 , minPts = 5)
tmp
plot(tmp)

dbscan::as.dendrogram(tmp) |> plot()
extracted <- dbscan::extractDBSCAN(object = tmp, eps_cl = 5280 * 2) 
plot(extracted)
extracted$cluster |> table()
View(extracted)

tmp <- dbscan::hdbscan(x = st_coordinates(cls_sf_prj), minPts = 7)
dbscan::coredist(x = st_coordinates(cls_sf_prj), minPts = 5)
tmp$cluster |> table()
plot(tmp, show_flat = TRUE)


#debug(getPolys)
mp <- mapCluster(.bkgrnd = TarrMap,
           .points = cls_sf,
           .tf = date_interval,
           .cluster = Cluster.Title,
           .core = Core,
           .cond = "Fooflu",
           .drawCluster = TRUE)
mp

debug(mapCluster)
mp_sf <- mapCluster(.bkgrnd = TarrMap,
                          .points = cls_sf,
                          .tf = date_interval,
                          .cluster = Cluster,
                          .core = Core,
                          .cond = "FooFlu",
                          .drawCluster = FALSE)

mp_sf

rm(TarrMap,TMap,tam_fn,date_interval,  cls_sf, mp_sf, pts_sf, mp)

#--- MMWR functions tests
mmwrYearFirstEndDate(1960)
mmwrYearFirstEndDate(1940:1943)
mmwrYearFirstEndDate(c(1939))  # this statement should throw an error
mmwrYearFirstEndDate(year(mps$Event.Date)) %>% unique()

dts <- rep(seq(from=as.Date("1955-01-01"), as.Date("2018-07-31"),by = "1 day"),100)
system.time(
  tmp1 <-  mmwrWeek(dts))
system.time(
  tmp2 <- epiweek(dts))
tmps <- cbind(tmp1,tmp2)
table(tmp1==tmp2)
rm(tmp1,tmp2,tmps, dts)

epiweek(x = mps$Event.Date)
head(mps$Event.Date)
mmwrWeek(as.Date("2018-05-10"))

mmwrWeekBegin(mmwrWeek(mps$Event.Date), year(mps$Event.Date))
mmwrWeekEnd(weekNum = mmwrWeek(mps$Event.Date))
table(mmwrWeekMonth(mmwrWeek(mps$Event.Date),label = T, abbr = F))
mmwrYearFirstEndDate(yr = c(2017,2018,2020))
mmwrYearFirstEndDate(yr = 1960:2100)  # currently an error is thrown here
mmwrWeek(mdy("12/31/2020"))

#---- population and age  category tests ----
tarr::population$census |> attributes()
tarr::population$census.estimates |> attributes()
pop <- tarr::population$census

map(tarr::population, \(x) attr(x, which = "source"))
class(tarr::population$census)
#debug(retrieve_county_population)
tmp <- retrieve_county_population(.pop_df = pop,
                                  #.age = c("1", "2"),
                                  .age = 0:18,
                                  .sex = c("Male", "Female"),
                                  .year = c(2010, 2020), 
                                  .race = c("Asian", "White"),
                                  .county = c("Tarrant"),
                                  .ethnicity = "All" #c("Non-Hispanic", "Hispanic")
)

groups <- age_cat(ages = iv_start(tmp$age.iv), by = age.groups$ILI)
groups


census.estimates$age.iv |> unique() |> class()

age.cat(mps$AgeYrs,.by=5,.above.char = "+")
age.cat(.ages = mps$AgeYrs, .by = c(0,1,5,10,15,20,30,50,65))
age.cat(mps$AgeYrs,.by = c(0,4,9,14,19,30,40,50,60))

age.groups$IMM

tmp <- retrieve_county_population(.df = tarr::population$texas.estimates, .year=2015, .race = "All")
tmp <- retrieve_county_population(.df = tarr::population$texas.estimates,
                                  .year = c(2017:2019),
                                  .sex=c("All"), .age = c(0,1,5,10,15,20,30,50,65))


#==== Test plot_age_group_year function
rm(list=ls())
dis <- baseNbsProcess(.chooseNew = F)
glimpse(dis)

levels(dis$Condition)
dis$Condition %>% unique()
conds <- c("Typhus fever-fleaborne, murine","Acute Flaccid Myelitis (AFM)", "Salmonellosis",
           "Shiga toxin-producing Escherichia coli (STEC)","Shigellosis", "Campylobacteriosis")
charts2 <- purrr::map(conds, function(x) dis %>% filter(Condition == x) %>%
                 plot_age_group_year(.dateFld = Event.Date,.ageFld = AgeYrs,.age.groups = age.groups$Yr.10,
                                     .condition = x,.years = c(2010:2019),
                                     .rates = F,
                                     #.fill = "sienna4",
                                     #.th = ggthemes::theme_tufte(ticks=F),
                                     #.th = ggthemes::theme_hc(),
                                     #.th = ggthemes::theme_stata(),
                                     #.th = ggthemes::theme_few(),
                                     .external = F,
                                     .suppress.level = 10))
charts2

charts2 <- plot_age_group_year(dis[dis$Condition.Description=="Acute Flaccid Myelitis (AFM)",],
                    .dateFld = Event.Date,.ageFld = AgeYrs,.age.groups = age.groups$Yr.10,
                    .condition = "Acute Flaccid Myelitis (AFM)",.years = c(2010:2019),
                    .rates = F,
                    #.fill = "sienna4",
                    #.th = ggthemes::theme_tufte(ticks=F),
                    #.th = ggthemes::theme_hc(),
                    #.th = ggthemes::theme_stata(),
                    #.th = ggthemes::theme_few(),
                    .external = F,
                    .suppress.level = 0)
charts2
sum(charts2$data$Cases[charts2$data$Year==2017])

#logo <- jpeg::readJPEG("G:\\Monthly Report\\Code\\TAarrantSeal.jpg",native = T)
logo <-  magick::image_read("G:\\Monthly Report\\Code\\TAarrantSeal.jpg")
#logo <-  magick::image_read("~/R/Projects/Comm Disease/Data/R scripts/TAarrantSeal.jpg")
# charts2 + annotation_raster(raster = logo,xmin = 10, xmax = 30,ymin = 100, ymax = 130)
grid::grid.raster(logo, x = 0.90, y = 0.92, just = c('left', 'bottom'), width = unit(.75, 'inches'))

rm(list=ls())

# test epiCurve
#library(incidence)

load(file.path(paths$data, "base.rdata"))
conds <- c("Typhus fever-fleaborne, murine","Acute Flaccid Myelitis (AFM)", "Salmonellosis",
           "Shiga toxin-producing Escherichia coli (STEC)","Shigellosis", "Campylobacteriosis")

dis <- dis %>%
  filter(Condition %in% conds) %>%
  droplevels()

dis %>%
  filter(grepl("Salmonell",Condition,  ignore.case = T)) %>%
  pull(Condition) %>%
  table()

dis <- dis %>% filter(Condition %in% c("Salmonellosis", "Campylobacteriosis", "Shigellosis"))

#tmp <- epi_curve(.df = dis %>% filter(
#tmp <- epi_curve(.df = dis %>% filter(Condition %in% c("Salmonellosis", "Campylobacteriosis", "Shigellosis"),
#                                      lubridate::year(Event.Date) > 2015),
tmp <- epi_curve(.df = filter(dis, grepl("Salmonell",Condition, ignore.case=T), Event.Date > as.Date("2013-01-01")),
                 .dateBrks = "4 months",
                 .groupFld = Condition,
                 .interval = "month",
                 .facet = T,
                 .fill = "sienna",
                 .dateFld = "Event.Date"#,
                 #.th = ggthemes::theme_pander()
)
tmp


tmp <- epi_curve(.df = dis %>% filter(Condition %in% c("Salmonellosis", "Campylobacteriosis"),
                                         lubridate::year(Event.Date) > 2015),
                    .dateBrks = "year",
                    .groupFld = Condition,
                    .interval = "year",
                    .fill = c("sienna","grey30"),
                    .dateFld = Event.Date,
                    .facet = T,
                    .th = ggthemes::theme_pander()
)
tmp

pop2016_2019 <- retrieve_county_population(.year = 2016:2019,.county = "Tarrant") %>%
  dplyr::filter(age.group == "All Ages")

rates <- left_join(x = tmp$data %>% mutate(Year = year(Dates)),
                   y = pop2016_2019 %>% select(Year=year,population) %>% mutate(Year = as.numeric(Year)),
                   by = "Year") %>%
  mutate(Rates = as.numeric(format(Cases/population*10^5, digits = 1,nsmall = 1)),
         Cases = Rates)

tmp$data <- rates
tmp$labels$y <- "Cases/ 10^5 Population"
tmp$labels$title <- "Rates by Year"
tmp +
  scale_y_continuous(limits = c(0,max(rates$Cases,na.rm = T)),expand = expand_scale(add= c(0,.1)))#+
  # geom_text(aes(x=Dates ,y=Cases,label=Cases),
  #           vjust=1,hjust=0.5,size=3.5,color="white", inherit.aes = TRUE)


?paths
?scanCluster
?mapCluster
?retrieve_county_population
?age.cat
?age.groups
?plot_age_group_year
?tarr

# test loading of spatial objects
tmp <- load_tarrant_spatial(.name = "ZCTA")
tborder <- load_tarrant_spatial(.name = "tarrant_border")
city <- load_tarrant_spatial("tarrant_cities")


plot(st_geometry(tmp), col = "blue")
plot(st_geometry(tborder), border = "red", col = NA, add=TRUE)
plot(city[,1])
rm(tmp, tborder, city)

# ===== Test retrieval of the zip code estimates ===
tmp <- retrieve_zip_code_population(.endYear = 2015)
tmp <- retrieve_zip_code_population(.endYear = "2017")
tmp <- retrieve_zip_code_population(.endYear = 2020)  # test error condition
rm(list=ls())

#========  Test correct_city function =====
cities <- baseData(.chooseNew = F) %>%
  pull(City) %>%
  #unique() %>%
  sort()

table(cities)

tmp <- correct_city(cities) %>%
  stringr::str_to_title()
  #unique() %>%
  #sort()
table(tmp[grepl("Fort( *)W(o|q|r)(r|o)(t|h)(h|t)", tmp, ignore.case = T)])

cities[grepl("Fort( *)W(o|q|r)(r|o)(t|h)(h|t|k)", cities, ignore.case = T)] %>% unique()

correct_city(cities) %>%
  stringr::str_to_title() %>%
#unique() %>%
sort() %>%
  table()

geo_sets$ZCTA
zips <- retrieve_zip_code_population(.endYear = 2019)
zips_geo <- load_tarrant_spatial(.name = "ZCTA")

plot(zips_geo, max.plot = 5)

create_key("Ronald","McDonald", lubridate::mdy("01-05-1945"), len = 5)
lubridate::is.Date(lubridate::mdy("01-05-1945"))
# Test %nin%
x <- 1:10
table <- 7:15
x[x %nin% table]

tarrant.zip.codes.estimates$end.year %>% unique()

ret <- retrieve_county_population(.year = 2019, .ageGrp = age.groups$Annual)
sum(ret$population)
ret2 <- retrieve_county_population(.year = 2019, .ageGrp = 1)
sum(ret2$population)/2

groups = c("0","34", "5-15", "60 to 80", "80+", "> 85", "25 -", "< 30",
 "all", "104", "80-105")

groups
age.group.encode(groups)


ret


str_pad("This is  test of padding", 30, "left")
9005015
ret
groups
open_ended_more
en65 <- age.group.encode("< 65 ")
age.group.decode(en65)
age.group.decode(9950000)

(.ageGrp = age.groups$Annual)

tarr::census.estimates$race %>% unique() %>% sort()
