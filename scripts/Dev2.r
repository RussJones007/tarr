

?case_interview_classifier
?calc_age
?age_cat
?categorize_age
.libPaths()
library(maptiles)

frDate <- as.Date("2017-01-01")
toDate <- as.Date("2019-12-31")
 mmwrWeek(seq(from=frDate,to=toDate,by="week")) 
?Only.Functions
?mash_phone
?mmwrWeek

geo_sets |> names()

?load_tarrant_spatial
border <- load_tarrant_spatial("tarrant_border")
#border <- readRDS("~/R/Projects/GIS/Shape Files/Tarrant/Border/Tarrant Border.rds")
st_crs(border)
#border <- st_transform(border, crs = 4326)
#plot(st_geometry(border))


?scan_cluster
?mapCluster



tarr_2 <- get_tiles(x = border, 
                    provider  = "Stadia.AlidadeSmooth",
                    zoom = 11, 
                    crop = TRUE, 
                    apikey = Sys.getenv("STADIA"))

#tmap::tmap_provider_credits("Stadia.AlidadeSmooth")
foo <- synthetic_outbreak[synthetic_outbreak$condition == "fooflu",] |> 
  st_as_sf(coords = c("lon", "lat")) 

glimpse(foo)
st_crs(foo) <-  st_crs(border)
plot(st_geometry(foo))
plot(st_geometry(border), add = TRUE, col = NA)

foo_dbscan <- scan_cluster(points = foo, method = "HDBSCAN", eps = 5280*1.25, minPts = 7)
foo_dbscan <- scan_cluster(points = foo, eps = 5280*1.75, minPts = 4)
foo_dbscan
foo_dbscan |> print(n = 10)
foo_dbscan |> attributes()
so <- foo_dbscan |> scan_object() 
class(so)
so |> plot()
dbscan:::print.hdbscan
class(nx)
(required_cols %in% names(x))

names(x)
class(foo_dbscan)
class(x)
tmp <- foo_dbscan[, c("race", "sex", "condition", "cluster", "point_type")]
tmp |> class()
tmp
undebug(plot.clustered)
plot(tmp)

scan_object(foo_dbscan) |> class()
scan_object(foo_dbscan) 
scan_unit(foo_dbscan)


# mapCluster is currently failing.
ggmap::register_google(Sys.getenv("GOOGLE_MAPS_API"))
bkgrd <- ggmap::get_map(location = "Tarrant County", source = "google")
bkgrd_raster <- ggmap::ggmap(bkgrd) 
mapCluster(.points = foo_dbscan, .cluster = cluster, .core = point_type, .cond = "Foo Flu", .bkgrnd = bkgrd_raster)

#tmp <- plot(foo_dbscan, ratio = 1, background = "Esri.WorldStreetMap")+
tmp <- plot(foo_dbscan, ratio = 1)+
  tm_shape(shp = border) +
  tm_borders(col = "navy")

tmp
tmap::tmap_save(tm = tmp, filename = "../test.html", selfcontained = TRUE)
tmap::tmap_providers()

scan_object(foo_dbscan)
class(foo_dbscan)
foo_selected <- foo_dbscan |> 
  select(-sex)

scan_object(foo_selected)
class(foo_selected)

file.path(paths$spatial,"Tarrant/Border/Tarrant Border.rdata")
paths <- paths_defined()

# create a points object 
foo <- synthetic_outbreak[synthetic_outbreak$condition == "fooflu",] |>  
   st_as_sf(coords = c("lon", "lat"))
st_crs(foo) <- "epsg:4326"
foo <- st_transform(foo, "epsg:2276")
# dbscan for clusters     
foo_dbscan <- scan_cluster(points = foo, eps = 5280*2.5, minPts = 4)   

poly_1 <- foo_dbscan |> get_cluster_polys(cluster == 2, ratio = 0.7)
plot(st_geometry(foo_dbscan), col = "red")
plot(poly_1, add= TRUE, col = NA, border = "blue")

foo_dbscan |> names()
polys <- foo_dbscan |> 
  #filter(cluster == 2) |> 
  get_cluster_polys(cluster = cluster)

polys |> View()
plot(polys)

#foo_dbscan$point_type |> unique()
foo_hdbscan <- scan_cluster(points = foo, minPts = 7, method = "HDBSCAN", )
foo_hdbscan |> scan_object() |> glance()

new_foo <- st_geometry(foo) |>
  st_transform(, crs = "epsg:2276") 

jp_clust <- dbscan::jpclust(x = st_coordinates(new_foo), k = 6, kt = 3)
jp_clust |> tidy()
jp_clust |> class()
str(jp_clust)
jp_clust
plot(jp_clust)

new_foo_clust <- augment(x = jp_clust, data = new_foo)




#undebug(autoplot)
autoplot(foo_hdbscan, ratio = 1, title = "Test plot", subtitle = "For foo Flu")

plot(foo_dbscan, background = tarr_2, main = "Foo Disease Density Clustering", ratio = .5)
plot(foo_dbscan, main = "Foo Disease Density Clustering", ratio = .5)

#debug(plot.clustered)
plot(foo_dbscan, main = "Foo Disease Density Clustering", ratio = .75)


pl <- plot(foo_hdbscan, alpha = .6, 
     main = "Foo Disease Density Clustering", 
     ratio = .8,
     map_credit = tmap::tmap_provider_credits("Stadia.AlidadeSmooth"))

pl + 
  tm_shape(border) + 
  tm_borders(col = "navy")

pl


#undebug(autoplot.clustered)
pl <- autoplot.clustered(foo_hdbscan, background = tarr_2)  + 
  labs(title = "Foo Disease")
pl$labels$caption <- paste(pl$labels$caption, "\n", tmap::tmap_provider_credits("Stadia.AlidadeSmooth"))
pl


cen <- tarr.population::county_population(pop = tarr.population::census, 
                                          ethnicity = "All",
                                          year = 2020, 
                                          race = c("White", "Black", "All")
)
cen_df <- cen |> as.data.frame()
cen_df$age.char
plot(cen)
ag <- c(age.groups$Yr.5, 75, 80, 85, 90, 95) |> sort()
cen_2 <- tarr.population::group_ages(cen, age_groups = ag)
cen_df_2 <- cen_2 |> as.data.frame()
autoplot(cen_2)



# age_group work --------------------------------------------------------------------------------------------------

ages <- runif(100, 0, 99) |> as.integer()
ages[runif(5, 1, 98) |> as.integer()] <- 0
(ages == 0) |> table()

age_gr <- age_cat(ages = ages, by = c(0, 1, 15, 25, 40, 50, 60, 80, Inf))
levels(age_gr)
age_gr
age_ch <- c(as.character(age_gr), rep("< 1", 3))
age_ch[age_ch == "0"] <- "< 1"
unique(age_ch)

age_gr <- as.age_group(age_ch)
unique(age_gr)
get_symbols(age_gr)

as.character(age_gr)


agr <- age_cat(ages = ages, by = age.groups$Yr.5) |> 
  as.character()
agr
ag <- as.age_group(agr)
class(ag)
attributes(ag)
