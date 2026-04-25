# Testing function in the GeoFunction.r file

test_that("scan_cluster() function tests",
          {
            # Set up the objects used in the test
            pts_sf <- synthetic_outbreak |> 
              filter(condition == "fooflu") |> 
              st_as_sf(x = _, coords = c("lon", "lat"), crs = 4326)# simple feature points
            clust <- scan_cluster(points = pts_sf, minPts = 5, eps = set_units(5280, "feet") * 1.75)
            scan_object(clust)
            expect_error(scan_cluster(points = as.data.frame(pts_sf), minPts = 5, eps = set_units(5280, "feet")))
            expect_error(scan_cluster(points = pts_sf[0,], minPts = 5, eps = set_units(5280, "feet")))
            expect_error(scan_cluster(points = pts_sf, minPts = 5, eps = set_units(5280, "lumens")))
            expect_error(scan_cluster(points = pts_sf, minPts = 5, eps = set_units(5280, "feet"), crs = NA))
            expect_error(scan_cluster(points = pts_sf, minPts = "5", eps = set_units(5280, "feet"), crs = NA))
            expect_error(scan_cluster(points = pts_sf, minPts = 5, eps = "5280 feet"))
            expect_error(scan_cluster(points = pts_sf, minPts = 5, eps = c(1000, 5280)))
            expect_s3_class(clust, "sf")
            expect_equal(dbscan::ncluster(scan_object(clust)), 3)
            expect_equal(attr(clust, "scan")$eps |> as.numeric(), 5280 * 1.75)
            expect_equal(class(scan_object(clust))[1], "dbscan_fast")
          })
