#!/usr/bin/env Rscript
# Per-tile treetops + crowns outputs (tile-suffixed GPKGs)
#
# Fixes included:
#  1) CHM strict validity: CHM valid ONLY where BOTH DSM & DTM valid
#  2) Ortho footprint crop+mask: CHM is cropped/masked to ortho footprint and SAVED as final CHM
#  3) Tiling uses FINAL CHM extent (no more large boundary tiles)
#  4) Stable memory: terra-first, tile-by-tile conversions only
#  5) Safe writing: ALWAYS write treetops + crowns GPKGs (even if empty)
#  6) Crown delineation stable: crowns kept as terra SpatVector; write with terra::writeVector
#  7) Crown strategy: seeded (dalponte2016) using treetops -> fallback to watershed
#  8) Watershed ext is in PIXELS (computed from ext_m / resolution) [fixes 'ext must be positive integer']
#
# Run:
#   nohup Rscript run_crowns.R > run_crowns.log 2>&1 & disown

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(raster)   # lidR expects RasterLayer
  library(lidR)
  library(dplyr)
  library(EBImage)  # kept because you said it was needed in your env
})

# -------------------------
# Performance / stability knobs
# -------------------------
n_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
if (!is.finite(n_cores) || n_cores < 2) n_cores <- 2L
N_WORKERS <- max(1L, n_cores - 1L)

terraOptions(
  memfrac  = 0.80,
  tempdir  = "/tmp",
  progress = 0,
  threads  = N_WORKERS
)

# -------------------------
# Inputs / outputs
# -------------------------
dsm_path <- "C:/Users/nayani/mydata/CAREER/clipped_data/seedling_detection_data/sch4/dsm_sch4.tif"
dtm_path <- "C:/Users/nayani/mydata/CAREER/clipped_data/seedling_detection_data/sch4/sch4_dtm.tif"

# Ortho footprint (GeoTIFF)
ortho_path <- "C:/Users/nayani/mydata/CAREER/clipped_data/seedling_detection_data/sch4/sch4.tif"

out_dir <- "C:/Users/nayani/mydata/CAREER/clipped_data/seedling_detection_data/sch4"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

status_file    <- file.path(out_dir, "STATUS.txt")
tiles_done_rds <- file.path(out_dir, "sch4_tiles_done.rds")

prefix <- "sch4_orig"

# Base CHM (DSM - smoothed DTM) written once
chm_base_out  <- file.path(out_dir, paste0(prefix, "_chm_base.tif"))
# Final CHM masked/cropped to ortho footprint (THIS is used for tiling)
chm_final_out <- file.path(out_dir, paste0(prefix, "_chm_ortho_masked.tif"))

log_status <- function(msg) {
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line  <- paste0(stamp, " | ", msg, "\n")
  cat(line)
  cat(line, file = status_file, append = TRUE)
}

tile_id <- function(xs, ys) paste0("x", round(xs), "_y", round(ys))

# -------------------------
# Parameters you can tune
# -------------------------
# Treetops
hmin_main <- 0.3
ws_vec    <- c(0.3, 0.5, 0.8, 1.2)
dedupe_radius_m <- 0.4

# Tiles
tile_px  <- 3000
ws_main  <- 1.0
buffer_m <- max(2, 3 * ws_main)

# Crowns (seeded)
seeded_min_h <- 0.5
seeded_raster_res_m <- 0.06   # your CHM resolution
seeded_max_cr_m     <- 2.0    # allow shrubs/small trees
seeded_seed_radius_m <- 0.12  # ~2px at 0.06m; small

# Crowns (watershed fallback)
ws_min_h  <- 0.5
ws_th_tree <- 0.5   # meters (height threshold)
ws_tol     <- 0.08  # meters (tolerance)
ws_ext_m   <- 0.6   # <-- YOU REQUESTED 0.6 m crown expansion (converted to pixels inside function)

# NA fill (helps small shrubs in patchy CHM)
do_fill_na <- TRUE
fill_radius_m <- 0.12  # ~2px at 0.06m

# # Rock filter toggle (RStudio)
# do_rock_filter <- TRUE      # <- set FALSE to disable
# rock_min_area_m2 <- 2       # m^2  (your new threshold)
# rock_min_med_h   <- 0.35    # m
# rock_min_sd_h    <- 0.20    # m
# # -------------------------
# Safe writers
# -------------------------
safe_write_gpkg_sf <- function(x_sf, dsn, layer, quiet = TRUE) {
  dir.create(dirname(dsn), recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(x_sf)) {
    x_sf <- sf::st_sf(geometry = sf::st_sfc(crs = NA))
  }
  if (!inherits(x_sf, "sf")) stop("safe_write_gpkg_sf: object is not sf")
  
  if (nrow(x_sf) == 0) {
    if (!"geometry" %in% names(x_sf)) x_sf$geometry <- sf::st_sfc()
  }
  
  tryCatch({
    sf::st_write(x_sf, dsn = dsn, layer = layer, delete_dsn = TRUE, quiet = quiet)
    TRUE
  }, error = function(e) {
    log_status(paste0("WRITE FAILED: ", basename(dsn), " | ", conditionMessage(e)))
    FALSE
  })
}

# Create an empty POLYGON SpatVector safely
empty_crowns_vect <- function(crs_wkt, id_col = "treeID") {
  v <- terra::vect("POLYGON ((0 0,1 0,1 1,0 1,0 0))", crs = crs_wkt)
  v[[id_col]]  <- 1L
  v[["tile_id"]] <- "x0_y0"
  v <- v[0, ]  # empty but schema preserved
  v
}

safe_write_gpkg_vect <- function(v, dsn, crs_wkt, layer = "crowns") {
  dir.create(dirname(dsn), recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(v) || terra::nrow(v) == 0) {
    v <- empty_crowns_vect(crs_wkt = crs_wkt, id_col = "treeID")
  }
  
  tryCatch({
    terra::writeVector(v, dsn, filetype = "GPKG", layer = layer, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    log_status(paste0("WRITE FAILED: ", basename(dsn), " | ", conditionMessage(e)))
    FALSE
  })
}

empty_ttops_sf <- function(crs) {
  sf::st_sf(
    data.frame(Z = numeric(0), ws = numeric(0), tile_id = character(0)),
    geometry = sf::st_sfc(crs = crs)
  )
}

# -------------------------
# Helper: fill small NA gaps (helps shrubs/small crowns)
# -------------------------
fill_na_small <- function(chm_tile, radius_m = 0.12) {
  rx <- terra::res(chm_tile)[1]
  w_px <- max(3L, as.integer(round(radius_m / rx)) * 2L + 1L)  # odd window
  w <- matrix(1, w_px, w_px)
  
  na_mask <- is.na(chm_tile)
  if (!terra::global(na_mask, "sum", na.rm = TRUE)[1, 1] > 0) return(chm_tile)
  
  # local mean, then fill only where NA
  sm <- terra::focal(chm_tile, w = w, fun = mean, na.policy = "omit", fillvalue = NA)
  out <- chm_tile
  out[na_mask] <- sm[na_mask]
  out
}

# -------------------------
# 1) Read DSM/DTM
# -------------------------
log_status("Reading DSM/DTM with terra...")
dsm <- terra::rast(dsm_path)
dtm <- terra::rast(dtm_path)

# Resample DSM to DTM grid (your approach)
log_status("Resampling DSM -> DTM grid...")
dsm_resamp <- terra::resample(dsm, dtm, method = "bilinear")

# Crop to same extent (safety)
if (!all(terra::ext(dsm_resamp) == terra::ext(dtm))) {
  log_status("Cropping DTM to DSM extent...")
  dtm <- terra::crop(dtm, terra::ext(dsm_resamp))
}

# -------------------------
# 2) Build/load BASE CHM (strict DSM&DTM validity)
# -------------------------
if (file.exists(chm_base_out)) {
  log_status(paste0("Base CHM exists, loading: ", chm_base_out))
  chm_base <- terra::rast(chm_base_out)
} else {
  smooth_m <- 7
  rx <- terra::res(dtm)[1]
  w_px <- max(3L, as.integer(round(smooth_m / rx)))
  if (w_px %% 2 == 0) w_px <- w_px + 1L
  
  log_status("Building strict-validity mask (DSM & DTM both non-NA)...")
  valid_mask <- !is.na(dsm_resamp) & !is.na(dtm)
  
  log_status(paste0("Smoothing DTM: ~", smooth_m, " m (", w_px, " px)..."))
  w <- matrix(1, w_px, w_px)
  
  dtm_smooth <- terra::focal(
    dtm,
    w         = w,
    fun       = mean,
    na.policy = "omit",
    fillvalue = NA
  )
  
  # enforce original valid footprint after smoothing (prevents bleed)
  dtm_smooth <- terra::mask(dtm_smooth, valid_mask, maskvalues = 0, updatevalue = NA)
  
  log_status("Computing CHM = DSM - smoothed DTM (then masking to valid footprint)...")
  chm_base <- dsm_resamp - dtm_smooth
  chm_base[chm_base < 0] <- 0
  chm_base <- terra::mask(chm_base, valid_mask, maskvalues = 0, updatevalue = NA)
  
  log_status(paste0("Writing base CHM: ", chm_base_out))
  terra::writeRaster(
    chm_base,
    chm_base_out,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TILED=YES"))
  )
  
  rm(dtm_smooth, valid_mask)
  gc()
}

chm_crs <- terra::crs(chm_base, proj = TRUE)
if (is.na(chm_crs) || !nzchar(chm_crs)) stop("CHM CRS is missing.")

# -------------------------
# 3) Build/load FINAL ortho-masked CHM (THIS drives tiling)
# -------------------------
if (file.exists(chm_final_out)) {
  log_status(paste0("Final ortho-masked CHM exists, loading: ", chm_final_out))
  chm <- terra::rast(chm_final_out)
} else {
  if (!file.exists(ortho_path)) stop(paste0("Ortho file not found: ", ortho_path))
  
  log_status("Loading ortho and building footprint mask (one-time)...")
  ortho <- terra::rast(ortho_path)
  
  # project ortho to CHM CRS if needed
  if (!terra::same.crs(ortho, chm_base)) {
    log_status("Ortho CRS differs; projecting ortho to CHM CRS...")
    ortho <- terra::project(ortho, chm_base, method = "near")
  }
  
  # Crop BASE CHM to ortho intersection first (shrinks extent)
  inter_ext2 <- terra::intersect(terra::ext(chm_base), terra::ext(ortho))
  chm2   <- terra::crop(chm_base, inter_ext2, snap = "out")
  ortho2 <- terra::crop(ortho, terra::ext(chm2), snap = "out")
  
  # Align ortho to CHM grid (perfect masking)
  ortho2 <- terra::resample(ortho2, chm2, method = "near")
  
  # Prefer alpha if present and sensible; otherwise band1 non-NA
  use_alpha <- FALSE
  if (terra::nlyr(ortho2) >= 4) {
    rng4 <- terra::global(ortho2[[4]], "range", na.rm = TRUE)
    if (is.finite(rng4[1, 1]) && is.finite(rng4[1, 2])) {
      if (rng4[1, 2] <= 1.01 || rng4[1, 2] <= 255.01) use_alpha <- TRUE
    }
  }
  
  if (use_alpha) {
    log_status("Using ortho band 4 (alpha) for footprint mask...")
    footprint <- ortho2[[4]] > 0
  } else {
    log_status("Using ortho band 1 non-NA for footprint mask...")
    footprint <- !is.na(ortho2[[1]])
  }
  
  log_status("Masking CHM to ortho footprint...")
  chm <- terra::mask(chm2, footprint, maskvalues = 0, updatevalue = NA)
  
  log_status(paste0("Writing FINAL ortho-masked CHM: ", chm_final_out))
  terra::writeRaster(
    chm,
    chm_final_out,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TILED=YES"))
  )
  
  rm(ortho, ortho2, footprint, chm2)
  gc()
}

# -------------------------
# 4) Helpers: treetops + crowns
# -------------------------
detect_ttops_multiscale <- function(chm_tile, ws_vec = c(0.3, 0.5, 0.8, 1.2), hmin = 0.3) {
  chm_use <- chm_tile
  chm_use[chm_use < hmin] <- NA
  
  mx <- terra::global(chm_use, "max", na.rm = TRUE)[1, 1]
  if (!is.finite(mx)) return(NULL)
  
  chm_r <- raster::raster(chm_use)
  
  out <- lapply(ws_vec, function(ws) {
    tt <- lidR::locate_trees(chm_r, lidR::lmf(ws = ws, hmin = hmin))
    if (!is.null(tt) && nrow(tt) > 0) tt$ws <- ws
    tt
  })
  
  out <- Filter(function(x) !is.null(x) && nrow(x) > 0, out)
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

dedupe_ttops <- function(ttops_sf, radius_m = 0.4) {
  if (is.null(ttops_sf) || nrow(ttops_sf) == 0) return(ttops_sf)
  
  buf <- sf::st_buffer(ttops_sf, radius_m)
  grp <- sf::st_intersects(buf, buf)
  
  group_id <- integer(nrow(ttops_sf))
  gid <- 0L
  for (i in seq_len(nrow(ttops_sf))) {
    if (group_id[i] != 0) next
    gid <- gid + 1L
    stack <- i
    while (length(stack)) {
      j <- stack[1]
      stack <- stack[-1]
      if (group_id[j] != 0) next
      group_id[j] <- gid
      stack <- unique(c(stack, grp[[j]]))
    }
  }
  
  ttops_sf$grp <- group_id
  ttops_sf |>
    dplyr::group_by(grp) |>
    dplyr::slice_max(order_by = Z, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-grp)
}

# Seeded crowns (dalponte2016) using treetops as seeds
# Returns terra SpatVector (polygons) or NULL
crowns_seeded_dalponte <- function(chm_tile, tt_sf, min_h = 0.5, max_cr = 2.0, raster_res = 0.06) {
  if (is.null(tt_sf) || nrow(tt_sf) == 0) return(NULL)
  
  chm_use <- chm_tile
  chm_use[chm_use < min_h] <- NA
  mx <- terra::global(chm_use, "max", na.rm = TRUE)[1, 1]
  if (!is.finite(mx) || mx < min_h) return(NULL)
  
  chm_r <- raster::raster(chm_use)
  
  # lidR wants SpatialPointsDataFrame
  sp <- as(tt_sf, "Spatial")
  
  algo <- tryCatch(
    lidR::dalponte2016(chm = chm_r, treetops = sp, th_tree = min_h, max_cr = max_cr, ID = "treeID"),
    error = function(e) NULL
  )
  if (is.null(algo)) return(NULL)
  
  seg_r <- tryCatch(algo(), error = function(e) NULL)
  if (is.null(seg_r)) return(NULL)
  
  seg <- terra::rast(seg_r)
  seg[seg == 0] <- NA
  
  polys <- terra::as.polygons(seg, dissolve = TRUE, values = TRUE, na.rm = TRUE)
  if (is.null(polys) || terra::nrow(polys) == 0) return(NULL)
  
  names(polys)[1] <- "treeID"
  polys
}

# Watershed fallback: ext in METERS -> converted to integer pixels
crowns_from_chm_watershed_lidr <- function(chm_tile, min_h = 0.5, th_tree = 0.5, tol = 0.08,
                                           ext_m = 0.6, id_col = "treeID",
                                           do_fill_na = TRUE, fill_radius_m = 0.12) {
  chm_use <- chm_tile
  chm_use[chm_use < min_h] <- NA
  
  mx <- terra::global(chm_use, "max", na.rm = TRUE)[1, 1]
  if (!is.finite(mx) || mx < min_h) return(NULL)
  
  if (do_fill_na) chm_use <- fill_na_small(chm_use, radius_m = fill_radius_m)
  
  rx <- terra::res(chm_use)[1]
  ext_px <- max(1L, as.integer(round(ext_m / rx)))  # MUST be positive integer
  
  chm_r <- raster::raster(chm_use)
  
  seg_fun <- lidR::watershed(chm_r, th_tree = th_tree, tol = tol, ext = ext_px)
  seg_r <- seg_fun()
  if (is.null(seg_r)) return(NULL)
  
  seg <- terra::rast(seg_r)
  seg[seg == 0] <- NA
  
  polys <- terra::as.polygons(seg, dissolve = TRUE, values = TRUE, na.rm = TRUE)
  if (is.null(polys) || terra::nrow(polys) == 0) return(NULL)
  
  names(polys)[1] <- id_col
  polys
}

# -------------------------
# 5) Tile plan + resume (USES FINAL CHM)
# -------------------------
rx <- terra::res(chm)[1]
ry <- terra::res(chm)[2]
tile_w_m <- tile_px * rx
tile_h_m <- tile_px * ry

E <- terra::ext(chm)
x_starts <- seq(E[1], E[2], by = tile_w_m)
y_starts <- seq(E[3], E[4], by = tile_h_m)

tiles_done <- character(0)
if (file.exists(tiles_done_rds)) tiles_done <- readRDS(tiles_done_rds)
log_status(paste0("Threads: ", N_WORKERS, " | Tiles already done: ", length(tiles_done)))

# -------------------------
# 6) Main loop
# -------------------------
log_status("Starting tiling loop...")

for (xs in x_starts) {
  for (ys in y_starts) {
    
    tid <- tile_id(xs, ys)
    if (tid %in% tiles_done) next
    
    ttops_tile_gpkg  <- file.path(out_dir, paste0(prefix, "_treetops_", tid, ".gpkg"))
    crowns_tile_gpkg <- file.path(out_dir, paste0(prefix, "_crowns_",   tid, ".gpkg"))
    
    ex <- terra::ext(
      xs, min(xs + tile_w_m, E[2]),
      ys, min(ys + tile_h_m, E[4])
    )
    exb <- terra::ext(
      ex[1] - buffer_m, ex[2] + buffer_m,
      ex[3] - buffer_m, ex[4] + buffer_m
    )
    
    log_status(paste0("Tile ", tid, " | crop buffered CHM..."))
    chm_tile <- terra::crop(chm, exb, snap = "out")
    
    mx <- terra::global(chm_tile, "max", na.rm = TRUE)[1, 1]
    if (!is.finite(mx) || mx < hmin_main) {
      log_status(paste0("Tile ", tid, " max<hmin or all-NA; writing EMPTY outputs."))
      
      tt_empty <- empty_ttops_sf(crs = chm_crs)
      safe_write_gpkg_sf(tt_empty, ttops_tile_gpkg, layer = "treetops")
      
      safe_write_gpkg_vect(NULL, crowns_tile_gpkg, crs_wkt = chm_crs, layer = "crowns")
      
      tiles_done <- c(tiles_done, tid)
      saveRDS(tiles_done, tiles_done_rds)
      
      rm(chm_tile, tt_empty)
      gc()
      next
    }
    
    # ---- Treetops
    log_status(paste0("Tile ", tid, " | treetop detection..."))
    tt_ms <- tryCatch(
      detect_ttops_multiscale(chm_tile, ws_vec = ws_vec, hmin = hmin_main),
      error = function(e) {
        log_status(paste0("Tile ", tid, " treetop detection FAILED: ", conditionMessage(e)))
        NULL
      }
    )
    
    tt_final <- NULL
    if (is.null(tt_ms) || nrow(tt_ms) == 0) {
      log_status(paste0("Tile ", tid, " | no treetops; writing EMPTY treetops."))
      tt_final <- empty_ttops_sf(crs = chm_crs)
      safe_write_gpkg_sf(tt_final, ttops_tile_gpkg, layer = "treetops")
      tt_sf <- NULL
    } else {
      tt_sf <- sf::st_as_sf(tt_ms)
      sf::st_crs(tt_sf) <- chm_crs
      
      # keep only points inside unbuffered bbox
      xy <- sf::st_coordinates(tt_sf)
      keep <- xy[, 1] >= ex[1] & xy[, 1] <= ex[2] & xy[, 2] >= ex[3] & xy[, 2] <= ex[4]
      log_status(paste0("Tile ", tid, " treetops raw=", nrow(tt_sf), " kept=", sum(keep)))
      tt_sf <- tt_sf[keep, , drop = FALSE]
      
      if (nrow(tt_sf) == 0) {
        log_status(paste0("Tile ", tid, " | all treetops in buffer; writing EMPTY treetops."))
        tt_final <- empty_ttops_sf(crs = chm_crs)
        safe_write_gpkg_sf(tt_final, ttops_tile_gpkg, layer = "treetops")
      } else {
        tt_final <- dedupe_ttops(tt_sf, radius_m = dedupe_radius_m)
        tt_final$tile_id <- tid
        safe_write_gpkg_sf(tt_final, ttops_tile_gpkg, layer = "treetops")
      }
    }
    
    # ---- Crowns: seeded -> watershed fallback
    crowns_v <- NULL
    
    if (!is.null(tt_sf) && nrow(tt_sf) > 0) {
      log_status(paste0("Tile ", tid, " | seeded crowns (dalponte2016)..."))
      crowns_v <- tryCatch(
        crowns_seeded_dalponte(
          chm_tile = chm_tile,
          tt_sf    = tt_sf,
          min_h    = seeded_min_h,
          max_cr   = seeded_max_cr_m,
          raster_res = seeded_raster_res_m
        ),
        error = function(e) {
          log_status(paste0("Tile ", tid, " seeded crowns FAILED: ", conditionMessage(e)))
          NULL
        }
      )
      
      if (is.null(crowns_v) || terra::nrow(crowns_v) == 0) {
        log_status(paste0("Tile ", tid, " | seeded crowns empty; fallback to watershed..."))
      }
    } else {
      log_status(paste0("Tile ", tid, " | no treetops available for seeding; using watershed..."))
    }
    
    if (is.null(crowns_v) || terra::nrow(crowns_v) == 0) {
      crowns_v <- tryCatch(
        crowns_from_chm_watershed_lidr(
          chm_tile = chm_tile,
          min_h    = ws_min_h,
          th_tree  = ws_th_tree,
          tol      = ws_tol,
          ext_m    = ws_ext_m,            # 0.6 m expansion -> pixels internally
          id_col   = "treeID",
          do_fill_na = do_fill_na,
          fill_radius_m = fill_radius_m
        ),
        error = function(e) {
          log_status(paste0("Tile ", tid, " watershed crowns FAILED: ", conditionMessage(e)))
          NULL
        }
      )
    }
    
    if (is.null(crowns_v) || terra::nrow(crowns_v) == 0) {
      log_status(paste0("Tile ", tid, " | no crowns; writing EMPTY crowns."))
      safe_write_gpkg_vect(NULL, crowns_tile_gpkg, crs_wkt = chm_crs, layer = "crowns")
    } else {
      crowns_v$tile_id <- tid
      crowns_v <- terra::crop(crowns_v, ex)
      
      if (is.null(crowns_v) || terra::nrow(crowns_v) == 0) {
        log_status(paste0("Tile ", tid, " | crowns cropped to zero; writing EMPTY crowns."))
        safe_write_gpkg_vect(NULL, crowns_tile_gpkg, crs_wkt = chm_crs, layer = "crowns")
      } else {
        safe_write_gpkg_vect(crowns_v, crowns_tile_gpkg, crs_wkt = chm_crs, layer = "crowns")
      }
    }
    
    # checkpoint
    tiles_done <- c(tiles_done, tid)
    saveRDS(tiles_done, tiles_done_rds)
    
    rm(chm_tile, tt_ms, tt_final, tt_sf, crowns_v)
    gc()
    
    log_status(paste0("Tile ", tid, " DONE."))
  }
}

log_status("All tiles finished.")
