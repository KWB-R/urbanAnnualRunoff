install.packages("ecmwfr", repos = "https://cloud.r-project.org")

### 0) Register for CDS services: https://cds.climate.copernicus.eu/user/register

### 1) Copy UUID and API KEY: https://cds.climate.copernicus.eu/user

### 2) Add both in R environement file:
### "COPERNICUS_CLIMATE_UUID = "myuuid"
### "COPERNICUS_CLIMATE_APIKEY = "myapikey"
usethis::edit_r_environ()

### 3) Run ecmwfr::wf_set_key()
ecmwfr::wf_set_key(user = Sys.getenv("COPERNICUS_CLIMATE_UUID"),
                   key = Sys.getenv("COPERNICUS_CLIMATE_APIKEY"),
                   service = "cds")

### 4) Go to: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
###    to create a query

### 5) Copy to Rstudio -> mark all -> goto Tools -> Addins -> Browse_Addins
###    ecmwfr -> Python to Mars -> in order to convert query to R list:
### see: https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html

request <- list(
  product_type = "reanalysis",
  format = "grib",
  variable = c("2m_temperature", "evaporation", "potential_evaporation", "precipitation_type", "runoff", "sub_surface_runoff", "surface_runoff", "total_precipitation"),
  year = "2020",
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(40, 116, 39, 117),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download.grib"
)

### 6) Run request (may take a while as nothing happens because many requests R
###    are queued (in addition: maximum request data amount limited to 140000
###    data points)
###    To check queue status: https://cds.climate.copernicus.eu/cdsapp#!/yourrequests?tab=form

ncfile <- ecmwfr::wf_request(request = request,
                             transfer = TRUE,
                             path = "~",
                             verbose = FALSE)
