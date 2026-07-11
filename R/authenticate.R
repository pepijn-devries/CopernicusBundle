#' TODO
#' 
#' TODO
#' @param package TODO
#' @param which TODO
#' @returns TODO
#' @export
cb_authentication_vars <- function(package, which, hint = TRUE, ...) {
  if (missing(package)) package <- ""
  if (missing(which)) which <- ""
  
  climate_text <- \() {
    c(
      "# https://pepijn-devries.github.io/CopernicusClimate/articles/download.html#access-token https://cds.climate.copernicus.eu/profile"[hint],
      "ECMWF_DATASTORES_KEY=\"token\""
    )
  }
  dataspace_text <- \() {
    private_api <- \() {
      c(
        "# https://pepijn-devries.github.io/CopernicusDataspace/articles/Authentication.html"[hint],
        "CDSE_API_USERNAME=\"username\"",
        "CDSE_API_PASSWORD=\"password\""
      )
    }
    public_api <- \() {
      c(
        "# https://pepijn-devries.github.io/CopernicusDataspace/articles/Authentication.html"[hint],
        "CDSE_API_CLIENTID=\"clientid\"",
        "CDSE_API_CLIENTSECRET=\"clientsecret\""
      )
    }
    s3 <- \() {
      c(
        "# https://pepijn-devries.github.io/CopernicusDataspace/articles/Authentication.html"[hint],
        "CDSE_API_S3ID=\"s3_id\"",
        "CDSE_API_S3SECRET=\"s3_secret\""
      )
    }
    switch(
      which,
      private_api = private_api(),
      public_api  = public_api(),
      s3          = s3(),
      c(private_api(), public_api(), s3())
    )
  }
  marine_text <- \() {
    c(
      "# https://data.marine.copernicus.eu/register"[hint],
      "COPERNICUSMARINE_SERVICE_USERNAME=\"username\"",
      "COPERNICUSMARINE_SERVICE_PASSWORD=\"password\""
    )
  }
  switch (tolower(package),
          copernicusclimate   = climate_text(),
          copernicusdataspace = dataspace_text(),
          copernicusmarine    = marine_text(),
          c(
            climate_text(), dataspace_text(), marine_text()
          )
  )
}

#' @export
cb_insert_authentication_vars <- function(...) {
  rstudioapi::insertText(
    cb_authentication_vars(...)
  )
}