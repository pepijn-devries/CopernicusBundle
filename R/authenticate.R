#' List Environment Variables for Storing Account Details
#' 
#' All implemented Copernicus packages use Environment variables
#' to easily and automatically access account details. This
#' way you don't have to store sensitive information in your
#' scripts. This function returns all variables, as you
#' would need to store them in the `.renviron` file.
#' @param package Package for which to show the variables.
#' If missing (default) variables for all implemented packages
#' are returned
#' @param which A string indicating which variables to return.
#' Only applicable when `package="CopernicusDataspace"`, it
#' can be any of `c("s3", "public_api", "private_api")`.
#' @param hint A `logical` value. If set to `true`, hints are shown.
#' They are included as comments (starting with `#`).
#' @param ... TODO
#' @returns A vector of `character` strings, listing the variables
#' as they should appear in your `.renviron` file.
#' @examples
#' cb_authentication_vars()
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

#' TODO
#' 
#' TODO
#' @param ... TODO
#' @returns TODO
#' @examples
#' # TODO
#' 
#' @export
cb_insert_authentication_vars <- function(...) {
  rstudioapi::insertText(
    cb_authentication_vars(...)
  )
}