# List Environment Variables for Storing Account Details

All implemented Copernicus packages use Environment variables to easily
and automatically access account details. This way you don't have to
store sensitive information in your scripts. This function returns all
variables, as you would need to store them in the `.renviron` file.

## Usage

``` r
cb_authentication_vars(package, which, hint = TRUE, ...)
```

## Arguments

- package:

  Package for which to show the variables. If missing (default)
  variables for all implemented packages are returned

- which:

  A string indicating which variables to return. Only applicable when
  `package="CopernicusDataspace"`, it can be any of
  `c("s3", "public_api", "private_api")`.

- hint:

  A `logical` value. If set to `true`, hints are shown. They are
  included as comments (starting with `#`).

- ...:

  TODO

## Value

A vector of `character` strings, listing the variables as they should
appear in your `.renviron` file.

## Examples

``` r
cb_authentication_vars()
#>  [1] "# https://pepijn-devries.github.io/CopernicusClimate/articles/download.html#access-token https://cds.climate.copernicus.eu/profile"
#>  [2] "ECMWF_DATASTORES_KEY=\"token\""                                                                                                    
#>  [3] "# https://pepijn-devries.github.io/CopernicusDataspace/articles/Authentication.html"                                               
#>  [4] "CDSE_API_USERNAME=\"username\""                                                                                                    
#>  [5] "CDSE_API_PASSWORD=\"password\""                                                                                                    
#>  [6] "# https://pepijn-devries.github.io/CopernicusDataspace/articles/Authentication.html"                                               
#>  [7] "CDSE_API_CLIENTID=\"clientid\""                                                                                                    
#>  [8] "CDSE_API_CLIENTSECRET=\"clientsecret\""                                                                                            
#>  [9] "# https://pepijn-devries.github.io/CopernicusDataspace/articles/Authentication.html"                                               
#> [10] "CDSE_API_S3ID=\"s3_id\""                                                                                                           
#> [11] "CDSE_API_S3SECRET=\"s3_secret\""                                                                                                   
#> [12] "# https://data.marine.copernicus.eu/register"                                                                                      
#> [13] "COPERNICUSMARINE_SERVICE_USERNAME=\"username\""                                                                                    
#> [14] "COPERNICUSMARINE_SERVICE_PASSWORD=\"password\""                                                                                    
```
