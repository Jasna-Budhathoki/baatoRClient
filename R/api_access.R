library(R6)
library(httr)


baato_API <- R6Class(classname = "baato_API", #name of the class
                     public = list(
                       key = NULL,
                       initialize = function(key = NA){
                         self$key = key
                       },
                       set_key = function(key){
                         self$key <- key 
                         cat("The key is ", self$key)
                       },
                       display_key = function(){
                         cat(paste0("the key is", self$key, ".\n"))
                       },
                       
                       #validate the response 
#' Check if the json object returned by API call is valid 
#'
#' @param req 
#'
#' @return error message 
#' @export
                       validate_resp = function(req){
                         if (http_type(req) != "application/json") {
                           stop("API did not return json", call. = FALSE)
                         }
                         
                         if (http_error(req)) {
                           stop(
                             sprintf(
                               "Baato API request failed [%s]\n%s\n>", 
                               status_code(req),
                               useful_response$message
                             ),
                             call. = FALSE
                           )
                         }
                         
                         if (req$status_code != 200) {
                           pull <- jsonlite::fromJSON(content)
                           stop(pull$message, call. = FALSE)
                         }
                         
                       },
                       
                       #display the response 

#' display the response from the API request 
#'
#' @param req 
#'
#' @return the API response in JSON format 
#' @export
                       display_resp = function(req){
                         content <- httr::content(req, as = "text")
                         api_char <- base::rawToChar(req$content)
                         api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
                         api_JSON
                         
                       },
                       
#' Calls the reverse_search api 
#'
#' @param lat 
#' @param lon 
#'
#' @return the API response in JSON format 
#'
#' @examples baato$rev_api_call(lat = 27.70446921370009, lon = 85.32051086425783)
#' @export
                       rev_api_call = function(lat,lon){
                         reverse_url = "https://api.baato.io/api/v1/reverse"
                         req <- httr::GET(reverse_url, query = list(
                           key = self$key,
                           lat = lat,
                           lon = lon
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                       },
                       
#' Calls the search api 
#'
#' @param q 
#' @param limit 
#'
#' @return the API response in JSON format 
#' @export
#'
#' @examples baato$search_api_call(q="shemrock nepalgunj",limit=4)
#' @export
                       search_api_call = function(q,limit) {
                         search_url <- "https://api.baato.io/api/v1/search"
                         req <- httr::GET(search_url, query = list(
                           key = self$key,
                           q = q,
                           limit = limit
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
#' Calls the places api 
#'
#' @param placeId 
#'
#' @return the API response in JSON format 
#'
#' @examples baato$places_api_call(placeId = 102235)
#' @export
                       places_api_call = function(placeId){
                         place_url <- "https://api.baato.io/api/v1/places"
                         req <- httr::GET(place_url, query = list(
                           key = self$key,
                           placeId = placeId
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
#' Title
#'
#' @param type 
#' @param lat 
#' @param lon 
#' @param limit
#'  
#' @examples baato$nearby_api_call(type="school",lat=27.71765,lon=85.32691,limit=20)
#'
#' @return the API response in JSON format 
#' @export
#' 
                       nearby_api_call = function(type,lat,lon,limit){
                         nearby_url <- "https://api.baato.io/api/v1/search/nearby"
                         req <- httr::GET(nearby_url, query = list(
                           key = self$key,
                           type = type,
                           lat = lat,
                           lon = lon, 
                           limit = limit
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
#' Title
#'
#' @param style_name 
#'
#' @return the API response in JSON format 
#'
#' @examples
#' baato$styles_api_call(style_name="monochrome")
#' @export
                       styles_api_call = function(style_name){
                         styles_url <- "https://api.baato.io/api/v1/styles"
                         req <- httr::GET(styles_url, query = list(
                           style_name = style_name,
                           key = self$key
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
#' Title
#'
#' @param points 
#' @param mode 
#'
#' @return the API response in JSON format 
#'
#' @examples baato$baato_directions_api(points = point, mode="car")
#' @export
                       baato_directions_api = function(points,mode) {
                         directions_url <- "https://api.baato.io/api/v1/directions"
                         p1 = points[1]
                         p2 = points[2]
                         
                         req <- httr::GET(directions_url, query = list(
                           key = self$key,
                           "points[]" = p1,
                           "points[]" = p2,
                           mode = mode
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       }
                       
                       
                     )
)


baato <- baato_API$new(key = "bpk.05j39KLSi-P80QvtSUdrMYpwuIqsgjTYlMuWtcvZ9UDZ")
baato$rev_api_call(lat = 27.70446921370009, lon = 85.32051086425783)
point <- c("27.71772,85.32784","27.73449,85.33714")
baato$baato_directions_api(points = point, mode="car")
baato$nearby_api_call(type="school",lat=27.71765,lon=85.32691,limit=20)
baato$places_api_call(placeId = 102235)
baato$search_api_call(q="shemrock nepalgunj",limit=4)
baato$styles_api_call(style_name="monochrome")

