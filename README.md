
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppcongress

The goal of ppcongress is to provide an R interface to [ProPublica’s
Congress API](https://projects.propublica.org/api-docs/congress-api/).
The following endpoints have been
    mapped:

  - [members](https://projects.propublica.org/api-docs/congress-api/members/)

The following endpoints are on the
    roadmap:

  - [bills](https://projects.propublica.org/api-docs/congress-api/bills/)
  - [votes](https://projects.propublica.org/api-docs/congress-api/votes/)
  - [statements](https://projects.propublica.org/api-docs/congress-api/statements/)
  - [committees](https://projects.propublica.org/api-docs/congress-api/committees/)

Please file an [issue](https://github.com/zamorarr/ppcongress/issues) if
there are other endpoints you want added.

## Example

``` r
library(ppcongress)
```

You can get a list of all the members of the 116th congress in the house
with the `members()` function:

``` r
json <- members("house", 116)
print(json)
#> <propublica congress/v1/116/house/members.json>
#> List of 5
#>  $ congress   : chr "116"
#>  $ chamber    : chr "House"
#>  $ num_results: int 442
#>  $ offset     : int 0
#>  $ members    :List of 442
```

The function `as_tibble()` will convert this list object to a data
frame:

``` r
df <- as_tibble(json)
str(df)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    442 obs. of  44 variables:
#>  $ id                  : chr  "A000374" "A000370" "A000055" "A000371" ...
#>  $ title               : chr  "Representative" "Representative" "Representative" "Representative" ...
#>  $ short_title         : chr  "Rep." "Rep." "Rep." "Rep." ...
#>  $ api_uri             : chr  "https://api.propublica.org/congress/v1/members/A000374.json" "https://api.propublica.org/congress/v1/members/A000370.json" "https://api.propublica.org/congress/v1/members/A000055.json" "https://api.propublica.org/congress/v1/members/A000371.json" ...
#>  $ first_name          : chr  "Ralph" "Alma" "Robert" "Pete" ...
#>  $ middle_name         : chr  NA NA "B." NA ...
#>  $ last_name           : chr  "Abraham" "Adams" "Aderholt" "Aguilar" ...
#>  $ suffix              : chr  NA NA NA NA ...
#>  $ date_of_birth       : Date, format: "1954-09-16" "1946-05-27" ...
#>  $ gender              : chr  "M" "F" "M" "M" ...
#>  $ party               : chr  "R" "D" "R" "D" ...
#>  $ leadership_role     : chr  NA NA NA NA ...
#>  $ twitter_account     : chr  "RepAbraham" "RepAdams" "Robert_Aderholt" "reppeteaguilar" ...
#>  $ facebook_account    : chr  "CongressmanRalphAbraham" "CongresswomanAdams" "RobertAderholt" "reppeteaguilar" ...
#>  $ youtube_account     : chr  NA NA "RobertAderholt" NA ...
#>  $ govtrack_id         : chr  "412630" "412607" "400004" "412615" ...
#>  $ cspan_id            : chr  "76236" "76386" "45516" "79994" ...
#>  $ votesmart_id        : chr  "155414" "5935" "441" "70114" ...
#>  $ icpsr_id            : chr  "21522" "21545" "29701" "21506" ...
#>  $ crp_id              : chr  "N00036633" "N00035451" "N00003028" "N00033997" ...
#>  $ google_entity_id    : chr  "/m/012dwd7_" "/m/02b45d" "/m/024p03" "/m/0jwv0xf" ...
#>  $ fec_candidate_id    : chr  "H4LA05221" "H4NC12100" "H6AL04098" "H2CA31125" ...
#>  $ url                 : chr  "https://abraham.house.gov" "https://adams.house.gov" "https://aderholt.house.gov" "https://aguilar.house.gov" ...
#>  $ rss_url             : chr  "https://abraham.house.gov/rss.xml" "https://adams.house.gov/rss.xml" "https://aderholt.house.gov/rss.xml" "https://aguilar.house.gov/rss.xml" ...
#>  $ contact_form        : logi  NA NA NA NA NA NA ...
#>  $ in_office           : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ dw_nominate         : num  0.524 -0.469 0.365 -0.288 0.671 NA 0.654 NA 0.38 NA ...
#>  $ ideal_point         : logi  NA NA NA NA NA NA ...
#>  $ seniority           : int  6 8 24 6 6 2 10 2 10 2 ...
#>  $ next_election       : int  2020 2020 2020 2020 2020 2020 2020 NA 2020 2020 ...
#>  $ total_votes         : int  507 507 507 507 507 507 430 77 507 507 ...
#>  $ missed_votes        : int  214 13 8 1 0 20 0 0 16 19 ...
#>  $ total_present       : int  0 0 0 0 0 0 5 3 0 0 ...
#>  $ last_updated        : POSIXct, format: "2019-07-25 14:00:22" "2019-07-25 14:00:24" ...
#>  $ ocd_id              : chr  "ocd-division/country:us/state:la/cd:5" "ocd-division/country:us/state:nc/cd:12" "ocd-division/country:us/state:al/cd:4" "ocd-division/country:us/state:ca/cd:31" ...
#>  $ office              : chr  "417 Cannon House Office Building" "2436 Rayburn House Office Building" "1203 Longworth House Office Building" "109 Cannon House Office Building" ...
#>  $ phone               : chr  "202-225-8490" "202-225-1510" "202-225-4876" "202-225-3201" ...
#>  $ fax                 : logi  NA NA NA NA NA NA ...
#>  $ state               : chr  "LA" "NC" "AL" "CA" ...
#>  $ district            : chr  "5" "12" "4" "31" ...
#>  $ at_large            : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ geoid               : chr  "2205" "3712" "0104" "0631" ...
#>  $ missed_votes_pct    : num  42.21 2.56 1.58 0.2 0 ...
#>  $ votes_with_party_pct: num  94.5 98.8 91.6 96.6 89.1 ...
```

## Installation

You can install the development version of ppcongress from
[GitHub](https://github.com/zamorarr/ppcongress) with:

``` r
remotes::install_github("zamorarr/ppcongress")
```

You also need to sign up to get an API Key from [ProPublica’s Data
Store](https://www.propublica.org/datastore/api/propublica-congress-api).
Put this key in your *.Renviron* file by opening it:

``` r
usethis::edit_r_environ()
```

Then adding the following line, replacing XXX with your own key.

``` sh
PROPUBLICA_API_KEY=XXX
```
