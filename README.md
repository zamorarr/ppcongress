
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/zamorarr/ppcongress.svg?branch=master)](https://travis-ci.org/zamorarr/ppcongress)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/zamorarr/ppcongress?branch=master&svg=true)](https://ci.appveyor.com/project/zamorarr/ppcongress)

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

## Example

``` r
library(ppcongress)
```

### Members

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
#> Date in ISO8601 format; converting timezone from UTC to "America/New_York".
print(df)
#> # A tibble: 442 x 44
#>    id    title short_title api_uri first_name middle_name last_name suffix
#>    <chr> <chr> <chr>       <chr>   <chr>      <chr>       <chr>     <chr> 
#>  1 A000… Repr… Rep.        https:… Ralph      <NA>        Abraham   <NA>  
#>  2 A000… Repr… Rep.        https:… Alma       <NA>        Adams     <NA>  
#>  3 A000… Repr… Rep.        https:… Robert     B.          Aderholt  <NA>  
#>  4 A000… Repr… Rep.        https:… Pete       <NA>        Aguilar   <NA>  
#>  5 A000… Repr… Rep.        https:… Rick       <NA>        Allen     <NA>  
#>  6 A000… Repr… Rep.        https:… Colin      <NA>        Allred    <NA>  
#>  7 A000… Repr… Rep.        https:… Justin     <NA>        Amash     <NA>  
#>  8 A000… Repr… Rep.        https:… Justin     <NA>        Amash     <NA>  
#>  9 A000… Repr… Rep.        https:… Mark       <NA>        Amodei    <NA>  
#> 10 A000… Repr… Rep.        https:… Kelly      <NA>        Armstrong <NA>  
#> # … with 432 more rows, and 36 more variables: date_of_birth <date>,
#> #   gender <chr>, party <chr>, leadership_role <chr>,
#> #   twitter_account <chr>, facebook_account <chr>, youtube_account <chr>,
#> #   govtrack_id <chr>, cspan_id <chr>, votesmart_id <chr>, icpsr_id <chr>,
#> #   crp_id <chr>, google_entity_id <chr>, fec_candidate_id <chr>,
#> #   url <chr>, rss_url <chr>, contact_form <lgl>, in_office <lgl>,
#> #   dw_nominate <dbl>, ideal_point <lgl>, seniority <int>,
#> #   next_election <int>, total_votes <int>, missed_votes <int>,
#> #   total_present <int>, last_updated <dttm>, ocd_id <chr>, office <chr>,
#> #   phone <chr>, fax <lgl>, state <chr>, district <chr>, at_large <lgl>,
#> #   geoid <chr>, missed_votes_pct <dbl>, votes_with_party_pct <dbl>
```

### Bills

You can get a list of all upcoming bills in the house:

``` r
json <- bills_upcoming("house")
print(json)
#> <propublica congress/v1/bills/upcoming/house.json>
#> List of 2
#>  $ date : chr "2019-07-31"
#>  $ bills:List of 43
```

``` r
as_tibble(json)
#> Date in ISO8601 format; converting timezone from UTC to "America/New_York".
#> # A tibble: 43 x 16
#>    congress chamber bill_id bill_slug bill_type bill_number api_uri
#>       <int> <chr>   <chr>   <chr>     <chr>     <chr>       <chr>  
#>  1      116 house   hr3877… hr3877    hr        H.R.3877    https:…
#>  2      116 house   hr397-… hr397     hr        H.R.397     https:…
#>  3      116 house   hr3239… hr3239    hr        H.R.3239    https:…
#>  4      116 house   hr2203… hr2203    hr        H.R.2203    https:…
#>  5      116 house   hres50… hres507   hres      H.RES.507   https:…
#>  6      116 house   hr3670… hr3670    hr        H.R.3670    https:…
#>  7      116 house   hr2336… hr2336    hr        H.R.2336    https:…
#>  8      116 house   hr3868… hr3868    hr        H.R.3868    https:…
#>  9      116 house   hr1850… hr1850    hr        H.R.1850    https:…
#> 10      116 house   hres35… hres358   hres      H.RES.358   https:…
#> # … with 33 more rows, and 9 more variables: legislative_day <date>,
#> #   scheduled_at <dttm>, range <chr>, context <chr>, description <chr>,
#> #   bill_url <chr>, consideration <chr>, source_type <chr>, url <chr>
```
