
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

This package provides tools to extract and manipulate data from the
[Armed Conflict Event and Location Data Project
(ACLED)](https://acleddata.com/).

To access ACLED data, register an account at
[developer.acleddata.com](developer.acleddata.com).

## Installation (for private repo)

Since the package repo is currently private, you need to tell R and
Github that you’re a collaborator. To do so, you first create a Github
personal access token (PAT). You can set this to expire after a certain
time (the default) or be permanent. We can initiate this process
internally via:

``` r
# install.packages("usethis") if not installed already
# create personal access token - this should redirect to your github page where you can copy the token
usethis::create_github_token()
```

After you’ve copied the PAT from the browser, return to R and run this,
which will store the PAT locally.

``` r
# paste the token where it says YourPAT
credentials::set_github_pat("YourPAT")
# if you run this, it should print your token; if not we've failed
Sys.getenv("GITHUB_PAT")
```

I recommend also setting the PAT in your `.Rprofile` so it’s stored for
all R sessions (i.e., you don’t have to save the PAT and paste it in
each time you re-install).

``` r
# to set your PAT for all R sessions, run
file.edit(file.path("~", ".Rprofile"))
# and then paste credentials::set_github_pat("YourPAT") into the .Rprofile script
# save the file
```

Now you can install the package and it will automatically locate your
PAT.

``` r
# install from github
devtools::install_github("billingtt/acledR")
```
