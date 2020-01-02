# chirps 0.0-3

## New features

* `get_esi` is added to retrieve Evaporative Stress Index whith S3 methods for "data.frame" and "sf"

* S3 methods for objects of class "data.frame" and "sf" in `get_chirps`

## Changes in behaviour

* `.get_request_progress` and a `while` condition are added to check the progress of large requests and prevent the function to fail.

* `.GET` is added as a general function to retrieve other datasets from ClimateSERV

* improvements in internal functions documentation 

# chirps 0.0-2

## New features

* Calculate precipitation indices with `precip_indices` over a time span

# chirps 0.0-1

* GitHub-only release of prototype package.