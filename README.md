driver
=========

An R API wrapper for Google Drive

__Author:__ Oliver Keyes<br/>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br/>
__Status:__ In development [![Build Status](https://travis-ci.org/Ironholds/driver.png?branch=master)](https://travis-ci.org/Ironholds/driver)

Description
======
_driver_ is a wrapper around the Google Drive API. It currently allows read-only access to file metadata, file content,
comments, replies and revisions. In the future it will also allow file uploads.

To install the development version:

    library(devtools)
    devtools::install_github("ironholds/driver")

Dependencies
======
* R (doy)
* [httr](http://cran.r-project.org/web/packages/httr/index.html) and its dependencies.
