
---
title: README.md
author: Iago Mosqueira, EC JRC G03
date: 19 August 2015
output:
  pdf_document:
    toc: true
    toc_depth: 2
    keep_md: true
rights:  Creative Commons Share Alike 4.0
---

# Working directory

Default Working Directory

As with the standard R GUI, RStudio employs the notion of a global default working directory. Normally this is the user home directory (typically referenced using ~ in R). When RStudio starts up it does the following:

    Executes the .Rprofile (if any) from the default working directory.
    Loads the .RData file (if any) from the default working directory into the workspace.
    Performs the other actions described in R Startup.

When RStudio exits and there are changes to the workspace, a prompt asks whether these changes should be saved to the .RData file in the current working directory.

This default behavior can be customized in the following ways using the RStudio Options dialog:

    Change the default working directory
    Enable/disable the loading of .RData from the default working directory at startup
    Specify whether .RData is always saved, never saved, or prompted for save at exit.



https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces


# Projects

https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects
