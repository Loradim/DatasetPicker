#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(grid)
if (!require(ggrepel)) {
    install.packages("ggrepel")
    library(ggrepel)
}

# server function for the app
shinyServer(function(input, output, session) {
    dat <- shiny::reactive({ 
        # search for datasets inside a package
        package <- input$package
        if (package == "") {
            package <- "datasets"
        }
        filter_class <- input$filterClass
        filter_name <- input$filterName
    
        out_package <- as.character()
        out_nameload <- as.character()
        out_namevar <- as.character()
        out_description <- as.character()
        out_class <- as.character()
        out_obs <- as.numeric()
        out_vars <- as.numeric()
        out_amount_NA <- as.numeric()
        
        oldwarning <- getOption("warn")
        options(warn = -1)
        
        datasets_in_package <- try(data(package = package), silent = TRUE)
        if (class(datasets_in_package) != "try-error") {
            
            # package exists, now look into all datasets in that package
            rc <- datasets_in_package$results
            df <- as.data.frame(rc)
            env <- new.env()
            for (i in 1:nrow(df)) {
                dataset <- df$Item[i]
                item <- as.character(dataset)
                reg_rc <- regmatches(item, regexec("(.*?) \\((.*)\\)", item, perl = TRUE))[[1]]
                okay <- TRUE
                if (length(reg_rc) == 0) {
                    # returned datasets equals data set name
                    name_load <- item
                    name_var <- item
                } else if (length(reg_rc) == 3) {
                    # returned datasets are different from data set name
                    name_load <- reg_rc[3]
                    name_var <- reg_rc[2]
                } else {
                    okay <- FALSE
                }
                if (okay) {
                    data(list = name_load, package = package, envir = env)
                    eval(parse(text = paste0("ds <- env$", name_var)))
                    cs <- class(ds)
                    
                    # try to harmonize class types
                    if (any(cs == "data.frame")) {
                        cs = "data.frame"                
                    } else if (any(cs == "matrix")) {
                        cs = "matrix"
                    } else if (any(cs == "ts")) {
                        cs = "ts"
                    }
                    
                    # depending on class type, get number of variables and observations
                    if (cs == "data.frame" | cs == "matrix") {
                        obs <- nrow(ds)
                        vars <- ncol(ds)
                    } else if (cs == "table" | cs == "array") {
                        df2 <- as.data.frame(ds)
                        obs <- nrow(ds)
                        vars <- ncol(ds)
                    } else if (cs == "ts" | cs == "numeric" | cs == "character" | cs == "logical" | cs == "factor") {
                        obs <- length(ds)
                        vars <- 1
                    } else if (cs == "dist") {
                        l <- length(attr(ds, "Labels"))
                        vars <- 1
                        obs <- l * (l-1)
                    } else if (cs == "list") {
                        vars <- 0
                        obs <- 0
                        for (l in ds) {
                            vars <- vars + 1
                            obs <- max(obs, length(l))
                        }
                    } else {
                        obs <- NA
                        vars <- NA
                    }
                    description <- df$Title[i]
                    
                    # save information per dataset
                    out_package <- c(out_package, package)
                    out_nameload <- c(out_nameload, name_load)
                    out_namevar <- c(out_namevar, name_var)
                    out_description <- c(out_description, as.character(description))
                    out_class <- c(out_class, paste0(cs, sep = "", collapse = ";"))
                    out_obs <- c(out_obs, obs)
                    out_vars <- c(out_vars, vars)
                }
            }
            rm(env)
        }
        
        # put all information together into one data.frame
        df_out <- data.frame(PackageName = as.character(out_package),
                             DatasetLoadname = as.character(out_nameload),
                             DatasetVarname = out_namevar,
                             Description = out_description,
                             Class = as.factor(out_class),
                             Observations = as.integer(out_obs),
                             Variables = as.integer(out_vars),
                             stringsAsFactors = FALSE)

        # update slider info based on datasets in the specified package
        oldPackage <- ifelse(is.null(session$userData$oldPackage), "", session$userData$oldPackage)
        NbrObs <- input$NbrObs
        NbrVars <- input$NbrVars
        
        if (oldPackage == input$package) {
            updateSliderInput(session = session, inputId = "NbrVars", min = min(df_out$Variables),
                              max = max(df_out$Variables))
            updateSliderInput(session = session, inputId = "NbrObs", min = min(df_out$Observations),
                              max = max(df_out$Observations))
        } else {
            # package name has changed: reset slider and min / max values
            updateSliderInput(session = session, inputId = "NbrVars", min = min(df_out$Variables),
                              max = max(df_out$Variables), value = c(min(df_out$Variables),
                                                                     max(df_out$Variables)))
            updateSliderInput(session = session, inputId = "NbrObs", min = min(df_out$Observations),
                              max = max(df_out$Observations), value = c(min(df_out$Observations),
                                                                        max(df_out$Observations)))
            NbrObs <- c(min(df_out$Observations), max(df_out$Observations))
            NbrVars <- c(min(df_out$Variables), max(df_out$Variables))
            session$userData$oldPackage = input$package
        }
        
        # apply filters to the output
        df_out <- df_out[grep(filter_class, df_out$Class), ]
        lines <- unique(c(grep(filter_name, df_out$DatasetLoadname, ignore.case = TRUE),
                          grep(filter_name, df_out$DatasetVarname, ignore.case = TRUE),
                          grep(filter_name, df_out$Description, ignore.case = TRUE))
        )
        df_out <- df_out[lines, ]
        df_out <- df_out[df_out$Observations >= NbrObs[1] & df_out$Observations <= NbrObs[2], ]
        df_out <- df_out[df_out$Variables >= NbrVars[1] & df_out$Variables <= NbrVars[2], ]        

        options(warn = oldwarning)
        # output our gathered information
        df_out
    })
    
    # render plot as overview
    output$plot <- shiny::renderPlot({
        package <- input$package
        if (package == "") {
            package <- "datasets"
        }
        g <- ggplot(data = dat(), aes(x = Observations, y = Variables, color = Class)) +
            geom_point() +
            scale_x_continuous(trans = "log10") +
            scale_y_continuous(trans = "log10") +
            geom_text_repel(aes(label=DatasetLoadname)) +
            ggtitle(paste0("Package '", package, "': ", dim(dat())[1], " matching datasets found",
                           sep = "", collapse = "")) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.direction = "horizontal", legend.position = "bottom") +
            labs(x = "Number of observations (logscale)") +
            labs(y = "Number of Variables (logscale)")
        if (!package %in% installed.packages()[,1]) {
            g <- g + annotation_custom(textGrob(paste0("Package '", package, "' not installed on server")), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 
        }
        g
    })
    
    # output data in table format
    output$table <- shiny::renderDataTable({ as.data.frame(dat()) })
})
