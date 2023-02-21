library(ggplot2)
library(stringr)

boot_sd_ci <- function(x, confidence = 95, itr = 1000) {
  
  # init iterations and sample
  i <- itr
  bt_avg <- NULL
  
  # loop over iterations
  while (i > 0) {
    # sample randomly from x
    spl <- sample(x, length(x), replace = TRUE)
    
    # store mean
    bt_avg <- c(bt_avg, sum(spl)/length(spl))
    
    # decrement i
    i <- i-1
  }
  
  # mean over the bootstrapped samples
  bt_est <- sum(bt_avg)/itr
  
  # compute standard deviation
  bt_sd <- sqrt((1/(length(x)-1)) * sum((bt_avg-bt_est)^2))
  
  # compute confidence interval
  # sorting bt_avg numerically
  st_avg <- sort(bt_avg)
  
  # get the first value after the 2.5 first centiles
  bt_95ci_inf <- st_avg[floor(0.5*(1-0.01*confidence)*itr)+1]
  
  # get the last value before the 2.5 last centiles
  bt_95ci_sup <- st_avg[floor((0.01*confidence+0.5*(1-0.01*confidence))*itr)-1]
  
  res <- c(bt_sd, bt_95ci_inf, bt_95ci_sup)
  return(res) 
  
}

Path = "C:/Users/adb3/Desktop/PhD/GitKraken/Chapter3sim/"
folder = "C:/Users/adb3/Desktop/PhD/GitKraken/Chapter3sim/folder-cullBatch1/"
# Path = "/home/adrian/Documents/GitKraken/Chapter3sim/"
# folder = "/home/adrian/Documents/GitKraken/Chapter3sim/folder-cullBatch1/"
Keyword = "Results"
Pattern = "cullBatch1-p"
freqTrials = 10
exclude.ext = FALSE

path = Path; keyword = Keyword; pattern = Pattern; 

mergeResults <- function(path, keyword = c("Results", "Snapshot"), pattern) {
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # loop over the folders
  for (i in 1:length(content)) {
    
    # path to folder
    # folder = paste("~/", path, content[i], sep = "")
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", folder))
    
    # get content
    simFol = list.files(folder)
    
    # select only folders
    simFol <- grep(pattern = c(pattern), x = simFol, value = T)
    
    # loop over the sim folders
    for (j in 1:length(simFol)) {
      
      # get results folder
      # resFol <- paste(folder, simFol[j], sep = "/")
      resFol <- paste(folder, simFol[j], sep = "")
      print(paste("in sim folder ", resFol))
      
      # get files
      results <- list.files(resFol)
      
      # only csv files
      results <- grep(pattern = c(".csv"), x = results, value = T)
      
      # only the results files
      results <- grep(pattern = c(keyword), x = results, value = T)
      
      # if(length(results[m])==0) {next}
      
      # loop over the results files
      for (m in 1:length(results)) {
        
        # print(paste("reading file ", results[m]))
        # read results
        res <- read.csv(paste(resFol, results[m], sep = "/"))
        
        # if first round, create merge table
        if (m == 1) {
          
          # create an empty table
          headers <- colnames(res)
          headers <- c("replicate", colnames(res), "prey1growthRate", "prey2growthRate", "predatorGrowthRate", "prey1catchRate", "prey2catchRate")
          tab <- data.frame(matrix(ncol = length(headers), nrow = 0))
        }
        
        # bind the replicate number to the table
        replicate <- rep(m, times = dim(res)[1])
        res <- cbind(replicate, res)
        
        # Compute growth rate
        prey1growthRate <- 0
        prey2growthRate <- 0
        predatorGrowthRate <- 0
        prey1catchRate <- 0
        prey2catchRate <- 0
        
        # loop over the lines
        for (k in 2:dim(res)[1]) {
          prey1growthRate    <- c(prey1growthRate, ifelse(res$prey1PopulationSize[k-1] != 0, (res$prey1PopulationSize[k]-res$prey1PopulationSize[k-1])/res$prey1PopulationSize[k-1], 0))
          prey2growthRate    <- c(prey2growthRate, ifelse(res$prey2PopulationSize[k-1] != 0, (res$prey2PopulationSize[k]-res$prey2PopulationSize[k-1])/res$prey2PopulationSize[k-1], 0))
          predatorGrowthRate <- c(predatorGrowthRate, ifelse(res$predator1PopulationSize[k-1] != 0, (res$predator1PopulationSize[k]-res$predator1PopulationSize[k-1])/res$predator1PopulationSize[k-1], 0))
          prey1catchRate     <- c(prey1catchRate, ifelse(res$prey1PopulationSize[k-1] != 0, res$prey1catches[k]/res$prey1PopulationSize[k-1], 0))
          prey2catchRate     <- c(prey2catchRate, ifelse(res$prey2PopulationSize[k-1] != 0, res$prey2catches[k]/res$prey2PopulationSize[k-1], 0))
        } # end loop over lines
        
        res <- cbind(res, prey1growthRate, prey2growthRate, predatorGrowthRate, prey1catchRate, prey2catchRate)
        tab <- rbind(tab, res)
        
        # delete original file when finished
        file.remove(paste(resFol, results[m], sep = "/"))
        
      } # end loop over files (replicates)
      
      # name columns
      colnames(tab) <- headers
      
      # create stats folder and save table
      newFolderDir <- paste(resFol, "/stats", sep = "")
      dir.create(path = newFolderDir)
      write.csv(tab, file = paste(newFolderDir, "/merged", keyword, ".csv", sep = ""), row.names = FALSE)
      
    } # end loop over sim folders
    
  } # end loop over folders
  
} # end of function

# statsResults <- function(path, keyword = c("Results", "Snapshot"), pattern, freqTrials) {
#   
#   # get the directory content
#   # content <- list.files(paste("~/", path, sep = ""))
#   content <- list.files(path)
#   
#   # order alphabetically
#   content <- content[order(content)]
#   
#   # only the folders
#   content <- grep(pattern = c("folder"), x = content, value = T)
#   
#   # loop over the local SA folders
#   for (i in 1:length(content)) {
#     
#     # path to folder
#     folder = paste(path, content[i], sep = "")
#     print(paste("in localSA folder ", folder))
#     
#     # create a global stats folder
#     dir.create(path = paste(folder, "allStatsAndPlots", sep = "/"))
#     
#     # create a Results folder
#     dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
#     
#     # get content
#     simFol = list.files(folder)
#     
#     # select only folders
#     simFol <- grep(pattern = c(pattern), x = simFol, value = T)
#     
#     # loop over the sim folders
#     for (j in 1:length(simFol)) {
#       
#       print(paste("in sim folder ", simFol[j]))
#       
#       # # rename the file
#       # cux <- list.files(paste(path, content[i], simFol[j], sep = "/"))
#       # cux <- grep(pattern = c("stats"), x = cux, value = T)
#       # file.rename(paste(path, content[i], simFol[j], cux, sep = "/"), paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = ""))
#       
#       # path to folder
#       # statsFol = paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = "")
#       # statsFol = paste(folder, simFol[j], "/stats", sep = "/")
#       statsFol = paste(folder, simFol[j], "/stats", sep = "")
#       # statsFol = paste(path, simFol[j], "/stats-", simFol[j], sep = "")
#       
#       # file.rename(paste(statsFol, results, sep = "/"), paste(statsFol, "/merged", keyword, "-", simFol[j], ".csv", sep = ""))
#       
#       # get files
#       results <- list.files(statsFol)
#       
#       # only csv files
#       results <- grep(pattern = c("merged"), x = results, value = T)
#       
#       # only the results files
#       results <- grep(pattern = c(keyword), x = results, value = T)
#       
#       # get name
#       name <- paste(statsFol, "/", results, sep = "")
#       
#       # read table
#       stats <- read.csv(name)
#       
#       # if(length(results[m])==0) {next}
#       
#       ## create table
#       
#       # new headers
#       headers <- colnames(stats)
#       newHeaders <- c(headers[2], "repNb")
#       for (k in 3:length(headers)) {
#         newHeaders <- c(newHeaders, paste(headers[k], "Mean", sep = ""), paste(headers[k], "ICinf", sep = ""), paste(headers[k], "ICsup", sep = ""))
#       }
#       
#       # create an empty table 
#       tab <- data.frame(matrix(ncol = length(newHeaders), nrow = 0)) 
#       
#       # first line
#       # subset per ts number
#       sub <- subset(stats, stats$timeStep == as.numeric(0))
#       
#       # initiate new line with time step
#       newLine <- c(0, dim(sub)[1])
#       
#       # apply stats to each column of measures
#       for (m in 3:(dim(sub)[2])) { # first line is rep, second is ts
#         newLine <- c(newLine, mean(sub[,m]), boot_sd_ci(sub[,m])[2], boot_sd_ci(sub[,m])[3])
#       }
#       
#       # rbind the line to tab
#       tab <- rbind(tab, newLine)
#       
#       # for loop making subset for each time step except 0
#       ts <- levels(as.factor(stats$timeStep))[-1]
#       prevFinDens <- sub[,c(5, 6, 9)]
#       
#       for (k in 1:length(ts)) {
#         
#         if (k %% 10 == 0) {
#           
#           # subset per ts number
#           subTS <- subset(stats, stats$timeStep == as.numeric(ts[k]))
#           
#           # subset per ts number
#           sub <- subset(stats, stats$timeStep %in% c((as.numeric(ts[k]) - freqTrials + 1) : as.numeric(ts[k])))
#           
#           # initiate new line with time step
#           newLine <- c(as.numeric(ts[k]), max(sub$replicate))
#           
#           # print(paste("newline is ", length(newLine), " and should be 2"))
#           
#           # apply stats to each column of measures
#           for (m in 3:6) { # first line is rep, second is ts
#             newLine <- c(newLine, mean(subTS[,m]), boot_sd_ci(subTS[,m])[2], boot_sd_ci(subTS[,m])[3])
#           }
#           
#           # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3))
#           
#           # column 7 and 8 catches count. Averaged over the last 10 ts
#           for (m in 7:8) { 
#             
#             sumCatchesVec <- NULL
#             
#             for (n in 1:max(sub$replicate)) {
#               # sum of the catches in a moving + feeding sequence for each replicates
#               sumCatchesVec <- c(sumCatchesVec, sum(sub[which(sub$replicate==n),m]))
#             }
#             
#             newLine <- c(newLine, mean(sumCatchesVec), boot_sd_ci(sumCatchesVec)[2], boot_sd_ci(sumCatchesVec)[3])
#           }
#           
#           # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3))
#           
#           # column 9 is predator population size
#           newLine <- c(newLine, mean(subTS[,9]), boot_sd_ci(subTS[,9])[2], boot_sd_ci(subTS[,9])[3])
#           
#           # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3))
#           
#           # column 10 to 11 are preys growth rates.
#           for (m in 10:11) { 
#             if (length(which(sub[,m]==0)) >= max(sub$replicate)*freqTrials-max(sub$replicate)) {
#               newLine <- c(newLine, mean(subTS[,m]), boot_sd_ci(subTS[,m])[2:3])
#             } else {
#               grVec <- ifelse(prevFinDens[m-9] != 0, (subTS[,m-5]-prevFinDens[, m-9])/prevFinDens[, m-9], rep(0, max(sub$replicate)))
#               newLine <- c(newLine, mean(grVec), boot_sd_ci(grVec)[2:3])
#             }
#           }
#           
#           # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3 + 2*3))
#           
#           # column 12 is predator growth rate
#           newLine <- c(newLine, mean(subTS[,12]), boot_sd_ci(subTS[,12])[2], boot_sd_ci(subTS[,12])[3])
#           
#           # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3 + 2*3 + 3))
#           
#           # column 13 to 14 are preys catch rates. Averaged over the last 10 ts
#           for (m in 13:14) { # first line is rep, second is ts
#             newLine <- c(newLine, mean(sub[,m]), boot_sd_ci(sub[,m])[2], boot_sd_ci(sub[,m])[3])
#           }
#           
#           # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3 + 2*3 + 3 + 2*3, "(38)"))
#           
#           # rbind the line to tab
#           tab <- rbind(tab, newLine)
#           
#           # update prevFinDens
#           prevFinDens <- subTS[,c(5, 6, 9)]
#         }
#         
#       } # end loop over time steps
#       
#       # column names
#       colnames(tab) <- newHeaders
#       
#       # write in the corresponding folder
#       write.csv(tab, file = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), row.names = FALSE)
#       
#       # and copy in the global stats folder
#       file.copy(from = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
#       
#       # plot and save density figure
#       data <- tab
#       
#       x = data$timeStep/10
#       y1 = data$prey1PopulationSizeMean
#       y2 = data$prey2PopulationSizeMean
#       y3 = data$predator1PopulationSizeMean
#       y1min = data$prey1PopulationSizeICinf
#       y2min = data$prey2PopulationSizeICinf
#       y3min = data$predator1PopulationSizeICinf
#       y1max = data$prey1PopulationSizeICsup
#       y2max = data$prey2PopulationSizeICsup
#       y3max = data$predator1PopulationSizeICsup
#       y1c = "red"
#       y2c = "blue"
#       y3c = "orange"
#       tIntro = 210
#       
#       fig <- ggplot(data, aes(x)) + 
#         # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
#         geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
#         geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
#         geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
#         geom_line(aes(y = y1), color = y1c) +
#         geom_line(aes(y = y2), color = y2c) +
#         geom_line(aes(y = y3), color = y3c) +
#         # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
#         # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
#         # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
#         labs(x = "Generations", y = "Population density") +
#         scale_colour_manual(name='Populations',
#                             breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                             values=c(y1c, y2c, y3c))
#       
#       # save plot in this folder
#       ggsave(filename = paste("stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
#       
#       # and copy in the global stats folder
#       file.copy(from = paste(statsFol, "/stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), to = paste(folder, "allStatsAndPlots/", keyword, "Files", sep = ""))
#       
#       # plot and save catch rate figure
#       # data <- read.csv(paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""))
# 
#       x = data$timeStep/10
#       y1 = data$prey1catchRateMean
#       y2 = data$prey2catchRateMean
#       # y3 = data$predatorCatchRateMean
#       y1min = data$prey1catchRateICinf
#       y2min = data$prey2catchRateICinf
#       # y3min = data$predatorCatchRateICinf
#       y1max = data$prey1catchRateICsup
#       y2max = data$prey2catchRateICsup
#       # y3max = data$predatorCatchRateICsup
#       y1c = "red"
#       y2c = "blue"
#       y3c = "orange"
#       tIntro = 210
# 
#       fig <- ggplot(data, aes(x)) +
#         # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 1.05*min(c(min(y1min), min(y2min), min(y3min))), ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
#         geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
#         geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
#         # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
#         geom_line(aes(y = y1), color = y1c) +
#         geom_line(aes(y = y2), color = y2c) +
#         # geom_line(aes(y = y3), color = y3c) +
#         # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
#         # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
#         # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
#         labs(x = "Generations", y = "Average catch rate between trials") +
#         scale_colour_manual(name='Populations',
#                             breaks=c('Prey 1', 'Prey 2', 'Predator'),
#                             values=c(y1c, y2c, y3c))
# 
#       # save plot in this folder
#       ggsave(filename = paste("stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
# 
#       # and copy in the global stats folder
#       file.copy(from = paste(statsFol, "/stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
#       
#     } # end loop over sim folders
#     
#   } # end loop over local SA folders
#   
# }

exclude.ext = TRUE

statsResultsNoExt <- function(path, keyword = c("Results", "Snapshot"), pattern, freqTrials, exclude.ext = FALSE) {
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # loop over the local SA folders
  for (i in 1:length(content)) {
    
    # path to folder
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", folder))
    
    # create a global stats folder
    dir.create(path = paste(folder, "allStatsAndPlots", sep = "/"))
    
    # create a Results folder
    ifelse(exclude.ext == FALSE, 
           dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = "")),
           dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", "-woExt", sep = "")))
    
    # get content
    simFol = list.files(folder)
    
    # select only folders
    simFol <- grep(pattern = c(pattern), x = simFol, value = T)
    
    # loop over the sim folders
    for (j in 1:length(simFol)) {  
      
      print(paste("in sim folder ", simFol[j]))
      
      # # rename the file
      # cux <- list.files(paste(path, content[i], simFol[j], sep = "/"))
      # cux <- grep(pattern = c("stats"), x = cux, value = T)
      # file.rename(paste(path, content[i], simFol[j], cux, sep = "/"), paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = ""))
      
      # path to folder
      # statsFol = paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = "")
      # statsFol = paste(path, simFol[j], "/stats-", simFol[j], sep = "")
      statsFol = paste(folder, simFol[j], "stats", sep = "/")
      
      # file.rename(paste(statsFol, results, sep = "/"), paste(statsFol, "/merged", keyword, "-", simFol[j], ".csv", sep = ""))
      
      # get files
      results <- list.files(statsFol)
      
      # only csv files
      results <- grep(pattern = c("merged"), x = results, value = T)
      
      # only the results files
      results <- grep(pattern = c(keyword), x = results, value = T)
      
      # # rm false file and rename true file
      # if (length(results) > 1) {
      #   if  (grep(pattern = c("rep"), x = results[2]) == 1) {
      #     file.remove(paste(statsFol, grep(pattern = c("rep"), x = results, value = T), sep = "/"))
      #     file.rename(paste(statsFol, grep(pattern = c("rep"), x = results, value = T, invert = TRUE), sep = "/"), paste(statsFol, grep(pattern = c("rep"), x = results, value = T, invert = TRUE), sep = "/"))
      #   } else {
      #     file.remove(paste(statsFol, grep(pattern = c("NA"), x = results, value = T), sep = "/"))
      #   }
      #   
      #   # get files
      #   results <- list.files(statsFol)
      #   
      #   # only csv files
      #   results <- grep(pattern = c("merged"), x = results, value = T)
      #   
      #   # only the results files
      #   results <- grep(pattern = c(keyword), x = results, value = T)
      # }
      
      # get name
      name <- paste(statsFol, "/", results, sep = "")
      
      # read table
      stats <- read.csv(name)
      
      ## create table
      
      # new headers
      headers <- colnames(stats)
      newHeaders <- c(headers[2], "repNb")
      for (k in 3:length(headers)) {
        newHeaders <- c(newHeaders, paste(headers[k], "Mean", sep = ""), paste(headers[k], "ICinf", sep = ""), paste(headers[k], "ICsup", sep = ""))
      }
      
      # create an empty table 
      tab <- data.frame(matrix(ncol = length(newHeaders), nrow = 0))
      
      if (exclude.ext == TRUE) {
        # create temporary table
        temp <- data.frame(matrix(ncol = length(newHeaders), nrow = 0))
        
        # subset merged results and move to temp only if no extinction
        repNb <- levels(as.factor(stats$replicate))
        
        for (m in 1:length(repNb)) {
          sub <- subset(stats, stats$replicate == repNb[m])
          
          if (sub$prey1PopulationSize[dim(sub)[1]] == 0 | sub$prey2PopulationSize[dim(sub)[1]] == 0 | sub$predator1PopulationSize[dim(sub)[1]] == 0) {
            print(paste("replicate", m, "ended in extinction"))
          } else {
            temp <- rbind(temp, sub)
          }
        }  
      }  
        
      # if temp is still empty skip to the next j loop
      if (exclude.ext == TRUE & dim(temp)[1] == 0) {
        print("extinctions only -- move to next sim folder") # }  
        
      } else {
      
        if (exclude.ext == TRUE) {
          # otherwise replace stats by temp
          stats <- temp
        }
        
        # first line
        # subset per ts number
        sub <- subset(stats, stats$timeStep == as.numeric(0))
        
        # initiate new line with time step
        newLine <- c(0, dim(sub)[1])
        
        # apply stats to each column of measures
        for (m in 3:(dim(sub)[2])) { # first line is rep, second is ts
          newLine <- c(newLine, mean(sub[,m]), boot_sd_ci(sub[,m])[2], boot_sd_ci(sub[,m])[3])
        }
        
        # rbind the line to tab
        tab <- rbind(tab, newLine)
        
        # for loop making subset for each time step except 0
        ts <- levels(as.factor(stats$timeStep))[-1]
        prevFinDens <- sub[,c(5, 6, 9)]
        
        for (k in 1:length(ts)) {
          
          if (k %% 10 == 0) {
            
            # subset per ts number
            subTS <- subset(stats, stats$timeStep == as.numeric(ts[k]))
            
            # subset per ts number
            sub <- subset(stats, stats$timeStep %in% c((as.numeric(ts[k]) - freqTrials + 1) : as.numeric(ts[k])))
            
            # initiate new line with time step
            newLine <- c(as.numeric(ts[k]), max(sub$replicate))
            
            # print(paste("newline is ", length(newLine), " and should be 2"))
            
            # apply stats to each column of measures
            for (m in 3:6) { # first line is rep, second is ts
              newLine <- c(newLine, mean(subTS[,m]), boot_sd_ci(subTS[,m])[2], boot_sd_ci(subTS[,m])[3])
            }
            
            # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3))
            
            # column 7 and 8 catches count. Averaged over the last 10 ts
            for (m in 7:8) { 
              
              sumCatchesVec <- NULL
              
              for (n in 1:max(sub$replicate)) {
                # sum of the catches in a moving + feeding sequence for each replicates
                sumCatchesVec <- c(sumCatchesVec, sum(sub[which(sub$replicate==n),m]))
              }
              
              newLine <- c(newLine, mean(sumCatchesVec), boot_sd_ci(sumCatchesVec)[2], boot_sd_ci(sumCatchesVec)[3])
            }
            
            # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3))
            
            # column 9 is predator population size
            newLine <- c(newLine, mean(subTS[,9]), boot_sd_ci(subTS[,9])[2], boot_sd_ci(subTS[,9])[3])
            
            # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3))
            
            # column 10 to 11 are preys growth rates.
            for (m in 10:11) { 
              if (length(which(sub[,m]==0)) >= max(sub$replicate)*freqTrials-max(sub$replicate)) {
                newLine <- c(newLine, mean(subTS[,m]), boot_sd_ci(subTS[,m])[2:3])
              } else {
                grVec <- ifelse(prevFinDens[m-9] != 0, (subTS[,m-5]-prevFinDens[, m-9])/prevFinDens[, m-9], rep(0, max(sub$replicate)))
                newLine <- c(newLine, mean(grVec), boot_sd_ci(grVec)[2:3])
              }
            }
            
            # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3 + 2*3))
            
            # column 12 is predator growth rate
            newLine <- c(newLine, mean(subTS[,12]), boot_sd_ci(subTS[,12])[2], boot_sd_ci(subTS[,12])[3])
            
            # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3 + 2*3 + 3))
            
            # column 13 to 14 are preys catch rates. Averaged over the last 10 ts
            for (m in 13:14) { # first line is rep, second is ts
              newLine <- c(newLine, mean(sub[,m]), boot_sd_ci(sub[,m])[2], boot_sd_ci(sub[,m])[3])
            }
            
            # print(paste("newline is ", length(newLine), " and should be ", 2 + 4*3 + 2*3 + 3 + 2*3 + 3 + 2*3, "(38)"))
            
            # rbind the line to tab
            tab <- rbind(tab, newLine)
            
            # update prevFinDens
            prevFinDens <- subTS[,c(5, 6, 9)]
          }
          
        } # end loop over time steps
        
        # column names
        colnames(tab) <- newHeaders
        
        if (exclude.ext == FALSE) {
          
          # write in the corresponding folder
          write.csv(tab, file = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), row.names = FALSE) 
          
          # and copy in the global stats folder
          file.copy(from = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
        } else {
          
          # write directly in the global folder
          write.csv(tab, file = paste(folder, "/allStatsAndPlots/", keyword, "Files-woExt/woExt-", simFol[j], ".csv", sep = ""), row.names = FALSE)
        } 
        
        # plot and save density figure
        data <- tab
        
        x = data$timeStep/10
        y1 = data$prey1PopulationSizeMean
        y2 = data$prey2PopulationSizeMean
        y3 = data$predator1PopulationSizeMean
        y1min = data$prey1PopulationSizeICinf
        y2min = data$prey2PopulationSizeICinf
        y3min = data$predator1PopulationSizeICinf
        y1max = data$prey1PopulationSizeICsup
        y2max = data$prey2PopulationSizeICsup
        y3max = data$predator1PopulationSizeICsup
        y1c = "red"
        y2c = "blue"
        y3c = "orange"
        tIntro = 210
        
        fig <- ggplot(data, aes(x)) + 
          # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 0, ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
          geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
          geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
          geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
          geom_line(aes(y = y1), color = y1c) +
          geom_line(aes(y = y2), color = y2c) +
          geom_line(aes(y = y3), color = y3c) +
          # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
          # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
          # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
          labs(x = "Generations", y = "Population density") +
          scale_colour_manual(name='Populations',
                              breaks=c('Prey 1', 'Prey 2', 'Predator'),
                              values=c(y1c, y2c, y3c))
        
        if (exclude.ext == FALSE) {
          
          # save plot in this folder
          ggsave(filename = paste("stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
          
          # and copy in the global stats folder
          file.copy(from = paste(statsFol, "/stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
        } else {
          
          # save plot in global folder
          ggsave(filename = paste("woExt-stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), path = paste(folder, "/allStatsAndPlots/", keyword, "Files-woExt", sep = ""), plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
        }
        
        # plot and save catch rate figure
        # data <- read.csv(paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""))
        
        x = data$timeStep/10
        y1 = data$prey1catchRateMean
        y2 = data$prey2catchRateMean
        # y3 = data$predatorCatchRateMean
        y1min = data$prey1catchRateICinf
        y2min = data$prey2catchRateICinf
        # y3min = data$predatorCatchRateICinf
        y1max = data$prey1catchRateICsup
        y2max = data$prey2catchRateICsup
        # y3max = data$predatorCatchRateICsup
        y1c = "red"
        y2c = "blue"
        y3c = "orange"
        tIntro = 210
        
        fig <- ggplot(data, aes(x)) +
          # geom_rect(aes(xmin = 0, xmax = tIntro, ymin = 1.05*min(c(min(y1min), min(y2min), min(y3min))), ymax = 1.05*max(c(max(y1max), max(y2max), max(y3max)))), alpha=0.5, fill = "lightgrey") +
          geom_ribbon(aes(ymin = y1min, ymax = y1max), alpha = 0.2, size = 0.1, col = y1c, fill = y1c) +
          geom_ribbon(aes(ymin = y2min, ymax = y2max), alpha = 0.2, size = 0.1, col = y2c, fill = y2c) +
          # geom_ribbon(aes(ymin = y3min, ymax = y3max), alpha = 0.2, size = 0.1, col = y3c, fill = y3c) +
          geom_line(aes(y = y1), color = y1c) +
          geom_line(aes(y = y2), color = y2c) +
          # geom_line(aes(y = y3), color = y3c) +
          # geom_point(aes(y = y1), size = 2.5, shape = 21, fill = "white", color = y1c) +
          # geom_point(aes(y = y2), size = 2.5, shape = 22, fill = "white", color = y2c) +
          # geom_point(aes(y = y3), size = 2.5, shape = 24, fill = "white", color = y3c) +
          labs(x = "Generations", y = "Average catch rate between trials") +
          scale_colour_manual(name='Populations',
                              breaks=c('Prey 1', 'Prey 2', 'Predator'),
                              values=c(y1c, y2c, y3c))
        
        if (exclude.ext == FALSE) {
          # save plot in this folder
          ggsave(filename = paste("stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
          
          # and copy in the global stats folder
          file.copy(from = paste(statsFol, "/stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
        } else {
          # save plot in global folder
          ggsave(filename = paste("woExt-stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), path = paste(folder, "/allStatsAndPlots/", keyword, "Files-woExt", sep = ""), plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
        }
        
      } # end of else loop
      
    } # end loop over sim folders
    
  } # end loop over local SA folders
  
} # end of function

cullResults <- function(path, keyword = c("Results", "Snapshot"), pattern) {
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # create table 
  headers <- c("prey1cullQuota", "predcullQuota", # "prey2catchProb", "prey2maxCons", "prey2resAva", 
               "replicatesNb",
               "prey1extFreq", "prey2extFreq", "pred1extFreq",
               # "prey1densBeforeMean", "prey1densBeforeMax", "prey1densBeforeMin",
               # "prey2densBeforeMean", "prey2densBeforeMax", "prey2densBeforeMin",
               # "prey1growthBeforeMean", "prey1growthBeforeMax", "prey1growthBeforeMin",
               # "prey2growthBeforeMean", "prey2growthBeforeMax", "prey2growthBeforeMin",
               "prey1densAfterMean", "prey1densAfterMax", "prey1densAfterMin",
               "prey2densAfterMean", "prey2densAfterMax", "prey2densAfterMin",
               "predatorDensMean", "predatorDensMax", "predatorDensMin",
               "prey1growthAfterMean", "prey1growthAfterMax", "prey1growthAfterMin",
               "prey2growthAfterMean", "prey2growthAfterMax", "prey2growthAfterMin",
               "predatorGrowthMean", "predatorGrowthMax", "predatorGrowthMin",
               "prey1catchesMean", "prey1catchesMax", "prey1catchesMin",
               "prey2catchesMean", "prey2catchesMax", "prey2catchesMin",
               "prey1catchRateMean", "prey1catchRateMax", "prey1catchRateMin",
               "prey2catchRateMean", "prey2catchRateMax", "prey2catchRateMin")
  
  # set before after intervals
  tIntro = 301
  tEnd = 2000
  before <- c(tIntro-100, tIntro)
  after <- c(tEnd-500, tEnd)
  
  # loop over the folders
  for (i in 1:length(content)) {
    
    # path to folder
    # folder = paste("~/", path, content[i], sep = "")
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", i, ": ", folder))# get the value of XparamName
    
    tab <- data.frame(matrix(ncol = length(headers), nrow = 0))
    
    # # name columns
    # colnames(tab) <- headers
    
    # get content
    simFol = list.files(folder)
    
    # select only folders
    simFol <- grep(pattern = c(pattern), x = simFol, value = T)
    
    # loop over the sim folders
    for (j in 1:length(simFol)) {
      
      # get results folder
      resFol <- paste(folder, simFol[j], sep = "")
      print(paste("in sim folder ", j, ": ", resFol))
      
      # find stats folder and store path
      
      # get results files
      results <- list.files(resFol)
      
      # find stats folder and store path
      statsFol <- grep(pattern = c("stats"), x = results, value = T)
      
      # only csv files
      results <- grep(pattern = c(".csv"), x = results, value = T)
      
      # # # only the results files
      # results <- grep(pattern = c(keyword), x = results, value = T)
      
      # get param values and nb of rep
      strg = resFol
      
      # strg = sub(x = strg, pattern = "*.csv", replacement = "")   # cut ".csv" out
      strg = unlist(strsplit(strg, split = "-")) # split according to "-"
      strg = strg[-c(1:4)] # take out non param elements
      # strg = strg[-c(1, 2, 3, 4)] # take out non param elements
      
      varNames = c("pry1clQt", "prdclQt") # , "py2ctPr", "py2cons", "py2res")
      # baseValues = c(1, 100, 0.1, 10, 100)
      
      # initiate new line
      newLine <- NULL
      
      # get param values 
      for (k in 1:length(varNames)) {
        # newLine <- c(newLine, ifelse(str_detect(string = strg, pattern = varNames[k]), as.numeric(sub(x = strg, pattern = paste(varNames[k], "*", sep = ""), replacement = "")), baseValues[k]))
        newLine <- c(newLine, sub(x = strg, pattern = paste(varNames[k], "*", sep = ""), replacement = "")[k])
      }
      
      # find and open the merged results stats
      statsFol <- paste(folder, simFol[j], "stats", sep = "/")
      statsContent <- list.files(statsFol)
      mergedFile <- grep(pattern = c("merged"), x = statsContent, value = T)
      
      mergedFileName <- paste(statsFol, mergedFile, sep = "/")
      
      resTab <- as.data.frame(read.csv(mergedFileName, header = T, row.names = NULL, stringsAsFactors = T))
      
      # get nb of replicates
      replicateNb <- length(levels(as.factor(resTab$replicate)))
      newLine <- c(newLine, replicateNb)
      
      # initiate extinctions counters
      pry1ext = 0
      pry2ext = 0
      predExt = 0
      
      # loop over the results files
      for (m in 1:replicateNb) {
        
        # print(paste("reading file ", results[m]))
        # read results
        res <- subset(resTab, replicate == m)
        
        # count extinctions
        if (res[dim(res)[1], 5] <= 0) {pry1ext = pry1ext+1}
        if (res[dim(res)[1], 6] <= 0) {pry2ext = pry2ext+1}
        if (res[dim(res)[1], 9] <= 0) {predExt = predExt+1}
        
      } # end loop over files (replicates)
      
      # update new line
      newLine <- c(newLine, pry1ext/replicateNb, pry2ext/replicateNb, predExt/replicateNb)
      
      # browse stats folder and read stats
      # stats <- list.files(paste(resFol, statsFol, sep = "/"))
      statsFile <- grep(pattern = c("stats"), x = statsContent, value = T)
      statsFile <- grep(pattern = c(".csv"), x = statsFile, value = T)
      
      statsTab <- read.csv(paste(statsFol, statsFile, sep = "/"))
      
      # # subset before pred introduction
      # sub <- subset(statsTab, subset = statsTab$timeStep >= before[1] & statsTab$timeStep < before[2])
      # # get before measures
      # newLine <- c(newLine, 
      #              mean(sub$prey1PopulationSizeMean), max(sub$prey1PopulationSizeMean), min(sub$prey1PopulationSizeMean),
      #              mean(sub$prey2PopulationSizeMean), max(sub$prey2PopulationSizeMean), min(sub$prey2PopulationSizeMean),
      #              mean(sub$prey1growthRateMean/100), max(sub$prey1growthRateMean/100), min(sub$prey1growthRateMean/100),
      #              mean(sub$prey2growthRateMean/100), max(sub$prey2growthRateMean/100), min(sub$prey2growthRateMean/100))
      
      # subset after
      sub <- subset(statsTab, subset = statsTab$timeStep >= after[1] & statsTab$timeStep <= after[2])
      # get before measures
      newLine <- c(newLine, 
                   mean(sub$prey1PopulationSizeMean), max(sub$prey1PopulationSizeMean), min(sub$prey1PopulationSizeMean),
                   mean(sub$prey2PopulationSizeMean), max(sub$prey2PopulationSizeMean), min(sub$prey2PopulationSizeMean),
                   mean(sub$predator1PopulationSizeMean), max(sub$predator1PopulationSizeMean), min(sub$predator1PopulationSizeMean),
                   mean(sub$prey1growthRateMean), max(sub$prey1growthRateMean), min(sub$prey1growthRateMean),
                   mean(sub$prey2growthRateMean), max(sub$prey2growthRateMean), min(sub$prey2growthRateMean),
                   mean(sub$predatorGrowthRateMean), max(sub$predatorGrowthRateMean), min(sub$predatorGrowthRateMean),
                   mean(sub$prey1catchesMean), max(sub$prey1catchesMean), min(sub$prey1catchesMean),
                   mean(sub$prey2catchesMean), max(sub$prey2catchesMean), min(sub$prey2catchesMean),
                   mean(sub$prey1catchRateMean), max(sub$prey1catchRateMean), min(sub$prey1catchRateMean),
                   mean(sub$prey2catchRateMean), max(sub$prey2catchRateMean), min(sub$prey2catchRateMean))
      
      # rbind the line to tab
      tab <- rbind(tab, as.numeric(newLine))
      
    } # end loop over sim folders
    
    # name columns
    colnames(tab) <- headers
    
    # create local SA results folder in allStatsAndPlots and save table
    newFolderDir <- paste(folder, "allStatsAndPlots", "cullStratFiles", sep = "/")
    dir.create(path = newFolderDir)
    write.csv(tab, file = paste(newFolderDir, "/stats-", content[i], ".csv", sep = ""), row.names = FALSE)
    
  } # end loop over folders
  
} # end of function

cullResultsNoExt <- function(path, keyword = c("Results", "Snapshot"), pattern) {
  
  # get the directory content
  # content <- list.files(paste("~/", path, sep = ""))
  content <- list.files(path)
  
  # order alphabetically
  content <- content[order(content)]
  
  # only the folders
  content <- grep(pattern = c("folder"), x = content, value = T)
  
  # create table 
  headers <- c("prey1cullQuota", "predcullQuota", # "prey2catchProb", "prey2maxCons", "prey2resAva", 
               "replicatesNb",
               # "prey1densBeforeMean", "prey1densBeforeMax", "prey1densBeforeMin",
               # "prey2densBeforeMean", "prey2densBeforeMax", "prey2densBeforeMin",
               # "prey1growthBeforeMean", "prey1growthBeforeMax", "prey1growthBeforeMin",
               # "prey2growthBeforeMean", "prey2growthBeforeMax", "prey2growthBeforeMin",
               "prey1densAfterMean", "prey1densAfterMax", "prey1densAfterMin",
               "prey2densAfterMean", "prey2densAfterMax", "prey2densAfterMin",
               "predatorDensMean", "predatorDensMax", "predatorDensMin",
               "prey1growthAfterMean", "prey1growthAfterMax", "prey1growthAfterMin",
               "prey2growthAfterMean", "prey2growthAfterMax", "prey2growthAfterMin",
               "predatorGrowthMean", "predatorGrowthMax", "predatorGrowthMin",
               "prey1catchesMean", "prey1catchesMax", "prey1catchesMin",
               "prey2catchesMean", "prey2catchesMax", "prey2catchesMin",
               "prey1catchRateMean", "prey1catchRateMax", "prey1catchRateMin",
               "prey2catchRateMean", "prey2catchRateMax", "prey2catchRateMin")
  
  # set before after intervals
  tIntro = 301
  tEnd = 2000
  before <- c(tIntro-100, tIntro)
  after <- c(tEnd-500, tEnd)
  
  # loop over the folders
  for (i in 1:length(content)) {
    
    # path to folder
    # folder = paste("~/", path, content[i], sep = "")
    folder = paste(path, content[i], sep = "")
    print(paste("in localSA folder ", i, ": ", folder))# get the value of XparamName
    
    tab <- data.frame(matrix(ncol = length(headers), nrow = 0))
    
    # get content
    globalFol = list.files(folder)
    
    # select only folders
    globalFol <- grep(pattern = c("allStats"), x = globalFol, value = T)
    
    # get results folder
    resFol <- paste(folder, globalFol, sep = "/")
    # print(paste("in", resFol))
    # 
    # # find stats folder and store path
    # 
    # get results files
    statsFol <- list.files(resFol)
    
    # find stats folder and store path
    statsFol <- grep(pattern = c("ResultsFiles-woExt"), x = statsFol, value = T)
    print(paste("in", statsFol))
    
    results <- list.files(paste(resFol, statsFol, sep = "/"))
    
    # only csv files
    results <- grep(pattern = c(".csv"), x = results, value = T)
    
    # # only the results files
    # results <- grep(pattern = c(keyword), x = results, value = T)
    
    for (j in 1:length(results)) {
      
      # initiate new line
      newLine <- NULL
      
      print(paste("in results file", results[j]))
      
      # get param values and nb of rep
      # get param values and nb of rep
      strg = results[j]
      
      strg = sub(x = strg, pattern = "*.csv", replacement = "")   # cut ".csv" out
      strg = unlist(strsplit(strg, split = "-")) # split according to "-"
      strg = strg[-c(1:4)] # take out non param elements
      # strg = strg[-c(1, 2, 3, 4)] # take out non param elements
      
      varNames = c("pry1clQt", "prdclQt") # , "py2ctPr", "py2cons", "py2res")
      # baseValues = c(1, 100, 0.1, 10, 100)
      
      # get param values 
      for (k in 1:length(varNames)) {
        # newLine <- c(newLine, ifelse(str_detect(string = strg, pattern = varNames[k]), as.numeric(sub(x = strg, pattern = paste(varNames[k], "*", sep = ""), replacement = "")), baseValues[k]))
        newLine <- c(newLine, sub(x = strg, pattern = paste(varNames[k], "*", sep = ""), replacement = "")[k])
      }
      # # initiate extinctions counters
      # pry1ext = 0
      # pry2ext = 0
      # predExt = 0
      # 
      # # loop over the results files
      # for (m in 1:length(results)) {
      #   
      #   # print(paste("reading file ", results[m]))
      #   # read results
      #   res <- read.csv(paste(resFol, results[m], sep = "/"))
      #   
      #   # count extinctions
      #   if (res[dim(res)[1], 4] == 0) {pry1ext = pry1ext+1}
      #   if (res[dim(res)[1], 5] == 0) {pry2ext = pry2ext+1}
      #   if (res[dim(res)[1], 8] == 0) {predExt = predExt+1}
      #   
      # } # end loop over files (replicates)
      # 
      # # update new line
      # newLine <- c(newLine, pry1ext/length(results), pry2ext/length(results), predExt/length(results))
      
      # # browse stats folder and read stats
      # stats <- list.files(paste(resFol, statsFol, sep = "/"))
      # stats <- grep(pattern = c("stats"), x = stats, value = T)
      # stats <- grep(pattern = c(".csv"), x = stats, value = T)
      
      res <- read.csv(paste(resFol, statsFol, results[j], sep = "/"))
      
      # get nb of replicates
      replicateNb = res$repNb[2]
      newLine <- c(newLine, replicateNb)
      
      # # subset before pred introduction
      # sub <- subset(res, subset = res$timeStep >= before[1] & res$timeStep < before[2])
      # # get before measures
      # newLine <- c(newLine, 
      #              mean(sub$prey1PopulationSizeMean), max(sub$prey1PopulationSizeMean), min(sub$prey1PopulationSizeMean),
      #              mean(sub$prey2PopulationSizeMean), max(sub$prey2PopulationSizeMean), min(sub$prey2PopulationSizeMean),
      #              mean(sub$prey1growthRateMean/100), max(sub$prey1growthRateMean/100), min(sub$prey1growthRateMean/100),
      #              mean(sub$prey2growthRateMean/100), max(sub$prey2growthRateMean/100), min(sub$prey2growthRateMean/100))
      
      # subset after
      sub <- subset(res, subset = res$timeStep >= after[1] & res$timeStep <= after[2])
      # get before measures
      newLine <- c(newLine, 
                   mean(sub$prey1PopulationSizeMean), max(sub$prey1PopulationSizeMean), min(sub$prey1PopulationSizeMean),
                   mean(sub$prey2PopulationSizeMean), max(sub$prey2PopulationSizeMean), min(sub$prey2PopulationSizeMean),
                   mean(sub$predator1PopulationSizeMean), max(sub$predator1PopulationSizeMean), min(sub$predator1PopulationSizeMean),
                   mean(sub$prey1growthRateMean), max(sub$prey1growthRateMean), min(sub$prey1growthRateMean),
                   mean(sub$prey2growthRateMean), max(sub$prey2growthRateMean), min(sub$prey2growthRateMean),
                   mean(sub$predatorGrowthRateMean), max(sub$predatorGrowthRateMean), min(sub$predatorGrowthRateMean),
                   mean(sub$prey1catchesMean), max(sub$prey1catchesMean), min(sub$prey1catchesMean),
                   mean(sub$prey2catchesMean), max(sub$prey2catchesMean), min(sub$prey2catchesMean),
                   mean(sub$prey1catchRateMean), max(sub$prey1catchRateMean), min(sub$prey1catchRateMean),
                   mean(sub$prey2catchRateMean), max(sub$prey2catchRateMean), min(sub$prey2catchRateMean))
      
      # rbind the line to tab
      tab <- rbind(tab, as.numeric(newLine))
      
    } # end loop over results folders
    
    # name columns
    colnames(tab) <- headers
    
    # create local SA results folder in allStatsAndPlots and save table
    newFolderDir <- paste(folder, "allStatsAndPlots", "cullStratFiles-woExt", sep = "/")
    dir.create(path = newFolderDir)
    write.csv(tab, file = paste(newFolderDir, "/woExt-stats-", content[i], ".csv", sep = ""), row.names = FALSE)
    
  } # end loop over folders
  
} # end of function

#### Extinction frequency contour figures ####

library(plotly)

# build results matrix

tabFol <- paste(folder, "allStatsAndPlots", "/cullStratFiles", sep = "")

d <- read.csv(paste(tabFol, "stats-folder-cullBatch1.csv", sep = "/"))

# using plotly
{xVar <- levels(as.factor(d$prey1cullQuota))
yVar <- levels(as.factor(d$predcullQuota))

resmat <- matrix(data = d$prey2extFreq, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  x = xVar, 
  y = yVar, 
  # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
  z = t(resmat),
  type = "heatmap",
  colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
  # autocontour = F,
  # contours = list(showlabels = TRUE),
  # contours = list(
  #   start = 0,
  #   end = 1,
  #   size = 0.1,
  #   showlabels = T
  # )
)

xlab <- list(
  title = "prey I removal quota"#,
  # titlefont = f
)
ylab <- list(
  title = "predator removal quota"#,
  # titlefont = f
)

fig <- fig %>% colorbar(title = "Prey II\nextinction\nfrequency")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)

fig# prey 1 extinction

resmat <- matrix(data = d$prey1extFreq, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  x = xVar, 
  y = yVar, 
  # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
  z = t(resmat),
  type = "contour",
  colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
  autocontour = F,
  contours = list(showlabels = TRUE),
  contours = list(
    start = 0,
    end = 1,
    size = 0.1,
    showlabels = T
  )
)

xlab <- list(
  title = "prey I removal quota"#,
  # titlefont = f
)
ylab <- list(
  title = "predator removal quota"#,
  # titlefont = f
)

fig <- fig %>% colorbar(title = "Prey I\nextinction\nfrequency")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)

fig

# predator extinction
resmat <- matrix(data = d$pred1extFreq, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  x = xVar, 
  y = yVar, 
  # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
  z = t(resmat),
  type = "contour",
  colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
  autocontour = F,
  contours = list(showlabels = TRUE),
  contours = list(
    start = 0,
    end = 1,
    size = 0.1,
    showlabels = T
  )
)

xlab <- list(
  title = "prey I removal quota"#,
  # titlefont = f
)
ylab <- list(
  title = "predator removal quota"#,
  # titlefont = f
)

fig <- fig %>% colorbar(title = "Predator\nextinction\nfrequency")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)

fig}

# using ggplot

savePath = "C:/Users/adb3/OneDrive - University of Stirling/Chapter 3/figures/"

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
        geom_tile(aes(fill = prey2extFreq))+
        geom_text(aes(label = prey2extFreq), col = "cyan", size = 2.5) + 
        scale_fill_viridis_c("Prey 2\nextinction\nfrequency", option = 'plasma') + # , direction = -1
        labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey2extFreqHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey1extFreq))+
  geom_text(aes(label = prey1extFreq), col = "cyan", size = 2.5) + 
  scale_fill_viridis_c("Prey 1\nextinction\nfrequency", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1extFreqHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = pred1extFreq))+
  geom_text(aes(label = pred1extFreq), col = "cyan", size = 2.5) + 
  scale_fill_viridis_c("Predator\nextinction\nfrequency", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "pred1extFreqHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

#### 3D plot for final density #### 

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey2densAfterMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 2\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey2finalDensHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey1densAfterMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 1\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1finalDensHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = predatorDensMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Predator\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1finalDensHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

{xVar <- levels(as.factor(d$prey1cullQuota))
yVar <- levels(as.factor(d$predcullQuota))

resmat <- matrix(data = d$prey2densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  x = xVar, 
  y = yVar, 
  # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
  z = t(resmat),
  type = "contour",
  # colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
  autocontour = F,
  contours = list(showlabels = TRUE),
  contours = list(
    start = 0,
    end = 1,
    size = 0.1,
    showlabels = T
  )
)

xlab <- list(
  title = "prey I removal quota"#,
  # titlefont = f
)
ylab <- list(
  title = "predator removal quota"#,
  # titlefont = f
)

fig <- fig %>% colorbar(title = "Prey II\nfinal\ndensity")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)

fig# prey 1 extinction
}

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey1densAfterMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 1\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1finalDensHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = predatorDensMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Predator\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "pred1finalDensHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

# test 3D
xVar <- levels(as.factor(d$prey1cullQuota))
yVar <- levels(as.factor(d$predcullQuota))

resmat <- matrix(data = d$prey2densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
    xaxis = list(title = "Prey 1 removal"),
    yaxis = list(title = "Predator removal"),
    zaxis = list(title = "Prey 2 density"))) # ,
    # camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
    # aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey II\nfinal\ndensity")
fig

resmat <- matrix(data = d$prey1densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5)),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 1 density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey I\ndensity")
fig

resmat <- matrix(data = d$predatorDensMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Predator density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Predator\ndensity")
fig

#### oscillation amplitude ####

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey2densAfterMax - prey2densAfterMin))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 2\ndensity\namplitude", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey2densAmplHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey1densAfterMax - prey1densAfterMin))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 1\ndensity\namplitude", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1densAmplHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = predatorDensMax - predatorDensMin))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Predator\ndensity\namplitude", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "pred1densAmplHeatmap.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

{xVar <- levels(as.factor(d$prey1cullQuota))
  yVar <- levels(as.factor(d$predcullQuota))
  
  resmat <- matrix(data = d$prey2densAfterMean, ncol = length(yVar), nrow = length(xVar))
  
  fig <- plot_ly(
    x = xVar, 
    y = yVar, 
    # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
    z = t(resmat),
    type = "contour",
    # colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
    autocontour = F,
    contours = list(showlabels = TRUE),
    contours = list(
      start = 0,
      end = 1,
      size = 0.1,
      showlabels = T
    )
  )
  
  xlab <- list(
    title = "prey I removal quota"#,
    # titlefont = f
  )
  ylab <- list(
    title = "predator removal quota"#,
    # titlefont = f
  )
  
  fig <- fig %>% colorbar(title = "Prey II\nfinal\ndensity")
  fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
  
  fig# prey 1 extinction
}

# test 3D
xVar <- levels(as.factor(d$prey1cullQuota))
yVar <- levels(as.factor(d$predcullQuota))

resmat <- matrix(data = d$prey2densAfterMax - d$prey2densAfterMin, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 2 amplitude"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey II\ndensity\namplitude")
fig

resmat <- matrix(data = d$prey1densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5)),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 1 density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey I\ndensity")
fig

resmat <- matrix(data = d$predatorDensMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Predator density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Predator\ndensity")
fig

#### without extinction ####
# build results matrix

tabFol <- paste(folder, "allStatsAndPlots", "cullStratFiles-woExt", sep = "/")

d <- read.csv(paste(tabFol, "woExt-stats-folder-cullBatch1.csv", sep = "/"))

savePath = "C:/Users/adb3/OneDrive - University of Stirling/Chapter 3/figures/woExt"

#### final density #### 

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey2densAfterMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 2\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey2finalDensHeatmap-woExt.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey1densAfterMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 1\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1finalDensHeatmap-woExt.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = predatorDensMean))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Predator\nfinal\ndensity", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "pred1finalDensHeatmap-woExt.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)


# test 3D
xVar <- levels(as.factor(d$prey1cullQuota))
yVar <- levels(as.factor(d$predcullQuota))

bf <- d[1:5,]
af <- d[6:dim(d)[1],]
newD <- rbind(bf, c(0, 0.25, rep(NA, dim(d)[2]-2)), af)

resmat <- matrix(data = newD$prey2densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 2 density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey II\nfinal\ndensity")
fig

resmat <- matrix(data = newD$prey1densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5)),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 1 density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey I\ndensity")
fig

resmat <- matrix(data = newD$predatorDensMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Predator density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Predator\ndensity")
fig

#### oscillation amplitude ####

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey2densAfterMax - prey2densAfterMin))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 2\ndensity\namplitude", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey2densAmplHeatmap-woExt.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = prey1densAfterMax - prey1densAfterMin))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Prey 1\ndensity\namplitude", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "prey1densAmplHeatmap-woExt.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

ggp <- ggplot(d, aes(prey1cullQuota, predcullQuota)) +
  geom_tile(aes(fill = predatorDensMax - predatorDensMin))+
  # geom_text(aes(label = round(prey2densAfterMean, 2)), col = "white", size = 2.5) +
  scale_fill_viridis_c("Predator\ndensity\namplitude", option = 'plasma') + # , direction = -1
  labs(x = "Prey 1 removal quota", y = "Predator removal quota") 
ggp                  

ggsave(filename = "pred1densAmplHeatmap-woExt.png", plot = ggp, device = "png", scale = 4, path = savePath, limitsize = F)

{xVar <- levels(as.factor(d$prey1cullQuota))
  yVar <- levels(as.factor(d$predcullQuota))
  
  resmat <- matrix(data = d$prey2densAfterMean, ncol = length(yVar), nrow = length(xVar))
  
  fig <- plot_ly(
    x = xVar, 
    y = yVar, 
    # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
    z = t(resmat),
    type = "contour",
    # colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
    autocontour = F,
    contours = list(showlabels = TRUE),
    contours = list(
      start = 0,
      end = 1,
      size = 0.1,
      showlabels = T
    )
  )
  
  xlab <- list(
    title = "prey I removal quota"#,
    # titlefont = f
  )
  ylab <- list(
    title = "predator removal quota"#,
    # titlefont = f
  )
  
  fig <- fig %>% colorbar(title = "Prey II\nfinal\ndensity")
  fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
  
  fig# prey 1 extinction
}

# test 3D
xVar <- levels(as.factor(d$prey1cullQuota))
yVar <- levels(as.factor(d$predcullQuota))

resmat <- matrix(data = d$prey2densAfterMax - d$prey2densAfterMin, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 2 amplitude"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey II\ndensity\namplitude")
fig

resmat <- matrix(data = d$prey1densAfterMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white', width=0.5)),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Prey 1 density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Prey I\ndensity")
fig

resmat <- matrix(data = d$predatorDensMean, ncol = length(yVar), nrow = length(xVar))

fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white'),
    y = list(show = TRUE, start = 0, end = 0.25, size = 0.05, color = 'white')),
  x = xVar,
  y = yVar,
  z = resmat)
fig <- fig %>% layout( scene = list(
  xaxis = list(title = "Prey 1 removal"),
  yaxis = list(title = "Predator removal"),
  zaxis = list(title = "Predator density"))) # ,
# camera = list(eye = list(x = 0, y = -1, z = 0.5)) # ,
# aspectratio = list(x = .9, y = .8, z = 0.2)
fig <- fig %>% colorbar(title = "Predator\ndensity")
fig