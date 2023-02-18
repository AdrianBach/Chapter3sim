Path = "C:/Users/adb3/Desktop/PhD/GitKraken/Chapter3sim/"
folder = "C:/Users/adb3/Desktop/PhD/GitKraken/Chapter3sim/folder-preyAlone-newPredMaxCons/"
Keyword = "Results"
Pattern = "introiiPrey-p"
freqTrials = 10

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

statsResults <- function(path, keyword = c("Results", "Snapshot"), pattern, freqTrials) {
  
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
    dir.create(path = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
    
    # get content
    simFol = list.files(folder)
    
    # select only folders
    simFol <- grep(pattern = c(pattern), x = simFol, value = T)
    
    # loop over the sim folders
    for (j in 2:length(simFol)) {
      
      print(paste("in sim folder ", simFol[j]))
      
      # # rename the file
      # cux <- list.files(paste(path, content[i], simFol[j], sep = "/"))
      # cux <- grep(pattern = c("stats"), x = cux, value = T)
      # file.rename(paste(path, content[i], simFol[j], cux, sep = "/"), paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = ""))
      
      # path to folder
      # statsFol = paste(path, content[i], "/", simFol[j], "/stats-", simFol[j], sep = "")
      # statsFol = paste(folder, simFol[j], "/stats", sep = "/")
      statsFol = paste(folder, simFol[j], "/stats", sep = "")
      # statsFol = paste(path, simFol[j], "/stats-", simFol[j], sep = "")
      
      # file.rename(paste(statsFol, results, sep = "/"), paste(statsFol, "/merged", keyword, "-", simFol[j], ".csv", sep = ""))
      
      # get files
      results <- list.files(statsFol)
      
      # only csv files
      results <- grep(pattern = c("merged"), x = results, value = T)
      
      # only the results files
      results <- grep(pattern = c(keyword), x = results, value = T)
      
      # get name
      name <- paste(statsFol, "/", results, sep = "")
      
      # read table
      stats <- read.csv(name)
      
      # if(length(results[m])==0) {next}
      
      ## create table
      
      # new headers
      headers <- colnames(stats)
      newHeaders <- c(headers[2], "repNb")
      for (k in 3:length(headers)) {
        newHeaders <- c(newHeaders, paste(headers[k], "Mean", sep = ""), paste(headers[k], "ICinf", sep = ""), paste(headers[k], "ICsup", sep = ""))
      }
      
      # create an empty table 
      tab <- data.frame(matrix(ncol = length(newHeaders), nrow = 0)) 
      
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
      
      # write in the corresponding folder
      write.csv(tab, file = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), row.names = FALSE)
      
      # and copy in the global stats folder
      file.copy(from = paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
      
      # plot and save density figure
      data <- tab
      
      x = data$timeStep
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
        labs(x = "Time steps", y = "Population density") +
        scale_colour_manual(name='Populations',
                            breaks=c('Prey 1', 'Prey 2', 'Predator'),
                            values=c(y1c, y2c, y3c))
      
      # save plot in this folder
      ggsave(filename = paste("stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)
      
      # and copy in the global stats folder
      file.copy(from = paste(statsFol, "/stats", keyword, "-density-", simFol[j], ".pdf", sep = ""), to = paste(folder, "allStatsAndPlots/", keyword, "Files", sep = ""))
      
      # plot and save catch rate figure
      # data <- read.csv(paste(statsFol, "/stats", keyword, "-", simFol[j], ".csv", sep = ""))

      x = data$timeStep
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
        labs(x = "Time steps", y = "Average catch rate between trials") +
        scale_colour_manual(name='Populations',
                            breaks=c('Prey 1', 'Prey 2', 'Predator'),
                            values=c(y1c, y2c, y3c))

      # save plot in this folder
      ggsave(filename = paste("stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), path = statsFol, plot = fig, width = 6.22, height = 5.73, limitsize = TRUE)

      # and copy in the global stats folder
      file.copy(from = paste(statsFol, "/stats", keyword, "-catchRate-", simFol[j], ".pdf", sep = ""), to = paste(folder, "/allStatsAndPlots/", keyword, "Files", sep = ""))
      
    } # end loop over sim folders
    
  } # end loop over local SA folders
  
}

