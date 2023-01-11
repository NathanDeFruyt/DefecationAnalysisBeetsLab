## functions for Signes script
cdata <- data.frame()
read.stinky.folder <- function(folder){
  files <- list.files(path = folder, pattern = '.tsv', full.names = T)
  code <- read.csv(list.files(path = folder, pattern = 'code.csv', full.names = T), header = T); names(code) <- c('strain', 'code')
  
  pointer = 1
  for (file in files){
    headerline = grep('Time\tMedia file path\t', readLines(file))[1] -1
    subdata <- read.table(file = file, sep = '\t', dec = '.', header = T, skip = headerline)[c('Subject', 'Behavior', 'Behavioral.category', 'Time')]
    subdata$strain.code <- strsplit(basename(file), '.', fixed = T)[[1]][1]
    subdata$date <- basename(folder)
    names(subdata) <- c('Replicate', 'Behavior', 'Behavioral.category', 'Time', 'strain.code', 'date')
    subdata$file <- basename(file)
    if (file == files[1]) { data <- subdata } else { data <- rbind(data, subdata) }
  }
  
  data$strain <- as.factor(sapply(data$strain, function(x) return(code$strain[which(code$code == x)])))
  
  ## calculate all times between two succeeding events (per file: each file start from 0 and goes up to end)
  data$time = 0
  data$wormID = interaction(data$strain, data$Replicate)
  for (i in unique(data$wormID)){data$time[which(data$wormID == i)] <- c(0, diff(data$Time[which(data$wormID == i)])) }
  
  ## compute all cycle lengths
  data$cycle = 0
  for (worm in unique(data$wormID)) {
    pointer = 0
    for (i in c(1:nrow(data[which(data$wormID == worm),]))){
      data$cycle[which(data$wormID == worm)][i] <- pointer
      if (data$Behavior[which(data$wormID == worm)][i] == 'pBoc') {pointer = pointer + 1}
    }
  }
  
  # and create a new data.frame with cycle lengths
  pBocs <- data[which(data$Behavior == 'pBoc'),]
  pBocs$cycle.length = 0
  for (i in unique(pBocs$wormID)){pBocs$cycle.length[which(pBocs$wormID == i)] <- c(0, diff(pBocs$Time[which(pBocs$wormID == i)])) }
  
  # now exclude all first values 
  data <- data[-which(data$cycle == 0),]
  pBocs <- pBocs[-which(pBocs$cycle == 0),]
  
  # and select relevant columns only
  return(list(data = data, pBocs = pBocs))
}

asterisks <- function(p_value) {
  td = data.frame(value = c(0, 0.005, 0.01, 0.05, 0.1, 1),
                  symbol = c('****', '***', '**', '*', '.', 'n.s.'))
  if (is.na(p_value)) { return('NA') } else { return(td$symbol[which(td$value == min(td$value[which(td$value >= p_value)]))]) }
}  

ui <- fluidPage(h1("Signes defecation data formatter and analyzer"),
                sidebarLayout(
                  sidebarPanel(
                    h4("Welcome to this home-developed applet! We are happy that you have been watching worms do their private thing all day long! Let's take your interesting observations to next level by visualising them."),
                    p('Since you already loaded your data, the only thing you need to put in now is the strain you see as control. In most cases, this will be N2.'),
                  ),    
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Data", 
                               h3("Data Download"), 
                               p('In case you prefer doing your data analysis and visualisation in other software, use the download button below.'),
                               p(''),
                               p('Below you find three tables. Each table represents the first six lines of a dataset, which you can download by pressing the button below.'),
                               p(''),
                               tableOutput('head.pBocs.data'),
                               downloadButton('pBocs.data', 'Download cycle length data for each cycle of each worm'),
                               p(''),
                               tableOutput('head.pBocs.summary'),
                               downloadButton('pBocs.summary', 'Download summary of cycle length data'),
                               p(''),
                               tableOutput('head.expulsion.data'),
                               downloadButton('expulsion.data', 'Download expulsion data')),
                      # downloadButton('expulsion.data', label = 'Download expulsion data')),
                      tabPanel("Visualisation",
                               selectInput("reference.strain", label = 'select negative control', choices = unique(cdata$strain)),
                               
                               h3('Cycle length'),
                               # tableOutput('data'),
                               plotOutput('cycle.length.boxes'),
                               h3('Expulsion rate'),
                               plotOutput('expulsion.rates'),
                               tableOutput('test')
                      ), # tabpanel 'tinygraphs
                      tabPanel("Statistics",
                               tabsetPanel(
                                 tabPanel("ANOVA",
                                          h3('ANOVA summary cycle length'),
                                          tableOutput('anova.summary.cl'),
                                          p('The table above reports on the significances as measured using a one-way ANOVA.'),
                                          h3('ANOVA summary expulsion rate (Exp/cycle)'),
                                          tableOutput('anova.summary.exp'),
                                          p('The table above reports on the significances as measured using a generalised linear model taking a binomial error distribution into account.')
                                 ), # tabPanel ANOVA
                                 tabPanel("PostHoc",
                                          h3('PostHoc comparisons cycle length'),
                                          selectInput("cycle.length.test", label = 'p-value correction', choices = c('tukey', 'bonferroni', 'dunnett')),
                                          tableOutput('posthoc.cycle.length'),
                                          h3('PostHoc comparisons expulsion rate (Exp/cycle)'),
                                          selectInput("expulsion.test", label = 'p-value correction', choices = c('tukey', 'bonferroni', 'dunnett')),
                                          tableOutput('posthoc.expulsion')
                                 ) # tabPanel PostHoc
                               )
                      )
                    ) # tabsetpanel
                  ) # mainpanel
                ))

server <- function(input, output){
  output$data <- renderTable(cdata)
  
  data <- cdata
  pBocs <- cpBocs
  
  ## now visualise all pBocs: 
  clb <- ggplot(NULL)+
    geom_boxplot(aes(x = strain, y = cycle.length), size = 1, fill = 'white', alpha = .7, colour = 'black', data = pBocs)+
    geom_point(aes(x = strain, y = cycle.length), colour = 'black', fill = 'black', data = pBocs)+
    theme_classic()+
    theme(axis.line = element_line(size = 1, colour = 'black'),
          axis.ticks = element_line(size =1 , colour = 'black'),
          axis.text = element_text(size = 12, face = 'bold', colour = 'black'),
          axis.title = element_text(size = 14, face = 'bold', colour = 'black'))
  
  output$cycle.length.boxes <- renderPlot(clb)
  
  sem <- function(x) sd(x)/sqrt(length(x))
  bardata <- doBy::summaryBy(cycle.length ~ strain, data = pBocs, FUN = c(mean, sd, sem))
  names(bardata) <- c('strain', 'mean', 'sd', 'sem')
  pointdata <- doBy::summaryBy(cycle.length ~ Replicate + strain, data = pBocs, FUN = c(mean, sd, sem))
  names(pointdata) <- c('worm', 'strain', 'mean', 'sd', 'sem')
  
  ggplot(NULL)+
    geom_bar(aes(x = strain, y = mean), stat = 'identity', data = bardata, fill = 'white', colour = 'black')+
    geom_errorbar(aes(x = strain, ymin = mean - sem, ymax = mean + sem), width = 0.3, data = bardata)+
    geom_point(aes(x = strain, y = mean), data = pointdata)+
    scale_y_continuous(expand = c(0, 0), )+
    ylab(label = 'mean cycle length (s)')+
    theme_classic()+
    theme(axis.text = element_text(colour = 'black'),
          axis.line = element_line(colour = 'black'))
  
  ## let's do some statistics
  ## a simple linear model taking individual worms as random factors
  cycle.fit <- lmer(cycle.length ~ strain + (1|wormID), data = pBocs)
  
  cycle.fit.aov <- data.frame(Anova(cycle.fit)) %>% cbind(data.frame(factor = rownames(.)), ., sapply(.[3], asterisks))
  names(cycle.fit.aov) <- c('', 'Chisq', 'df', 'p-value', '')
  output$anova.summary.cl <- renderTable(cycle.fit.aov)
  
  ## and the posthoc comparisons for this
  padjust <- reactive(ifelse(input$cycle.length.test == 'dunnett', 'dunnettx', input$cycle.length.test))
  cl.ph.1 <- reactive(data.frame(summary(contrast(lsmeans(cycle.fit, ~strain),method="trt.vs.ctrl",adjust=as.character(padjust())))))
  cl.ph <- reactive({
    cl.ph <- cl.ph.1()
    cl.ph$sign <- sapply(cl.ph$p.value, asterisks)
    cl.ph <- cl.ph
  })
  
  ## and make a download button for the dataset
  output$posthoc.cycle.length <- renderTable(cl.ph())
  pBocs.output <- pBocs[c('Replicate', 'strain.code', 'strain', 'cycle.length', 'cycle', 'date')]
  
  output$head.pBocs.data <- renderTable(head(pBocs.output))
  
  output$pBocs.data <- downloadHandler(
    filename = function() {
      paste(pBocs.output$date, '-cycle_lengths.csv', sep='')
    },
    content = function(con) {
      write.csv(pBocs.output, con, sep = ';', dec = ',',  row.names = F)
    }
  )  
  
  pBocs.summary <- doBy::summaryBy(cycle.length~Replicate+strain.code+strain+date, data = pBocs.output, FUN = c(mean, median, sd, sem))
  names(pBocs.summary) <- c('Replicate', 'strain.code', 'strain', 'date', 'mean.cycle.length', 'median.cycle.length', 'sd', 'sem')
  
  output$head.pBocs.summary <- renderTable(head(pBocs.summary))
  
  output$pBocs.summary <- downloadHandler(
    filename = function() {
      paste(pBocs.summary$date, '-summary_cycle_lengths.csv', sep='')
    },
    content = function(con) {
      write.csv(pBocs.summary, con, sep = ';', dec = ',', row.names = F)
    }
  )  
  
  ## What about the number of expulsions per cycle? 
  sumdata <- doBy::summaryBy(wormID ~ strain + Replicate + date, data = data[which(data$Behavior == 'pBoc'),], FUN = length)
  names(sumdata) <- c('strain', 'worm', 'date', 'nr.pBoc')
  sumdata$dummy <- interaction(sumdata$strain, sumdata$worm)
  sumdata$nr.expulsion <- sapply(sumdata$dummy, function(x) {return(nrow(data[which(data$wormID == x & data$Behavior == 'Exp'),]))})
  sumdata$success = sumdata$nr.expulsion/sumdata$nr.pBoc
  
  ## visualise the success rate
  expulsion.rates <- ggplot(NULL)+
    geom_boxplot(aes(x = strain, y = success), size = 1, colour = 'black', fill = 'white', data = sumdata)+
    geom_point(aes(x = strain, y = success), colour = 'black', fill = 'black', data = sumdata)+
    theme_classic()+
    theme(axis.line = element_line(size = 1, colour = 'black'),
          axis.ticks = element_line(size =1 , colour = 'black'),
          axis.text = element_text(size = 12, face = 'bold', colour = 'black'),
          axis.title = element_text(size = 14, face = 'bold', colour = 'black'))
  
  success_bardata <- doBy::summaryBy(success ~ strain, data = sumdata, FUN = c(mean, sd, sem, length))
  names(success_bardata) <- c('strain', 'mean', 'sd', 'sem', 'n')
  ggplot(NULL)+
    geom_bar(aes(x = strain, y = mean), fill = 'white', colour = 'black', data = success_bardata, stat = 'identity')+
    geom_errorbar(aes(x = strain, ymin = mean - sem, ymax = mean+sem), data = success_bardata, width = 0.3)+
    geom_point(aes(x = strain, y = success),data = sumdata)+
    ylab(label = 'proportion expulsions/cycle')+
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(c(success_bardata$mean+success_bardata$sd, sumdata$success))+0.10))+
    theme_classic()+
    theme(axis.text = element_text(colour = 'black'),
          axis.line = element_line(colour = 'black'))+
    geom_text(aes(x = strain, y = 0.05, label = paste0('n=',n)), data  = success_bardata)
  
  output$expulsion.rates <- renderPlot(expulsion.rates)
  
  ## statistics on success rate: 
  sumdata$nr.fails <- sumdata$nr.pBoc - sumdata$nr.expulsion
  exp.fit <- glm(cbind(sumdata$nr.pBoc, sumdata$nr.fails) ~strain, family = 'binomial', data = sumdata)
  exp.fit.aov <- data.frame(Anova(exp.fit)) %>% cbind(data.frame(factor = rownames(.)), .)
  exp.fit.aov$sign <- sapply(exp.fit.aov$Pr..Chisq., asterisks)
  names(exp.fit.aov) <- c('', 'LR Chisq', 'df', 'p-value', '')
  
  ## render this as an output table
  output$anova.summary.exp <- renderTable(exp.fit.aov)
  
  ## next posthoc comparisons
  padjust.exp <- reactive(ifelse(input$expulsion.test == 'dunnett', 'dunnettx', input$expulsion.test))
  exp.ph.1 <- reactive(data.frame(summary(contrast(lsmeans(exp.fit, ~strain),method="trt.vs.ctrl",adjust=as.character(padjust.exp())))))
  exp.ph <- reactive({
    exp.ph <- exp.ph.1()
    exp.ph$sign <- sapply(exp.ph$p.value, asterisks)
    exp.ph <- exp.ph
  })
  
  output$posthoc.expulsion <- renderTable(exp.ph())
  
  sbc <- ggplot(NULL)+
    geom_bar(aes(x = strain, y = mean), fill = 'white', colour = 'black', data = success_bardata, stat = 'identity')+
    geom_errorbar(aes(x = strain, ymin = mean - sem, ymax = mean+sem), data = success_bardata, width = 0.3)+
    geom_point(aes(x = strain, y = success),data = sumdata)+
    ylab(label = 'proportion expulsions/cycle')+
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(c(success_bardata$mean+success_bardata$sd, sumdata$success))+0.10))+
    theme_classic()+
    theme(axis.text = element_text(colour = 'black'),
          axis.line = element_line(colour = 'black'))+
    geom_text(aes(x = strain, y = 0.05, label = paste0('n=',n)), data  = success_bardata)+
    geom_text(aes(x = strain, y = mean+sem+0.05, label = sign), data = success_bardata); sbc

    names(sumdata) <- c('strain', 'worm', 'date', 'nr.pBoc', 'nr.expulsion', 'success.rate', 'nr.fails')
  output$head.expulsion.data <- renderTable(head(sumdata))
  
  output$expulsion.data <- downloadHandler(
    filename = function() {
      paste(sumdata$date, '-expulsion_data.csv', sep='')
    },
    content = function(con) {
      write.csv(sumdata, con, sep = ';', dec = ',', row.names = F)
    }
  )
  
}

