#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(ggplot2)


scratch <- read.csv("F:/My work/R project/6070/Group Three_350-500 Views.csv")


corr1 <- ggplot(scratch, aes(x = viewers_website, y = downloaders_website)) +
  
  geom_point(aes(colour = downloaders_website), show.legend = F) +

  geom_smooth(method = "lm", se = F, colour = "orange") +
  
  xlim(c(350, 500)) +
  
  # ylim(c(0, 250)) +

  labs(y="Project Downloaded",

       x="Project Viewed",

       title="Project Downloaded vs Project Viewed"

      )







attach(scratch)

scr1 <- tapply(downloaders_website, viewers_website, mean)

scr1 <- as.data.frame(scr1)

scr1 <- cbind(viewers_website = rownames(scr1), scr1)
rownames(scr1) <- 1:nrow(scr1)

scr1[, 1] <- as.numeric(scr1[, 1])




corr2 <- ggplot(scr1, aes(x = viewers_website, y = scr1)) +
  
  geom_point(aes(colour = scr1), show.legend = F) +
  
  geom_smooth(method = "lm", se = F, colour = "orange") +
  
  # xlim(c(350, 500)) +
  # 
  # ylim(c(0, 250)) +

  scale_x_continuous(labels = NULL) +
  
  labs(y="Project Downloaded",
       
       x="Project Viewed",
       
       title="Project Downloaded Avg. vs Project Viewed"
       
  )






scratch_w <- scratch[-which(is.na(scratch$lovers_website)),]


corr3 <- ggplot(scratch_w, aes(x = viewers_website, y = lovers_website)) +
  
  geom_point(aes(colour = lovers_website), show.legend = F) +
  
  geom_smooth(method = "lm", se = F, colour = "orange") +
  
  xlim(c(350, 500)) +
  
  # ylim(c(0, 250)) +
  
  labs(y="Project Loved",
       
       x="Project Viewed",
       
       title="Project Loved vs Project Viewed"
       
  )






attach(scratch_w)

scr2 <- tapply(lovers_website, viewers_website, mean)

scr2 <- as.data.frame(scr2)

scr2 <- cbind(viewers_website = rownames(scr2), scr2)
rownames(scr2) <- 1:nrow(scr2)

scr2[, 1] <- as.numeric(scr2[, 1])





corr4 <- ggplot(scr2, aes(x = viewers_website, y = scr2)) +
  
  geom_point(aes(colour = scr2), show.legend = F) +
  
  geom_smooth(method = "lm", se = F, colour = "orange") +
  
  # xlim(c(350, 500)) +
  # 
  # ylim(c(0, 250)) +
  
  scale_x_continuous(labels = NULL) +
  
  labs(y="Project Loved",
       
       x="Project Viewed",
       
       title="Project Loved Avg. vs Project Viewed"
       
  )








p1<-ggplot(scratch,aes(viewers_website,color = is_remix))+geom_area(stat = "bin")
p2<-ggplot(scratch, aes(viewers_website, color = is_remix)) + geom_dotplot(
  aes(fill = ..x..))
p3<-ggplot(scratch,aes(date_created,viewers_website, color = is_remix)) + geom_jitter()


p4<-ggplot(scratch, aes(project_id, lovers_website,color=is_remix))+geom_bar(stat="identity")
p5<-ggplot(scratch,aes(lovers_website,color = is_remix))+geom_density(kernel="gaussian")







ui <- dashboardPage(
  dashboardHeader(title = "Scratch Dataset Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Part1", tabName = "Part1", icon = icon("dashboard")),
      menuItem("Part2", tabName = "Part2", icon = icon("dashboard")),
      menuItem("Part3", tabName = "Part3", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Part1",
    fluidRow(
      box(plotOutput("plot1", height = 350)),
      
      box(plotOutput("plot2", height = 350)),
      
      box(plotOutput("plot3", height = 350)),
      
      box(plotOutput("plot4", height = 350))
      )
    ),
    tabItem(tabName = "Part2",
            fluidRow(
              box(plotOutput("plot5", height = 350)),
              
              box(plotOutput("plot6", height = 350)),
              
              box(plotOutput("plot7", height = 350))
      )
    ),
    tabItem(tabName = "Part3",
            fluidRow(
              box(plotOutput("plot8", height = 350)),
              
              box(plotOutput("plot9", height = 350))
            )
    )
    )
  )
)


server <- function(input, output) {

  
  output$plot1 <- renderPlot({
    plot(corr1)
  })
  output$plot2 <- renderPlot({
    plot(corr2)
  })
  output$plot3 <- renderPlot({
    plot(corr3)
  })
  output$plot4 <- renderPlot({
    plot(corr4)
  })
  output$plot5 <- renderPlot({
    plot(p1)
  })
  output$plot6 <- renderPlot({
    plot(p2)
  })
  output$plot7 <- renderPlot({
    plot(p3)
  })
  output$plot8 <- renderPlot({
    plot(p4)
  })
  output$plot9 <- renderPlot({
    plot(p5)
  })
}

shinyApp(ui, server)
