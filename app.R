#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(devtools)
library(readxl)
library(viridis)
library(fastDummies)
library(scales)



#reading in EMS information


ems <- read_excel(
    "MA_EMS_Opioid.xlsx",
    na = c("***", "(1-4)")
) 



#This is clearly a wide dataset, and we want to pivot to a long format to make it tidy.
ems1 <- ems %>% 
    pivot_longer(cols = 2:ncol(ems), names_to = "age.group", values_to = "count") %>% 
    mutate( #checking if the word female is in the value "age.group"
        gender = ifelse( #making a new column, "gender". if it finds "female," then it makes the row female
            grepl("Female", age.group, fixed = TRUE), 
            "Female",
            ifelse( #if it isn't female, it checks to see if it's male. if it's male, then the row is called "male"
                grepl("Male", age.group, fixed = TRUE),
                "Male",
                "Total" #otherwise, it's called total
            )
        )
    ) %>% 
    mutate(
        age.group = ifelse(
            str_detect(age.group, "[0-9]*(\\+|\\-)([0-9]*|)"), #string detect...we're looking for a range with a dash, or a number with a +
            str_extract(age.group, "[0-9]*(\\+|\\-)([0-9]*|)"), #if we do find that pattern, use string extract
            "Total" #if it doesn't detect that, put the word "Total".
        )
    )





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Suspected Opioid Overdose Calls in Massachusetts, 2013-2019, by age group"),

    # Sidebar with a checkbox input for which age groups to display 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "ages", 
                               label = "ages", 
                               choices = unique(ems1$age.group), #specifying the choices--need to specify dataframe$column, 
                               #unique gives you every unique value in a vector...gets rid of duplicates. only gives the unique values of a column in a df
                               selected = unique(ems1$age.group), #all the checkboxes will be selected, by default   
                               inline = FALSE #puts choices vertically
                               ),
            width = 2
            
 
        # Show a plot of the generated distribution
        ),    
    mainPanel(
            
           plotOutput("EMScurve")
        )
    )
)

# Define server logic required to draw a plot
server <- function(input, output) {

    
    
    output$EMScurve <- renderPlot({
        #piping in data, do a filter before doing ggplot
        #we have all these checkboxes for ages groups, and only want to display age groups that have been selected.
        #we can get the currently selected agegroups by doing "input$ages"
        #filter needs to evaluate to a true or false
        ems1 %>% 
            filter(age.group %in% input$ages) %>% #filter gives an expression evaluated row by row, and evaluates to either T/F 
            # "input$ages" is a list/vector of the currently selected choices
            # looking at each reach row. for each row in the column "age.group," it's checking if we've selected that age group. it's checking if the age group in this row is in the list of currently selected age groups 
        ggplot()+
            aes(
                x = Year, 
                y = count,
                color = age.group
            )+
            labs(x = "Year", y = "EMS Calls for Suspected OD", title = "MA Opioid-Related EMS Incidents by year, 2013-2019")+
            geom_line()+
            guides(color = guide_legend(title = "Age Group"))+ #changing the title of legend
            theme(legend.title = element_text(size=14,face ="bold"))+
            scale_y_continuous(labels = comma, limits =c(0, 25000))+ #colon notation would give a vector of every value
            scale_x_continuous(limits = c(2013, 2019), breaks = seq(2013,2019,1))+ #seq(start, end, interval)
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            theme(panel.spacing = unit(1.5, "lines"))+
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold"))+
            theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), size=14,face="bold"))+
            theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0), size=14,face="bold"))+
            facet_wrap(~gender) #by gender
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
