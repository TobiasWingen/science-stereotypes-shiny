# Load packages ----
library(shiny)
library(ggplot2)
#for text in Figures
library(ggrepel)
# Load data ----
data_trupol <- readRDS("data/data_trupol.rds")


# Create User interface ----
ui <- fluidPage(
    title = h1("Stereotypes about scientists", align = "center"),
  #Users can select the axis
  fluidRow(
    column(2,
           h4("Axes selection"),
           helpText("How does a sample of 1,000 US-Americans view scientists regarding their stereotypic"),
           selectInput("varx", 
                       label = "Choose x-Axis",
                       choices = c("Power", "Political Orientation",
                                   "Warmth", "Trustworthiness"),
                       selected = "Political Orientation"),
           
           selectInput("vary", 
                       label = "Choose y-Axis",
                       choices = c("Power", "Political Orientation",
                                   "Warmth", "Trustworthiness"),
                       selected = "Trustworthiness"),
           
           radioButtons("desire_reg", 
                        label = "Add a regression line?",
                        choices = c("No","Yes"),
                        selected = "No"),
           
           radioButtons("desire_fix", 
                        label = "Axes limits:",
                        choices = c("Full range","Data-dependent"),
                        selected = "Data-dependent")
    ),
    #A plot with table, plot and explanation
    column(8, 
           h4("Stereotypes about scientists"),
      tabsetPanel(
        tabPanel("Plot", plotOutput("map")), 
        tabPanel("Table", tableOutput("table")),        tabPanel("Background", p("We asked 1,000 US-Americans to rate 20 groups of scientists regarding their typical attributes. Participants indicated scientists' stereotypic political orientation (What is the political orientation of [e.g., mathematicians]?: 0 = very liberal, 10 = very conservative), power (How powerful are...?: 0 = very powerless, 10 = very powerful, also called agency), warmth (How warm are...?: 0 = very cold, 10 = very warm, also called communion), and finally trustworthiness (How trustworthy are...?: 0 = very untrustworthy, 10 = very trustworthy).", style = "font-family: 'times'; font-si16pt"),
                                                                 p("In this shiny app, you can now plot the average rating across all participants for two selected stereotype-dimensions. You can also have a look at the exact values in the Table-tab. Finally, you have the option to filter the participants, so that you can see how scientists are viewed by different societal groups.", style = "font-family: 'times'; font-si16pt"),p("You can filter according to three criteria. 1.) Participants' political orientation (self-reported on a 0 (very liberal) to 10 (very conservative)-scale, with values of 4 - 6  classified as moderate), 2.) participants' age and 3.) participants' gender (We focus on men and women as possible subsamples, as the sample sizes for other reported genders were too low to allow interpretation).", style = "font-family: 'times'; font-si16pt"),p("More information about our work is presented in our", a("Preprint", href="https://osf.io/tdukb"),". Our code and data to create this shiny-app are shared on the", a("Open Science Framework.", href="https://osf.io/rvj4q/?view_only=7f6e3a0c993e427395e8f1c0447c69e1")),p("App created by Andrea Wingen and Tobias Wingen.", style = "font-family: 'times'; font-si16pt; font-style: italic")),
        tabPanel("Citation", p("If you use this app for your academic work, please cite our preprint:  Altenmüller, M. S., Wingen, T., & Schulte, A. (2022, November 23). Explaining polarized trust in scientists: A political stereotype-approach. https://doi.org/10.31219/osf.io/tdukb"))
        
      )
      ),
    column(2,
           h4("Filter criteria"),
           helpText("As viewed by people with the following:"),
           radioButtons("politics", 
                        label = "Political orientation: ",
                        choices = c("All","Conservative", "Liberal", "Moderate"),
                        selected = "All"),
           
           radioButtons("gender", 
                        label = "Gender: ",
                        choices = c("All","Men", "Women"
                        ),
                        selected = "All"),
           
           radioButtons("age", 
                        label = "Age: ",
                        choices = c("All","Under 50 years", "50 or older"),
                        selected = "All"))
    )
  )



# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    #create filter criteria
    ##load input by user
    gender_filter <- switch(input$gender, 
                            "Men" = 1,
                            "Women" = 2,
                            "All" = 3)
    age_filter <- switch(input$age, 
                         "Under 50 years" = "young",
                         "50 or older" = "old",
                         "All" = "3")
    pol_filter <- switch(input$politics, 
                         "Conservative" = "Con",
                         "Liberal" = "Lib",
                         "Moderate" = "Mod",
                         "All" = "3")
    ##subset based on gender
    if(gender_filter==3){ ##include all/no subsetting
      data_gender<-subset(data_trupol)
    }else{
      data_gender<-subset(data_trupol, gender == gender_filter)
    }
    
    ##subset based on age
    
    #median age (after subsetting??)
    median_age <- median(data_trupol$age, na.rm=T) 
    
    if(age_filter=="3"){
      data_age<-subset(data_gender)
    }else if(age_filter == "young"){
      data_age <- subset(data_gender, age < 50)
    }else if(age_filter == "old"){
      data_age <- subset(data_gender, age >= 50)
    }
    
    ##subset based on political orientation
    if(pol_filter==3){ ##include all/no subsetting
      data_pol<-subset(data_age)
    }else if(pol_filter=="Con"){
      data_pol<-subset(data_age, part_beliefs >= 7)
    }else if(pol_filter=="Lib"){
      data_pol<-subset(data_age, part_beliefs <= 3)
    }else{
      data_pol<-subset(data_age, part_beliefs > 3 & part_beliefs < 7)}
    #create dataset based on filter selections
    data<-data_pol
    df_t<-data[,grep(".Trust", names(data), value=TRUE)]
    means_t<-colMeans(df_t, na.rm = T)
    df_b<-data[,grep(".Beliefs", names(data), value=TRUE)]
    means_b<-colMeans(df_b, na.rm = T)
    df_c<-data[,grep(".Communion", names(data), value=TRUE)]
    means_c<-colMeans(df_c, na.rm = T)
    df_a<-data[,grep(".Agency", names(data), value=TRUE)]
    means_a<-colMeans(df_a, na.rm = T)
    means_abct.df<-as.data.frame(cbind(means_a,means_b,means_c,means_t))#rename
    rownames(means_abct.df) =c("Anthropologists","Mathematicians","Physicist","Astronomers","Chemists","Biologists","Climate scientists","Computer scientists","Economists","Theologians","Environmental scientists","Linguists","Medical scientists","Psychological scientists","Sociologists","Political scientists","Historians","Legal scholars","Philosophers","Art scholars")
    text<-rownames(means_abct.df)
    
    #create axis based on choice
    x_axis_choice <- switch(input$varx, 
                            "Power" = means_a,
                            "Political Orientation" = means_b,
                            "Warmth" = means_c,
                            "Trustworthiness" = means_t)
    y_axis_choice <- switch(input$vary, 
                            "Power" = means_a,
                            "Political Orientation" = means_b,
                            "Warmth" = means_c,
                            "Trustworthiness" = means_t)
    
    y_legend <- switch(input$vary, 
                       "Power" = "Power (0 - 10)",
                       "Political Orientation" = "Political Orientation (0 = v. liberal - 10 = v.conservative)",
                       "Warmth" = "Warmth (0 - 10)",
                       "Trustworthiness" = "Trustworthiness (0 -10 )")
    x_legend <- switch(input$varx, 
                       "Power" = "Power (0 - 10)",
                       "Political Orientation" = "Political Orientation (0 = v. liberal - 10 = v. conservative)",
                       "Warmth" = "Warmth (0 -10)",
                       "Trustworthiness" = "Trustworthiness (0 -10)")
    #plot with text
    #sample size after selection for captions
    samplesize_select <- nrow(data)
    
    plot<-ggplot(means_abct.df, aes(x = x_axis_choice, y = y_axis_choice)) +  geom_point()+ 
      xlab(x_legend) +# for the x axis label
      ylab(y_legend)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=18),panel.background = element_blank(), axis.line = element_line(colour = "black"),plot.title = element_text(hjust = 0.5, size = 14))+
      labs(caption = paste0("Sample sized based on user selection: ", samplesize_select))+
      geom_label_repel(aes(label = text), size = 4.5, max.overlaps = 100) 
    if(input$desire_fix=="Data-dependent" & input$desire_reg == "No"){
      plot
      }
    else if(input$desire_fix=="Data-dependent" & input$desire_reg == "Yes")  {
      plot +  geom_smooth(method='lm', se = F)
      }
    else if(input$desire_fix=="Full range" & input$desire_reg == "No")
    {
      plot + scale_x_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0:10)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0:10))
    }
    else
    {
      plot+ scale_x_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0:10)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0:10))+geom_smooth(method='lm', se = F)
    }
      
    
    
  })
  output$table <- renderTable({
    #create filter criteria
    ##load input by user
    gender_filter <- switch(input$gender, 
                            "Men" = 1,
                            "Women" = 2,
                            "All" = 3)
    age_filter <- switch(input$age, 
                         "Under 50 years" = "young",
                         "50 or older" = "old",
                         "All" = "3")
    pol_filter <- switch(input$politics, 
                         "Conservative" = "Con",
                         "Liberal" = "Lib",
                         "Moderate" = "Mod",
                         "All" = "3")
    ##subset based on gender
    if(gender_filter==3){ ##include all/no subsetting
      data_gender<-subset(data_trupol)
    }else{
      data_gender<-subset(data_trupol, gender == gender_filter)
    }
    
    ##subset based on age
    
    #median age (after subsetting??)
    median_age <- median(data_trupol$age, na.rm=T) 
    
    if(age_filter=="3"){
      data_age<-subset(data_gender)
    }else if(age_filter == "young"){
      data_age <- subset(data_gender, age < 50)
    }else if(age_filter == "old"){
      data_age <- subset(data_gender, age >= 50)
    }
    
    ##subset based on political orientation
    if(pol_filter==3){ ##include all/no subsetting
      data_pol<-subset(data_age)
    }else if(pol_filter=="Con"){
      data_pol<-subset(data_age, part_beliefs >= 7)
    }else if(pol_filter=="Lib"){
      data_pol<-subset(data_age, part_beliefs <= 3)
    }else{
      data_pol<-subset(data_age, part_beliefs > 3 & part_beliefs < 7)}
    #create dataset based on filter selections
    data<-data_pol
    df_t<-data[,grep(".Trust", names(data), value=TRUE)]
    means_t<-colMeans(df_t, na.rm = T)
    df_b<-data[,grep(".Beliefs", names(data), value=TRUE)]
    means_b<-colMeans(df_b, na.rm = T)
    df_c<-data[,grep(".Communion", names(data), value=TRUE)]
    means_c<-colMeans(df_c, na.rm = T)
    df_a<-data[,grep(".Agency", names(data), value=TRUE)]
    means_a<-colMeans(df_a, na.rm = T)
    means_abct.df<-as.data.frame(cbind(means_a,means_b,means_c,means_t))#rename
    rownames(means_abct.df) =c("Anthropologists","Mathematicians","Physicist","Astronomers","Chemists","Biologists","Climate scientists","Computer scientists","Economists","Theologians","Environmental scientists","Linguists","Medical scientists","Psychological scientists","Sociologists","Political scientists","Historians","Legal scholars","Philosophers","Art scholars")
    text<-rownames(means_abct.df)
    
    #create axis based on choice
    x_axis_choice <- switch(input$varx, 
                            "Power" = means_a,
                            "Political Orientation" = means_b,
                            "Warmth" = means_c,
                            "Trustworthiness" = means_t)
    y_axis_choice <- switch(input$vary, 
                            "Power" = means_a,
                            "Political Orientation" = means_b,
                            "Warmth" = means_c,
                            "Trustworthiness" = means_t)
    
    y_legend <- switch(input$vary, 
                       "Power" = "Power (0 = very powerless-10 = very powerful)",
                       "Political Orientation" = "Political stereotypes (0 = very liberal-10 = very conservative)",
                       "Warmth" = "Warmth (0 = very cold-10 = very warm)",
                       "Trustworthiness" = "Trustworthiness (0 = very untrustworthy-10 = very trustworthy)")
    x_legend <- switch(input$varx, 
                       "Power" = "Power (0 = very powerless-10 = very powerful)",
                       "Political Orientation" = "Political stereotypes (0 = very liberal-10 = very conservative)",
                       "Warmth" = "Warmth (0 = very cold-10 = very warm)",
                       "Trustworthiness" = "Trustworthiness (0 = very untrustworthy-10 = very trustworthy)")
   
    data1<-cbind(text,round(x_axis_choice, digits = 3), round(y_axis_choice, digits = 3))
    colnames(data1) <- c("Scientists",input$varx, input$vary)    # Applying colnames

    data1                                   # Print updated data
     
  })
}

# Run app ----
shinyApp(ui, server)