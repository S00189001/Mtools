mvis <- function(df, x, y = NULL, x_label = "NULL", y_label = "NULL", colorset = 1, categoryCol = "NULL")
{
  # TODO: Create a collection of colours to use ex: 1, 2, 3

  colorset <- CreateColorSets(colorset)
  theme <- CreateThemeSets()
  #colorset <- c("blue", "green", "purple", "gold")


  #if (colorset == "colorset1")
   # { colorset <- c("green", "red") }

  # TODO: Create a collection of theme sets to apply to graphs

  # If y is NULL, Then Univariate otherwise Bivariate
  if (!is.null(y))
  { uniVar = FALSE }
  else
  { uniVar = TRUE  }


  #Check x and y variable type
  if (uniVar == TRUE)
  { x_Type <- CheckVariableTypes(x) }
  else
  {
    x_Type <-  CheckVariableTypes(x)
    y_Type <-  CheckVariableTypes(y)

    # TODO: After check, Must ensure X is Category and Y is Continuous if Bivariate
    #if (x_Type == "Categorical") {
    #}
    #else
      if (x_Type == "Continious" & y_Type == "Categorical"){
      # The old switch a roo
      tmp <- x
      x <- y
      y <- tmp

      tmp <- x_Type
      x_Type <- y_Type
      y_Type <- tmp

      tmp <- x_label
      x_label <- y_label
      y_label <- tmp

    #}
    #else {
     # break
    }
  }

  CreateViz(df, x, y, x_Type, y_Type, x_label, y_label, uniVar, colorset, categoryCol, theme)



}


CreateViz <- function(df_, x_, y_ = NULL, x_Type_, y_Type_, x_label, y_label, uniVar_, colorset_, categoryCol_, theme_)
{
  # Univariate
  if (uniVar_)
  {
    if (x_Type_ == "Continious")  {
      # Create Continuous Univariate Plot
      ggplot(data = df_) +
        geom_histogram(mapping = aes(x = x_, fill = "Single")) +
        labs(title = paste("Univariate Plot for", x_label), x = x_label) +
        scale_fill_manual(values = colorset_[[c(1)]]) +
        theme_

    }
    else if (x_Type_ == "Categorical")  {
      # Create Categorical Univariate Plot
      #Theme1, Colour1
      ggplot(data = df_) +
        geom_bar(mapping = aes(x = x_, fill = x_)) +
        labs(title = paste("Univariate Plot for", x_label), x = x_label) +
        scale_fill_manual(values = colorset_[[c(1)]])+
        theme_
        #scale_fill_manual(values = c("red", "blue"))
    }
  }
  # Bivariate
  else if (!uniVar_)
  {
    if (x_Type_ == "Categorical" & y_Type_ == "Categorical") {
      ggplot(data = df_) +
        geom_count(mapping = aes(x = x_, y = y_, col = x_, fill = x_)) +
        labs(title = paste(x_label,"vs.",y_label), x = x_label, y = y_label) +
        scale_fill_manual(values = colorset_[[c(1)]]) +
        scale_color_manual(values = colorset_[[c(1)]])+
        theme_
    }
    else if (x_Type_ == "Categorical" & y_Type_ == "Continious"){
      ggplot(data = df_) +
        geom_boxplot(mapping = aes(x = x_, y = y_, fill = x_)) +
        labs(title = paste(x_label,"vs.",y_label), x = x_label, y = y_label) +
        scale_fill_manual(values = colorset_[[c(1)]])+
        theme_

     # ggplot(data = df_) +
       # geom_col(mapping = aes(x = x_, y = y_, fill = x_)) +
      #  labs(title = paste(x_label,"vs.",y_label), x = x_label, y = y_label) +
      #  scale_fill_manual(values = colorset_[[c(1)]])+
       # theme_
    }
    else if (x_Type_ == "Continious" & y_Type_ == "Continious") {
      ggplot(data = df_) +
        geom_point(mapping = aes(x = x_, y = y_, col = categoryCol_)) +
        labs(title = paste(x_label,"vs.",y_label), x = x_label, y = y_label) +
        scale_color_manual(values = colorset_[[c(1)]])+
        theme_
    }

  }
}


CreateColorSets <- function(colorset_)
{
  #colString <- as.character(colorset_)

  colorset1 <- list("blue", "red", "green", "purple")
  colorset2 <- list("#005555", "#069A8E", "#F7FF93","#005555", "#069A8E", "#F7FF93")
  colorset3 <- list("#1D1CE5", "#4649FF", "#7978FF", "#C47AFF", "#1D1CE5", "#4649FF", "#7978FF", "#C47AFF")
  colorset4 <- list("cyan", "gold", "purple", "blue")

  #DicColorSet <- c("1" = colorset1, "2" = colorset2, "3" = colorset3)

  listColorSet <- list(colorset1, colorset2, colorset3, colorset4)

  return(listColorSet[colorset_])
  #return(DicColorSet[colorset_])
}

CheckVariableTypes <- function(x_)
{
  if (is.numeric(x_) & !is.factor(x_))
  {
    #print("Continious")
    return("Continious")
  }
  else if (is.character(x_) || is.factor(x_))
  {
    #print("Categorical")
    return("Categorical")
  }
}

CreateThemeSets <- function()
{
  # myTheme -----------------------------------------------------------------
  # Setting up theme for viz
  myTheme <-  theme(
    panel.background = element_rect(fill = "black", colour = "black"),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(colour = "#008b8e", linetype = 1, size = 0.2),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#008b8e", linetype = 1, size = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    #Text
    title = element_text(colour = "#00d1d6", size = 13),
    axis.text = element_text(colour = "steelblue", face = "italic"),
    axis.title = element_text(colour = "#00d1d6"),
    axis.ticks = element_line(colour = "steelblue"),
    # Plot
    plot.background = element_rect(fill = "black"),
    # Legend
    legend.background = element_rect(fill = "transparent"),
    legend.text = element_text(colour = "steelblue"),
    legend.position = "right",
    legend.key = element_rect(fill = "transparent")
  )

  return(myTheme)
}
