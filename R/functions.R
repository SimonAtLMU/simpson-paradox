#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @import cowplot

#' @title Get started with Simpson Paradox
#' @description `hello_simpson` helps you to get started with the simpson-package. You don't need any input.
#' @example examples/hellosimpson.R
#' @returns A gentle introduction to the simpson package
#' @export
hello_simpson <- function() {
message("In this package you will learn more about a statistical phenomenon that was named after a british statistician named Edward Hugh Simpson. As we will see, the outcomes of this phenomenon are sometimes quite curious and paradox. To learn more about these statistical curiosities, use the function explore(). As input for this function, you need to chose a dataset. For educational purposes, we provide two example datasets in this package. Do you want to work with data on smoking behaviour and running speed? The chose the dataset called dash. Are you more interested in the development of wages in the US and how it differs in different educational subgroups? Then chose the dataset wages.")
}

#' @title Explore the datasets
#' @description `explore` generates a scatterplot with a regression line to give you a first impression of the data.
#' @param df  (dataframe dash or wages)
#' @example examples/explore.R
#' @returns A scatterplot and a description text on the console
#' @export
explore <- function(df) {
  if(substitute(df) == "dash") { #nochmal nach Alternative für substitute nachlesen

    plot <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = cigarettes_per_day,
        y = time_100_meter
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Smoked cigarettes per day",
        y = "Time for 100-meter dash (in sec)")

    message("Take a look at the scatterplot on the right! It looks like that there is a negative correlation between the number of smoked cigarettes per day and the time students need for the 100-meter dash. The regression line suggests that students that smoke more are faster in the 100-meter dash. That's strange... let's check our assumption by calculating a regression analysis. Please use the function regression_all() for this purpose. As before, use dash as input of the function.")

    return(plot)

  } else if (substitute(df) == "wages") {

    plot <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = year,
        y = wage_per_month
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Year",
        y = "Wage per month (in USD)",
      ) +
      scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013),
        labels = c("2005", "2007", "2009", "2011", "2013"))

    message("Take a look at the scatterplot on the right! Hmm... that looks kinda odd. When considering the regression line, it looks like that the monthly wages of US citizens rose overall since 2005. But by taking a look at the cases in the scatterplot, it doesn't look like that the wages really increased. Let's double check that by calculating a regression analysis. Please use the function regression_all() for this purpose. As before, use wages as input of the function.")

    return(plot)

  } else {
    stop("The dataset you chose is not in scope of this educational package. Please use the dataframe dash or wages")
  }
}

#' @title Confirm the results from the scatterplot
#' @description `regression_all` performs a regression analysis on the example data. All data (ungrouped) will be considered in the analysis.
#' @param df  (dataframe dash or wages)
#' @example examples/regression.R
#' @returns A summary of the regression analysis and an interpretation of the results.
#' @export
regression_all <- function(df) {
  if(substitute(df) == "dash") {
    regression_linear <- lm(cigarettes_per_day ~ time_100_meter, data = df)

    message("Can you believe that? The regression analysis testifies that students that smoke more are faster in the 100-meter dash. How is this possible? This result is against every intuition we have about the health effects on smoking?! Let's see how this relationship is when we differ between men and women. Use the function explore_group() for this reason.")

    return(summary(regression_linear))

  } else if (substitute(df) == "wages") {
    regression_linear <- lm(year ~ wage_per_month, data = df)

    message("Okay... the regression analysis testifies that there has been a slight increase of the wages since 2005. But the data looked really strange in the scatterplot, right? As you might remember, our dataset also contains information on the educational background of each case. Maybe we should consider this when interpreting the data. Use the function explore_group() to take the educational subgroups into account.")

    return(summary(regression_linear))

  } else {
    stop("The dataset you chose is not in scope of this educational package. Please use the dataframe dash or wages")
  }

}

#' @title Take a look at the grouped data
#' @description `explore_group` generates a scatterplot with several regression lines. Color is used to distinguish different groups from each other.
#' @param df  (dataframe dash or wages)
#' @example examples/explore_group.R
#' @returns A scatterplot and an interpretation of the results.
#' @export
explore_group <- function(df) {
  if(substitute(df) == "dash") { #nochmal nach Alternative für substitute nachlesen

    plot <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = cigarettes_per_day,
        y = time_100_meter,
        color = sex
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Smoked cigarettes per day",
        y = "Time for 100-meter dash (in sec)")

    message("What's going on right now?? When we seperate the students in groups based on their sex, we see a completely new relationship between smoking and running speed. Let's compare this to the last analysis. Use the function compare() for this reason and insert dash as function input.")

    return(plot)

  } else if (substitute(df) == "wages") {

    plot <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = year,
        y = wage_per_month,
        color = education
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Year", #x-Achse noch besser formatieren
        y = "Wage per month (in USD)",
      ) +
      scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013),
                         labels = c("2005", "2007", "2009", "2011", "2013"))

    message("What the heck?? When seperating the US citizens by their educational background the trend of the wage development suddenly changes. Now, it seems like that the salaries have shrunk since 2005. Let's compare this new result with the old one by using the function compare(). Please use wages as input again.")

    return(plot)

  } else {
    stop("The dataset you chose is not in scope of this educational package. Please use the dataframe dash or wages")
  }
}

#' @title Compare the ungrouped with the grouped results
#' @description `compare` shows two scatterplot to explain the Simpson Paradox. The text in the console explains the concept.
#' @param df  (dataframe dash or wages)
#' @example examples/explore_group.R
#' @returns A scatterplot, some intepretations and an explanation why the Simpson Paradox occurs.
#' @export
compare <- function(df) {
  if(substitute(df) == "dash") {

    plot1 <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = cigarettes_per_day,
        y = time_100_meter
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Smoked cigarettes per day",
        y = "Time for 100-meter dash (in sec)")

    plot2 <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = cigarettes_per_day,
        y = time_100_meter,
        color = sex
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Smoked cigarettes per day",
        y = "Time for 100-meter dash (in sec)") +
      theme(legend.position = "none")

    plot_merged <- plot_grid(plot1, plot2, labels = c('Total', 'Grouped'), label_size = 9)

    message("The thing we can see right here is a perfect example of the Simpson Paradox. We get different results depending on the groups we take into account in our regression analysis: when looking at all students, smoking cigarettes seems to have a positive impact on running speed (or negative on the time needed - shown in the scatterplot). But when we differ between sexes, we see that smoking has a negative effect on running speed (Students that smoke more need more time for the 100-meter dash). The Simpson paradox occurs because the two sexes differ in one characteristic that we did not consider at all in the first analysis: smoking behaviour. We see that male students tend to smoke more than female students. Just because boys run faster than girls on average (maybe because of physiological differences), the regression analyses of the whole group suggests that smoking more make you run faster. But as we know, that's untrue! So always think of confunding variables when developing a study or analyzing data. The Simpson Paradox might be in your data :)")

    return(plot_merged)

  } else if (substitute(df) == "wages") {

    plot1 <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = year,
        y = wage_per_month
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Year", #x-Achse noch besser formatieren
        y = "Wage per month (in USD)",
      ) +
      scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013),
                         labels = c("2005", "2007", "2009", "2011", "2013"))

    plot2 <- ggplot2::ggplot(
      data = df,
      mapping = aes(
        x = year,
        y = wage_per_month,
        color = education
      )) +
      geom_point() +
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(
        x = "Year", #x-Achse noch besser formatieren
        y = "Wage per month (in USD)",
      ) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013),
                         labels = c("2005", "2007", "2009", "2011", "2013"))

    plot_merged <- plot_grid(plot1, plot2, labels = c('Total', 'Grouped'), label_size = 9)

    message("The thing we can see right here is a perfect example of the Simpson Paradox. We get different results depending on the groups we take into account in our regression analysis: when looking at all citizens (Total), it seems like wages increasd slightly over the years. But when we look at the educational subgroups (Grouped), we see that in all of these groups wages decreased. How is this possible? Well, the explanation for this lies in the changing educational profile of workforce in the US since 2005. As you can see from the data, the number of people making a college degree (red dots) increased. On that, the wages for college graduates have fallen at a much slower rate than for people in other educational subgroups. The increase of people making a college degree and the relatively slower rate of wage decrease in this group is responsible for a misleading result of the first regression analysis. So always keep in mind that your perception of data can change depending on your viewpoint on it. Don't easily trust the results of a single analysis. :)")

    return(plot_merged)

  } else {
    stop("The dataset you chose is not in scope of this educational package. Please use the dataframe dash or wages")
  }
}

## usethis namespace: end
NULL
