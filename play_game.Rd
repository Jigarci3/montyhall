% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_game}
\alias{play_game}
\title{Wrap is created for the game is played from start to finish}
\usage{
play_game()
}
\arguments{
\item{If}{contestant choses to switch their door during the change_door
function, then remainng closed door becomes their selection
If final pick contains a car, then contestant has won}
}
\value{
final outcome is 'win' or 'lost'
}
\description{
A new game is created and all steps are completed to determine if
contestant has won or lost
}
\details{
Game is created, a door is selected, a goat door is revealed, the #'	contestant has the option to stay with their initial pick or 
switch to remaining closed door. Depending on that selection, 
game determines whether or not the contestant has won or lost.
}
\examples{
play_game()
}
