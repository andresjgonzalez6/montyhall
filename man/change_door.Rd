% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem-Gonzalez.R
\name{change_door}
\alias{change_door}
\title{Choice to stay with first choice or switch doors.}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{The}{variables 'stay, 'a.pick', and 'opened.door' are inserted
into the 'change_door' function.}
}
\value{
The function returns a length 1 numeric vector to indicate
a final door pick to finish the game. It will either
be 1, 2, or 3.
}
\description{
Can either stay with original door 'a.pick', or switch
to the final door not exposed during 'open_goat_door' function.
}
\details{
This function provides a choice to the player, to either
stay with their first pick 'a.pick', or to switch to
the the other door. By this point in the game the host
has already revealed a goat door. The logic behind
'change_door' function is to either 'stay' and maintain 'a.pick',
or to choose the door that is not 'a.pick' and also not
the revealed 'opened.door' with a goat. Whichever door is
chosen will be forwarded into the function as 'final.pick'.
The variable 'stay' can either be T or F, which is TRUE or
FALSE. T is default, where the 'final.pick' will stay with
the original choice of 'a.pick'. F would change the door
for 'final.pick'.
}
\examples{
change_door( stay=T, opened.door=2 , a.pick=3 )
change_door(F, 3, 1)
change_door(T, 1, 3)

}
