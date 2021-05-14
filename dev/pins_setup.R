# establish board with GitHub
library(pins)

board_register_local(name = "blah", cache = "prototyping/board_stuff")

board_register_github(repo = "rpodcast/hotshots.pinboard")

board_register_dospace()
df <- pin_get("hotshots_race_results", board = "dospace")


data(mtcars)
pin(mtcars, board = "blah")


pin_get("mtcars", board = "blah")


pin(iris, board = "blah")
pin_get("iris", board = "blah")


pin("prototyping/test.png", board = "blah")
