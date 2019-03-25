

createSliceSet <- function(problemSpec, focusPoints, slices) {
  r = list(problemSpec=problemSpec, focusPoints=focusPoints, slices=slices)
  class(r) = "SliceSet"
  r
}
