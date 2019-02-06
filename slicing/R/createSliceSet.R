

createSliceSet <- function(focusPoints, slices) {
  r = list(focusPoints=focusPoints, slices=slices)
  class(r) = "SliceSet"
  r
}
