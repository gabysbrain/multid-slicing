
toJson.sliceSet = function(s, file) {
  cat(rjson::toJSON(s), file=file)
}

fromJson.sliceSet = function(file) {
  s = rjson::fromJSON(file=file)
  class(s) = "SliceSet"
  s$focusPoints = data.frame(s$focusPoints)
  s$slices = data.frame(s$slices)
  s
}
