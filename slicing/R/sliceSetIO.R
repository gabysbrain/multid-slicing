
toJson.sliceSet = function(s, file) {
  cat(rjson::toJSON(s), file=file)
}
