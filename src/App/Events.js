exports.targetFileList = function(ev) {
  if(ev.target === undefined) return []; // FIXME: should return an empty filelist somehow
  return ev.target.files;
}

exports.readFileAsText = function(callback) {
  return function(f) {
    return function() { // returns an effect
      var reader = new FileReader();
      reader.onload = function(event) {
        callback(event.target.result)(); // callback is also an effect
      };
      reader.readAsText(f);
    }
  }
}

