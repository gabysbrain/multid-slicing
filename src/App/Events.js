exports.targetFileList = function(ev) {
  if(ev.target === undefined) return []; // FIXME: should return an empty filelist somehow
  return ev.target.files;
}

