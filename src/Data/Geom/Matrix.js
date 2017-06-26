exports.solver = function (n) {
  return function (A) {
    var x = new Array(n)
    for (var i=n-1; i>-1; i--) {
      x[i] = A[i][n] / A[i][i]
      for (var k = i-1; k>-1; k--) {
          A[k][n] -= A[k][i] * x[i]
      }
    }
    return x
  }
}
