function buildCopy(o) {
  var oo = {};
  for (var prop in o) oo[prop] = o[prop];
  return oo;
}

buildCopy({xx: 10, yy: 20}); //:: {xx: number, yy: number}
