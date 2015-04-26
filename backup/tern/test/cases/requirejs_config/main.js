// plugin=requirejs

requirejs.config({
  paths: {
    fooAlias: "../requirejs/foo"
  }
});

require(["fooAlias"], function(foo) {
  foo.aString; //: string
});
