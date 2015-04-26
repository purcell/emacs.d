// environment=browser
// environment=jquery
// plugin=requirejs {"override": {"jquery": "=$"}}

requirejs(["foo", "bar!abc", "useexports", "simplifiedcommon", "subdir/zap", "module_exports"],
          function(foo, bar, useexports, simplified, zap, module_exports) {
  foo.aString; //: string
  bar.aNumber; //: number
  bar.baz.bazProp; //: Date
  bar.baz.bazFooProp; //: string
  useexports.hello; //: bool
  simplified.hello; //: string
  simplified.func; //: fn() -> bool
  zap; //: string
  module_exports; //:: {one: number, two: number}

  foo; //origin: foo.js
  bar; //origin: bar.js
  bar.baz; //origin: baz.js
});

requirejs(["jquery"], function($) {
  $.fx.off; //: bool
});

requirejs(["require"], function(require) {
  require("jquery").fx.off; //: bool
  require("requireme").someprop; //: string
});

requirejs(["named"], function(named) {
  named.foo; //:: {a: number}
});

requirejs.config({
  p //+ packages, paths, ...
});

requirejs.config({
  //+ baseUrl, config, context, map, nodeIdCompat, packages, paths, shim, ...
});

requirejs.config({
  baseUrl: '',
  //+ config, context, map, nodeIdCompat, packages, paths, shim, ...
});
