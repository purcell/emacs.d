// environment=jquery
// plugin=requirejs {"override": {"jquery": "=$"}}

requirejs(["foo", "bar!abc", "useexports", "simplifiedcommon"], function(foo, bar, useexports, simplified) {
  foo.aString; //: string
  bar.aNumber; //: number
  bar.baz.bazProp; //: Date
  bar.baz.bazFooProp; //: string
  useexports.hello; //: bool
  simplified.hello; //: string
  simplified.func; //: fn() -> bool
});

requirejs(["jquery"], function($) {
  $.fx.off; //: bool
});
