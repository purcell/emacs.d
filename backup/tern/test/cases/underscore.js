// environment=underscore

var cloneObjOrig = {a: 1}, cloneObjCopy = _.clone(cloneObjOrig);
cloneObjOrig.a; //: number
cloneObjCopy.a; //: number

var extendObj = {a: 1, b: true};
_.extend(extendObj, {c: ''}, {d: 2});
extendObj; //:: {a: number, b: bool, c: string, d: number}

var defaulted = {x: 10};
_.defaults(defaulted, {y: ""});
defaulted.x; //: number
defaulted.y; //: string

_.has({}, "foo"); //: bool
_.isNaN(NaN); //: bool

var tapped;
_.tap(11, function(a) { tapped = a; });
tapped; //: number

_.reduce(["foo", "bar"], function(sum, s) { return sum + s.length; }, 0); //: number
_.map([1, 2, 3], toString); //: [string]
_.each([true, false], function(val, i) {
  val; //: bool
  i; //: number
  this.aa; //: number
}, {aa: 4});
