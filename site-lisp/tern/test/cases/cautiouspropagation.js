var grabbag = {};
grabbag[foo()] = "hi";
grabbag[bar()] = {abc: 10};
grabbag[baz()] = [1, 2, 3];
var inner = 55 || grabbag[quux()];
inner; //: number

var simple = {};
simple[foo()] = "a";
simple[bar()] = "b";
simple[baz()] = "c";
simple[quux()]; //: string
