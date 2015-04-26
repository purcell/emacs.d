["foo", "bar"].map(function(s) { return s.charCodeAt(0); }); //: [number]

var b = [];
b.push(true);
b; //: [bool]

var c = [];
c.push("hi");
c.push(10);
c; //: [string|number]

var d;
function setD(a) { d = a; }
setD.call(null, 55);
d; //: number
