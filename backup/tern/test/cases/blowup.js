var Ext = Ext || {};
Ext.extend = function(subclass, superclass) {
  superclass = subclass;
  subclass = function() { superclass.apply(this, arguments); };

  var F = function() {};
  F.prototype = superclass.prototype;
  subclass.prototype = new F();

  subclass.extend = function(o) { return Ext.extend(subclass, o); };
};

var Ext = Ext || {};
Ext.extend = function(subclass, superclass) {
  superclass = subclass;
  subclass = function() { superclass.apply(this, arguments); };

  var F = function() {};
  F.prototype = superclass.prototype;
  subclass.prototype = new F();

  subclass.extend = function(o) { return Ext.extend(subclass, o); };
};
