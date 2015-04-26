angular.module("foo", []).value("fooVal", {info: "info"}});

angular.module("test", ["foo"])
// Doc for testVal
.factory("testVal", function() {
  return "hi";
});
