var util = require("./util");
var tern = require("../lib/tern");
var fs = require("fs");

var file = fs.readFileSync(util.resolve("test/data/large.js"), "utf8");

var server = new tern.Server({defs: [util.ecma5]});

var curTest;
function fail(msg) { throw curTest + ": " + msg; }
function eq(a, b, msg) { if (a != b) fail(msg || a + " != " + b); }

exports.runTests = function(filter) {
  var added = false;
  function test(name, start, text, query, check) {
    name = "fragments/" + name;
    if (filter && name.indexOf(filter) == -1) return;
    if (!added) {
      server.request({files: [{type: "full", name: "file", text: file}]}, function(){});
      util.addFile();
      added = true;
    }
    util.addTest();
    curTest = name;
    if (typeof text == "number") text = file.slice(start, text);
    query.file = "#0";
    var fdata = {type: "part", name: "file", text: text, offset: start};
    server.request({files: [fdata], query: query}, function(err, data) {
      try {
        if (err) fail(err);
        else check(data);
      } catch (e) {
        util.failure(String(e));
      }
    });
  }

  test("simple", 864, 1065, {type: "definition", end: 59},
       function(data) { eq(data.start, 888); });
  test("only_body", 895, 1064, {type: "definition", end: 28},
       function(data) { eq(data.start, 888); });
  test("replace_body", 895, "\n  file", {type: "definition", end: 7},
       function(data) { eq(data.start, 888); });

  test("long_line", 6141, 6774, {type: "type", end: 603},
       function(data) { eq(data.type, "bool"); });
  test("long_line_replace", 6141, "var x = 'hi'; x;", {type: "type", end: 15},
       function(data) { eq(data.type, "string"); });

  test("redefine", 2444, "var defaults = 900; // and a long comment to cover the whole exprssion\n",
       {type: "type", end: 0, variable: "defaults"},
       function(data) { eq(data.type, "number"); });

  test("find_def", 3424, "function foo() { hasFrontMatter; }",
       {type: "definition", end: 31},
       function(data) { eq(data.start, 873); });
  test("find_def_in_fragment", 3420, file.slice(3424, 5103),
       {type: "definition", end: 1280},
       function(data) { eq(data.start, 3429); });
};
