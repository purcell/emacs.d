var tern = require("../lib/tern");
var util = require("./util");

var tests = [], added = false;
function test(name, f) {
  tests.push(function(filter) {
    if (filter && name.indexOf(filter) == -1) return;
    if (!added) { util.addFile(); added = true; }
    util.addTest();
    
    f(new tern.Server({defs: [util.ecma5], debug: true}));
  });
}

exports.runTests = function(filter) {
  tests.forEach(function(test) { test(filter); });
};

test("reloadCall", function(server) {
  server.addFile("call.js", "myfun('2');");
  server.addFile("fun.js", "function myfun(x) {\n  x;\n}");
  server.flush(function() {
    var query = {files: [{name: "fun.js", type: "full", text: "function myfun(xy) {\n  xy;\n}"}],
                 query: {type: "type", end: {line: 1, ch: 4}, file: "fun.js"}};
    server.request(query, function(err, data) {
      if (err) return util.failure(err);
      if (data.type != "string") util.failure("reloadCall: Reloading function cleared argument type");
    });
  });
});

test("reloadProps", function(server) {
  server.addFile("obj.js", "var o = {};\no.id = 1234;\no.name = 'test';");
  server.flush(function() {
    var query = {files: [{name: "obj.js", type: "full", text: "var o = {};\no.kapow = 1234;\no.name = 'test';\no."}],
                 query: {type: "completions", end: {line: 3, ch: 2}, file: "obj.js"}};
    server.request(query, function(err, data) {
      if (err) return util.failure(err);
      var compls = data.completions;
      compls.sort();
      if (compls.join() != "kapow,name") util.failure("reloadProps: Reloading properties failed (" + compls + ")");
    });
  });
});

test("reloadVar", function(server) {
  server.addFile("a.js", "var x = 1;");
  server.flush(function() {
    server.request({files: [{name: "a.js", type: "full", text: "var x = 'hi';"}],
                    query: {type: "type", end: {line: 0, ch: 5}, file: "a.js"}}, function(err, data) {

      if (err) return util.failure(err);
      if (data.type != "string")
        util.failure("reloadVar: var did not get new string type (" + data.type + ")");
    });
  });
});

test("reloadForeignProp", function(server) {
  server.addFile("a.js", "Date.foo = 100;");
  server.flush(function() {
    server.request({files: [{name: "a.js", type: "full", text: "Date.foo"}],
                    query: {type: "type", end: {line: 0, ch: 8}, file: "a.js"}}, function(err, data) {
      if (err) return util.failure(err);
      if (data.type != "?")
        util.failure("reloadForeignProp: reload did not clear Date.foo (" + data.type + ")");
    });
  });
});

test("reloadCalledForeignProp", function(server) {
  server.addFile("a.js", "Date.foo = function(a) { return a; };");
  server.addFile("b.js", "Date.foo('hi');");
  server.flush(function() {
    server.request({files: [{name: "a.js", type: "full", text: "Date.foo = function(a) { return a + 1; };"}],
                    query: {type: "type", end: {line: 0, ch: 33}, file: "a.js"}}, function(err, data) {
      if (err) return util.failure(err);
      if (data.type != "string")
        util.failure("reloadCalledForeignProp: reload cleared argument type for Date.foo (" + data.type + ")");
    });
  });
});

test("reloadDelFile", function(server) {
  server.addFile("a.js", "function foobar() { return 10; }");
  server.flush(function() {
    server.delFile("a.js");
    server.request({files: [{name: "b.js", type: "full", text: "foobar"}],
                    query: {type: "type", end: {line: 0, ch: 6}, file: "b.js"}}, function(err, data) {
      if (err) return util.failure(err);
      if (data.type != "?")
        util.failure("reloadDelFile: deleting a file did not clear the variable it created (" + data.type + ")");
    });
  });
});
