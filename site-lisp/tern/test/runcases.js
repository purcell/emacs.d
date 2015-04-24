var fs = require("fs"), path = require("path");
var infer = require("../lib/infer");
var tern = require("../lib/tern");
var acorn = require("acorn");
var walk = require("acorn/util/walk");
require("../plugin/requirejs.js");
require("../plugin/node.js");
require("../plugin/doc_comment.js");
var util = require("./util");

var defData = {
  browser: util.browser,
  jquery: util.jquery,
  underscore: util.underscore
};

function getDefs(text) {
  var spec = /\/\/ environment=(\w+)\n/g, m, defs = [util.ecma5];
  while (m = spec.exec(text)) {
    var data = defData[m[1]];
    if (!data) throw new Error("Unknown environment: " + m[1]);
    defs.push(data);
  }
  return defs;
}

function getPlugins(text) {
  var spec = /\/\/ plugin=(\w+)(?: (.*))?\n/g, m, plugins = {doc_comment: true};
  while (m = spec.exec(text))
    plugins[m[1]] = (m[2] && JSON.parse(m[2])) || (m[1] == "node" && {modules: nodeModules}) || {};
  return plugins;
}

fs.readdirSync(util.resolve("test/cases/defs")).forEach(function(name) {
  if (/\.json$/.test(name))
    defData[path.basename(name, ".json")] =
      JSON.parse(fs.readFileSync(util.resolve("test/cases/defs/" + name), "utf8"));
});

var nodeModules = {};
fs.readdirSync(util.resolve("test/cases/node_modules")).forEach(function(name) {
  if (/\.json$/.test(name))
    nodeModules[path.basename(name, ".json")] =
      JSON.parse(fs.readFileSync(util.resolve("test/cases/node_modules/" + name), "utf8"));
});

function serverOptions(context, text) {
  return {
    defs: getDefs(text),
    getFile: function(name) { return fs.readFileSync(path.resolve(context, name), "utf8"); },
    debug: true,
    projectDir: context,
    plugins: getPlugins(text)
  };
}

exports.runTests = function(filter) {
  var caseDir = util.resolve("test/cases") + "/";
  fs.readdirSync(caseDir).forEach(function(name) {
    if (filter && name.indexOf(filter) == -1) return;

    util.addFile();
    var fname = name, context = caseDir;
    if (fs.statSync(path.resolve(context, name)).isDirectory()) {
      if (name == "node_modules" || name == "defs") return;
      context += name + "/";
      fname = "main.js";
    }
    var text = fs.readFileSync(context + fname, "utf8");
    var server = new tern.Server(serverOptions(context, text));
    server.addFile(fname);
    var ast = server.files[0].ast;

    var typedef = /\/\/(<)?(\+|::?|:\?|doc:|loc:|refs:|exports:) *([^\r\n]*)/g, m;
    function fail(m, str) {
      util.failure(name + ", line " + acorn.getLineInfo(text, m.index).line + ": " + str);
    }

    while (m = typedef.exec(text)) (function(m) {
      var args = m[3], kind = m[2], directlyHere = m[1];
      util.addTest();
      if (kind == "+") { // Completion test
        var columnInfo = /\s*@(\d+)$/.exec(args), pos = m.index;
        if (columnInfo) {
          var line = acorn.getLineInfo(text, m.index).line;
          pos = {line: line - 1, ch: Number(columnInfo[1]) - 1};
          args = args.slice(0, columnInfo.index);
        } else {
          while (/\s/.test(text.charAt(pos - 1))) --pos;
        }
        var query = {type: "completions", end: pos, file: fname, guess: false};
        var andOthers = /,\s*\.\.\.$/.test(args);
        if (andOthers) args = args.slice(0, args.lastIndexOf(","));
        var parts = args ? args.split(/\s*,\s*/) : [];
        server.request({query: query}, function(err, resp) {
          if (err) throw err;
          var match = andOthers || parts.length == resp.completions.length;
          for (var i = 0; i < parts.length && match; ++i)
            if (resp.completions.indexOf(parts[i]) < 0) match = false;
          if (!match)
            fail(m, "Completion set failed at hint: " + parts[i - 1] +
                 "\n     got: " + resp.completions.join(", ") + "\n  wanted: " + args);
        });
      } else if (kind == "exports:") {
        server.request({query: {type: "node_exports", file: fname}}, function(err, resp) {
          if (err) throw err;
          if (resp.type != args) fail(m, "Export type failed. Got:\n    " + resp.type + "\nwanted:\n    " + args);
        });
      } else {
        var start, end;
        if (directlyHere) {
          for (end = m.index; end && /[\s:,;]/.test(text.charAt(end - 1)); --end) {}
          start = null;
        } else {
          var expr = walk.findNodeBefore(ast, m.index, "Expression");
          if (!expr) {
            fail(m, "No expresion found");
            return;
          }
          start = expr.node.start; end = expr.node.end;
        }
        var query = {type: kind == "doc:" ? "documentation" : kind == "loc:" ? "definition" : kind == "refs:" ? "refs" : "type",
                     start: start, end: end,
                     file: fname,
                     depth: kind == "::" ? 5 : null,
                     lineCharPositions: true};
        server.request({query: query}, function(err, resp) {
          if (err) throw err;

          if (kind == "doc:") { // Docstring test
            if (resp.doc != args) {
              fail(m, "Found " + (resp.doc ? "docstring\n  " + resp.doc + "\n" : "no docstring ") +
                   "instead of expected docstring\n  " + args);
            }
          } else if (kind == "loc:") { // Definition finding test
            var pos = args.match(/^\s*(\d+),\s*(\d+)/), line = Number(pos[1]), col = Number(pos[2]);
            if (!resp.start)
              fail(m, "Did not find a definition.");
            else if (resp.start.line + 1 != line || resp.start.ch != col)
              fail(m, "Found definition at " + (resp.start.line + 1) + ":" + resp.start.ch +
                   " instead of expected " + line + ":" + col);
          } else if (kind == "refs:") { // Reference finding test
            var pos = /\s*(\d+),\s*(\d+)/g, mm;
            while (mm = pos.exec(args)) {
              var line = Number(mm[1]), col = Number(mm[2]), found = false;
              for (var i = 0; i < resp.refs.length; ++i) {
                var ref = resp.refs[i];
                if (ref.start.line + 1 == line && ref.start.ch == col) { found = true; break; }
              }
              if (!found) fail(m, "Did not find reference at line " + line + ", column " + col + ".");
            }
          } else { // Type test
            var type = resp.guess && kind != ":?" ? "?" : resp.type || "?";
            if (type != args)
              fail(m, "Expression has type\n  " + type + "\ninstead of expected type\n  " + args);
          }
        });
      }
    })(m);
  });
};
