var useWorker = false;

var server, editor, defs = [];
var Pos = CodeMirror.Pos;

// Document management

var docs = [], curDoc;

function findDoc(name) { return docs[docID(name)]; }
function docID(name) { for (var i = 0; i < docs.length; ++i) if (docs[i].name == name) return i; }

function registerDoc(name, doc) {
  server.addDoc(name, doc);
  var data = {name: name, doc: doc};
  docs.push(data);
  var docTabs = document.getElementById("docs");
  var li = docTabs.appendChild(document.createElement("li"));
  li.appendChild(document.createTextNode(name));
  if (editor.getDoc() == doc) {
    setSelectedDoc(docs.length - 1);
    curDoc = data;
  }
}

function unregisterDoc(doc) {
  server.delDoc(doc.name);
  for (var i = 0; i < docs.length && doc != docs[i]; ++i) {}
  docs.splice(i, 1);
  var docList = document.getElementById("docs");
  docList.removeChild(docList.childNodes[i]);
  selectDoc(Math.max(0, i - 1));
}

function setSelectedDoc(pos) {
  var docTabs = document.getElementById("docs");
  for (var i = 0; i < docTabs.childNodes.length; ++i)
    docTabs.childNodes[i].className = pos == i ? "selected" : "";
}

function selectDoc(pos) {
  server.hideDoc(curDoc.name);
  setSelectedDoc(pos);
  curDoc = docs[pos];
  editor.swapDoc(curDoc.doc);
}

// Initialization

function load(file, c) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, true);
  xhr.send();
  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) c(xhr.responseText, xhr.status);
  };
}

CodeMirror.on(window, "load", function() {
  var files = ["../defs/ecma5.json", "../defs/browser.json", "../defs/jquery.json"];
  var loaded = 0;
  for (var i = 0; i < files.length; ++i) (function(i) {
    load(files[i], function(json) {
      defs[i] = JSON.parse(json);
      if (++loaded == files.length) initEditor();
    });
  })(i);

  var cmds = document.getElementById("commands");
  CodeMirror.on(cmds, "change", function() {
    if (!editor || cmds.selectedIndex == 0) return;
    var found = commands[cmds.value];
    cmds.selectedIndex = 0;
    editor.focus();
    if (found) found(editor);
  });
});

function initEditor() {
  var keyMap = {
    "Ctrl-I": function(cm) { server.showType(cm); },
    "Ctrl-Space": function(cm) { server.complete(cm); },
    "Alt-.": function(cm) { server.jumpToDef(cm); },
    "Alt-,": function(cm) { server.jumpBack(cm); },
    "Ctrl-Q": function(cm) { server.renamce(cm); }
  };

  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: keyMap,
    matchBrackets: true
  });

  server = new CodeMirror.TernServer({
    defs: defs,
    plugins: {requirejs: {}, doc_comment: true},
    switchToDoc: function(name) { selectDoc(docID(name)); },
    workerDeps: ["../../../acorn/acorn.js", "../../../acorn/acorn_loose.js",
                 "../../../acorn/util/walk.js", "../../../../lib/signal.js", "../../../../lib/tern.js",
                 "../../../../lib/def.js", "../../../../lib/infer.js", "../../../../lib/comment.js",
                 "../../../../plugin/requirejs.js", "../../../../plugin/doc_comment.js"],
    workerScript: "../node_modules/codemirror/addon/tern/worker.js",
    useWorker: useWorker

  });

  editor.on("cursorActivity", function(cm) { server.updateArgHints(cm); });

  registerDoc("test.js", editor.getDoc());
  registerDoc("test_dep.js", new CodeMirror.Doc(document.getElementById("requirejs_test_dep").firstChild.nodeValue, "javascript"));
  load("demo/underscore.js", function(body) {
    registerDoc("underscore.js", new CodeMirror.Doc(body, "javascript"));
  });

  CodeMirror.on(document.getElementById("docs"), "click", function(e) {
    var target = e.target || e.srcElement;
    if (target.nodeName.toLowerCase() != "li") return;
    for (var i = 0, c = target.parentNode.firstChild; ; ++i, (c = c.nextSibling))
      if (c == target) return selectDoc(i);
  });
}

var commands = {
  complete: function(cm) { server.complete(cm); },
  jumptodef: function(cm) { server.jumpToDef(cm); },
  findtype: function(cm) { server.showType(cm); },
  rename: function(cm) { server.rename(cm); },
  addfile: function() {
    var name = prompt("Name of the new buffer", "");
    if (name == null) return;
    if (!name) name = "test";
    var i = 0;
    while (findDoc(name + (i || ""))) ++i;
    registerDoc(name + (i || ""), new CodeMirror.Doc("", "javascript"));
    selectDoc(docs.length - 1);
  },
  delfile: function() {
    if (docs.length == 1) return;
    unregisterDoc(curDoc);
  }
};
