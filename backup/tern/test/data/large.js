var fs = require("fs");
var rmrf = require("rimraf");
var yaml = require("js-yaml");
var marked = require("marked");
var Mold = require("mold-template");
var util = require("./util");
CodeMirror = require("codemirror/addon/runmode/runmode.node.js");

marked.setOptions({highlight: highlightCode, gfm: true});

function highlightCode(code, lang) {
  if (!lang) return code;
  if (!CodeMirror.modes.hasOwnProperty(lang)) {
    try { require("codemirror/mode/" + lang + "/" + lang); }
    catch(e) { console.log(e.toString());CodeMirror.modes[lang] = false; }
  }
  if (CodeMirror.modes[lang]) {
    var html = "";
    CodeMirror.runMode(code, lang, function(token, style) {
      if (style) html += "<span class=\"cm-" + style + "\">" + Mold.escapeHTML(token) + "</span>";
      else html += Mold.escapeHTML(token);
    });
    return html;
  } else return code;
}

function hasFrontMatter(file) {
  var fd = fs.openSync(file, "r");
  var b = new Buffer(4);
  var ret = fs.readSync(fd, b, 0, 4, 0) == 4 && b.toString() == "---\n";
  fs.closeSync(fd);
  return ret;
}

function readFrontMatter(file) {
  if (/^---\n/.test(file)) {
    var end = file.search(/\n---\n/);
    if (end != -1) return {front: yaml.load(file.slice(4, end + 1)) || {}, main: file.slice(end + 5)};
  }
  return {front: {}, main: file};
}

function readPosts(config) {
  var posts = [];
  fs.readdirSync("_posts/").forEach(function(file) {
    var d = file.match(/^(\d{4})-(\d\d?)-(\d\d?)-(.+)\.(md|link)$/);
    var split = readFrontMatter(fs.readFileSync("_posts/" + file, "utf8"));
    var post = split.front;
    post.date = new Date(d[1], d[2] - 1, d[3]);
    post.name = d[4];
    if (!post.tags) post.tags = [];
    if (!post.tags.forEach && post.tags.split) post.tags = post.tags.split(/\s+/);
    if (d[5] == "md") {
      post.content = marked(split.main);
      post.url = getURL(config, post);
    } else if (d[5] == "link") {
      var escd = Mold.escapeHTML(post.url);
      post.content = "<p>Read this post at <a href=\"" + escd + "\">" + escd + "</a>.</p>";
      post.isLink = true;
    }
    posts.push(post);
  });
  posts.sort(function(a, b){return b.date - a.date;});
  return posts;
}

function gatherTags(posts) {
  var tags = {};
  posts.forEach(function(post) {
    if (post.tags) post.tags.forEach(function(tag) {
      (tags.hasOwnProperty(tag) ? tags[tag] : (tags[tag] = [])).push(post);
    });
    else post.tags = [];
  });
  return tags;
}

var defaults = {
  postLink: "${name}.html"
};

function readConfig() {
  var config = (util.exists("_config.yml") && yaml.load(fs.readFileSync("_config.yml", "utf8"))) || {};
  for (var opt in defaults) if (defaults.hasOwnProperty(opt) && !config.hasOwnProperty(opt))
    config[opt] = defaults[opt];
  return config;
}

function getURL(config, post) {
  var link = config.postLink;
  for (var prop in post) link = link.replace("${" + prop + "}", post[prop]);
  return link;
}

function ensureDirectories(path) {
  var parts = path.split("/"), cur = "";
  for (var i = 0; i < parts.length - 1; ++i) {
    cur += parts[i] + "/";
    if (!util.exists(cur, true)) fs.mkdirSync(cur);
  }
}

function prepareIncludes(ctx) {
  if (!util.exists("_includes/", true)) return;
  fs.readdirSync("_includes/").forEach(function(file) {
    Mold.define(file.match(/^(.*?)\.[^\.]+$/)[1],
                Mold.bake(fs.readFileSync("_includes/" + file, "utf8"), ctx));
  });
}

var layouts = {};
function getLayout(name, ctx) {
  if (name.indexOf(".") == -1) name = name + ".html";
  if (layouts.hasOwnProperty(name)) return layouts[name];
  var tmpl = Mold.bake(fs.readFileSync("_layouts/" + name, "utf8"), ctx);
  tmpl.filename = name;
  layouts[name] = tmpl;
  return tmpl;
}

function generate() {
  var config = readConfig(), posts = readPosts(config);
  var ctx = {site: {posts: posts, tags: gatherTags(posts), config: config},
             dateFormat: require("dateformat")};
  prepareIncludes(ctx);
  if (util.exists("_site", true)) rmrf.sync("_site");
  posts.forEach(function(post) {
    if (post.isLink) return;
    var path = "_site/" + post.url;
    ensureDirectories(path);
    fs.writeFileSync(path, getLayout(post.layout || "post.html", ctx)(post), "utf8");
  });
  function walkDir(dir) {
    fs.readdirSync(dir).forEach(function(fname) {
      if (/^[_\.]/.test(fname)) return;
      var file = dir + fname;
      if (fs.statSync(file).isDirectory()) {
        walkDir(file + "/");
      } else {
        var out = "_site/" + file;
        ensureDirectories(out);
        if (/\.md$/.test(fname) && hasFrontMatter(file)) {
          var split = readFrontMatter(fs.readFileSync(file, "utf8"));
          var doc = split.front;
          var layout = getLayout(doc.layout || "default.html", ctx);
          doc.content = marked(split.main);
          doc.name = fname.match(/^(.*?)\.[^\.]+$/)[1];
          doc.url = file;
          out = out.replace(/\.md$/, layout.filename.match(/(\.\w+|)$/)[1]);
          fs.writeFileSync(out, layout(doc), "utf8");
        } else {
          util.copyFileSync(file, out);
        }
      }
    });
  }
  walkDir("./");
}

generate();

// Long line
function highlightCode2(code, lang) { if (!lang) return code; if (!CodeMirror.modes.hasOwnProperty(lang)) { try { require("codemirror/mode/" + lang + "/" + lang); } catch(e) { console.log(e.toString());CodeMirror.modes[lang] = false; } } if (CodeMirror.modes[lang]) { var html = ""; CodeMirror.runMode(code, lang, function(token, style) { if (style) html += "<span class=\"cm-" + style + "\">" + Mold.escapeHTML(token) + "</span>"; else html += Mold.escapeHTML(token); }); return html; } else return code; } function hasFrontMatter2(file) { var fd = fs.openSync(file, "r"); var b = new Buffer(4); var ret = fs.readSync(fd, b, 0, 4, 0) == 4 && b.toString() == "---\n"; fs.closeSync(fd); return ret; } function readFrontMatter2(file) { if (/^---\n/.test(file)) { var end = file.search(/\n---\n/); if (end != -1) return {front: yaml.load(file.slice(4, end + 1)) || {}, main: file.slice(end + 5)}; } return {front: {}, main: file}; } function readPosts2(config) { var posts = []; fs.readdirSync("_posts/").forEach(function(file) { var d = file.match(/^(\d{4})-(\d\d?)-(\d\d?)-(.+)\.(md|link)$/); var split = readFrontMatter(fs.readFileSync("_posts/" + file, "utf8")); var post = split.front; post.date = new Date(d[1], d[2] - 1, d[3]); post.name = d[4]; if (!post.tags) post.tags = []; if (!post.tags.forEach && post.tags.split) post.tags = post.tags.split(/\s+/); if (d[5] == "md") { post.content = marked(split.main); post.url = getURL(config, post); } else if (d[5] == "link") { var escd = Mold.escapeHTML(post.url); post.content = "<p>Read this post at <a href=\"" + escd + "\">" + escd + "</a>.</p>"; post.isLink = true; } posts.push(post); }); posts.sort(function(a, b){return b.date - a.date;}); return posts; }
