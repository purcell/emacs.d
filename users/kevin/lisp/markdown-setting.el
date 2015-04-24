;; config markdown mode, for supporting local css file and Chinese
;; there is no way to generate css code into html file for now, will solve this later.
;; TODO: copy css code into generated html
;;(setq markdown-css-path "../markdown-css/clearness.css")
(setq markdown-content-type "text/html")
(setq markdown-coding-system nil)
(setq buffer-file-coding-system 'utf-8)

;; add css for markdown-mode export to html files
                                        ; Use org.css from the norang website for export document stylesheets
;;(setq markdown-css-dir "/home/kevin/.emacs.d/users/kevin/markdown-css/")
;;(setq markdown-css-theme "clearness")
(defconst markdown-css-style-solarized-light "<style type=\"text/css\">
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */.solarized-light {
  padding: 20px;
  color: #737373;
  font-size: 0.9em;
  font-family: \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNeo-Medium, Verdana, Tahoma, sans-serif;
  background: #fdf6e3;
  -webkit-font-smoothing: antialiased;
}
.solarized-light a {
  color: #1e6ea7;
}
.solarized-light a:hover {
  color: #268bd2;
}
.solarized-light h1,
.solarized-light h2,
.solarized-light h3,
.solarized-light h4,
.solarized-light h5 {
  color: #b58900;
}
.solarized-light h2 {
  border-bottom: 1px solid #f6d784;
}
.solarized-light h6 {
  color: #9c7600;
}
.solarized-light hr {
  border: 1px solid #fae7b3;
}
.solarized-light pre > code {
  font-size: .9em;
  font-family: Consolas, Inconsolata, Courier, monospace;
}
.solarized-light blockquote {
  border-left: 4px solid #fae7b3;
  padding: 0 15px;
  font-style: italic;
}
.solarized-light table {
  background-color: #fdf4dd;
}
.solarized-light table tr th,
.solarized-light table tr td {
  border: 1px solid #fae7b3;
}
.solarized-light table tr:nth-child(2n) {
  background-color: #fef8ea;
}
/**
 * after less
 */

 </style>")


(defconst markdown-css-style-github-rhio "<style type=\"text/css\">
@import url(http://fonts.googleapis.com/css?family=Tauri);
@import url(http://fonts.googleapis.com/css?family=Roboto+Condensed:300italic,400italic,700italic,400,300,700);
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.github-rhio {
  padding: 20px;
  color: #222222;
  font-family: 'Roboto Condensed', 'Tauri', AppleSDGothicNeo-Medium, Sans-serif;
  background: #ffffff;
  -webkit-font-smoothing: antialiased;
}
.github-rhio a {
  color: #3269a0;
}
.github-rhio a:hover {
  color: #4183c4;
}
.github-rhio h2 {
  border-bottom: 1px solid #e6e6e6;
}
.github-rhio h6 {
  color: #777;
}
.github-rhio hr {
  border: 1px solid #e6e6e6;
}
.github-rhio pre > code {
  font-size: .9em;
  font-family: Consolas, Inconsolata, Courier, monospace;
}
.github-rhio blockquote {
  border-left: 4px solid #e6e6e6;
  padding: 0 15px;
  font-style: italic;
}
.github-rhio table {
  background-color: #fafafa;
}
.github-rhio table tr th,
.github-rhio table tr td {
  border: 1px solid #e6e6e6;
}
.github-rhio table tr:nth-child(2n) {
  background-color: #f2f2f2;
}
/**
 * after less
 */

 </style>")

(defconst markdown-css-style-haroopad "<style type=\"text/css\">
@import url(http://fonts.googleapis.com/css?family=Tauri);

@import url(http://fonts.googleapis.com/css?family=Roboto+Condensed:300italic,400italic,700italic,400,300,700);
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.haroopad {
  padding: 20px;
  color: #222222;
  font-size: 0.9em;
  font-family: 'Roboto Condensed', 'Tauri', \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNeo-Medium, Verdana, Tahoma, sans-serif;
  background: #ffffff;
  -webkit-font-smoothing: antialiased;
}
.haroopad a {
  color: #3269a0;
}
.haroopad a:hover {
  color: #4183c4;
}
.haroopad h2 {
  border-bottom: 1px solid #e6e6e6;
}
.haroopad h6 {
  color: #777;
}
.haroopad hr {
  border: 1px solid #e6e6e6;
}
.haroopad p > code {
  font-family: Consolas, Inconsolata, Courier, monospace;
  color: #BD006A;
}
.haroopad pre > code {
  font-size: 1em;
  font-family: Consolas, Inconsolata, Courier, monospace;
  letter-spacing: -1px;
  font-weight: bold;
}
.haroopad blockquote {
  border-left: 4px solid #e6e6e6;
  padding: 0 15px;
  font-style: italic;
}
.haroopad table {
  background-color: #fafafa;
}
.haroopad table tr th,
.haroopad table tr td {
  border: 1px solid #e6e6e6;
}
.haroopad table tr:nth-child(2n) {
  background-color: #f2f2f2;
}
/**
 * after less
 */
/*
.haroopad {

  img {
    .bordered(@border-color, @border-color, @border-color, @border-color);
    .rounded(3px);
    .box-shadow(0 0 7px darken(@border-color, 18%));

    &:hover {
        -webkit-animation-duration: 1s;
        -webkit-animation-delay: .2s;
        .pulse;
    }
  }

  h1 {
    -webkit-animation-duration: .5s;
    -webkit-animation-delay: .2s;
    .tada;
  }

  &>ul {
    &>li {
        -webkit-animation-duration: .5s;
        -webkit-animation-delay: .2s;
        .fadeInLeft;
      }
  }

}
*/
 </style>")

(defconst markdown-css-style-storm "<style type=\"text/css\">
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://unixlab.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.storm {
  color: #000000;
  font-size: 17px;
  font-family: Arial, Helvetica, FreeSans, \"Liberation Sans\", \"Nimbus Sans L\", sans-serif;
  background: #ffffff;
  -webkit-font-smoothing: antialiased;
  width: 60%;
  margin: 0 auto; /* centering text */
  padding-bottom: 6em;
}
.storm a {
  color: #3269a0;
}
.storm a:hover {
  color: #4183c4;
}
.storm h2 {
  border-bottom: 1px solid #e6e6e6;
}
.storm h6 {
  color: #777;
}
.storm hr {
  border: 1px solid #e6e6e6;
}
.storm code {
    font-size: 0.98rem;
    display: inline-block;
    background-color: transparent;
    color: #657b83;
  }
.storm pre {
    line-height: 1.5;
    margin-bottom: 1em;
    background-color: #f2f1f1;
    padding: 0.313em;
    overflow: auto;
    border: 1px solid #c2c1c1;
    border-radius: 5px;
    }
.storm pre code {font-size: 0.85rem;}
.storm blockquote {
  border-left: 4px solid #e6e6e6;
  padding: 0 15px;
  font-style: italic;
}
.storm table {
  width:100%;
  max-width: 100%;
  border-collapse:collapse;
  margin: 2% 0;
}

.storm th, td {
  text-align:left;
  padding:0.313em 0.625em;
  border:1px solid $border;
}

.storm dt {
  color:#444;
  font-weight:700;
}
.storm dd {margin-left: 1em;}

.storm th {
  color:#444;
}
.storm img {
    display: block;
    margin: 1em auto;
    padding: .3em;
    border: 1px solid #DDD;
    max-width: 100%;
    border-radius: 5px;
  }
/**
 * after less
 */

 </style>")


(defconst markdown-css-style-github "<style type=\"text/css\">
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.github {
  padding: 20px;
  color: #000000;
  font-size: 0.9em;
  font-family: \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNeo-Medium, Verdana, Tahoma, sans-serif;
  background: #ffffff;
  -webkit-font-smoothing: antialiased;
}
.github a {
  color: #3269a0;
}
.github a:hover {
  color: #4183c4;
}
.github h2 {
  border-bottom: 1px solid #e6e6e6;
}
.github h6 {
  color: #777;
}
.github hr {
  border: 1px solid #e6e6e6;
}
.github pre > code {
  font-size: .9em;
  font-family: Consolas, Inconsolata, Courier, monospace;
}
.github blockquote {
  border-left: 4px solid #e6e6e6;
  padding: 0 15px;
  font-style: italic;
}
.github table {
  background-color: #fafafa;
}
.github table tr th,
.github table tr td {
  border: 1px solid #e6e6e6;
}
.github table tr:nth-child(2n) {
  background-color: #f2f2f2;
}
/**
 * after less
 */

 </style>")

(defconst markdown-css-style-markdown "<style type=\"text/css\">
/**
 * standard markdown style
 */
.markdown {
  padding: 20px;
  font-size: 0.9em;
}
.markdown a {
  text-decoration: none;
  vertical-align: baseline;
}
.markdown a:hover {
  text-decoration: underline;
}
.markdown h1 {
  font-size: 2.2em;
  font-weight: bold;
  margin: 1.5em 0 1em 0;
}
.markdown h2 {
  font-size: 1.8em;
  font-weight: bold;
  margin: 1.275em 0 0.85em 0;
}
.markdown h3 {
  font-size: 1.6em;
  font-weight: bold;
  margin: 1.125em 0 0.75em 0;
}
.markdown h4 {
  font-size: 1.4em;
  font-weight: bold;
  margin: 0.99em 0 0.66em 0;
}
.markdown h5 {
  font-size: 1.2em;
  font-weight: bold;
  margin: 0.855em 0 0.57em 0;
}
.markdown h6 {
  font-size: 1em;
  font-weight: bold;
  margin: 0.75em 0 0.5em 0;
}
.markdown h1:first-child,
.markdown h2:first-child,
.markdown h3:first-child,
.markdown h4:first-child,
.markdown h5:first-child,
.markdown h6:first-child {
  margin-top: 0;
}
.markdown h1 + p,
.markdown h2 + p,
.markdown h3 + p,
.markdown h4 + p,
.markdown h5 + p,
.markdown h6 + p {
  margin-top: 0;
}
.markdown hr {
  border: 1px solid #cccccc;
}
.markdown p {
  margin: 1em 0;
  line-height: 1.6em;
}
.markdown ol {
  list-style-type: decimal;
}
.markdown li {
  display: list-item;
  line-height: 1.4em;
}
.markdown blockquote {
  margin: 1em 20px;
}
.markdown blockquote > :first-child {
  margin-top: 0;
}
.markdown blockquote > :last-child {
  margin-bottom: 0;
}
.markdown blockquote cite:before {
  content: '\2014 \00A0';
}
.markdown .code {
  border-radius: 3px;
  word-break: break-all;
  word-wrap: break-word;
}
.markdown pre {
  border-radius: 3px;
  word-break: break-all;
  word-wrap: break-word;
  overflow: auto;
}
.markdown pre code {
  display: block;
}
.markdown pre > code {
  border: 1px solid #cccccc;
  white-space: pre;
  padding: .5em;
  margin: 0;
}
.markdown code {
  border-radius: 3px;
  word-break: break-all;
  word-wrap: break-word;
  border: 1px solid #cccccc;
  padding: 0 5px;
  margin: 0 2px;
}
.markdown img {
  max-width: 100%;
}
.markdown table {
  padding: 0;
  border-collapse: collapse;
  border-spacing: 0;
}
.markdown table tr th,
.markdown table tr td {
  border: 1px solid #cccccc;
  margin: 0;
  padding: 6px 13px;
}
.markdown table tr th {
  font-weight: bold;
}
.markdown table tr th > :first-child {
  margin-top: 0;
}
.markdown table tr th > :last-child {
  margin-bottom: 0;
}
.markdown table tr td > :first-child {
  margin-top: 0;
}
.markdown table tr td > :last-child {
  margin-bottom: 0;
}

 </style>")


(defvar markdown-css-style-solarized-dark "<style type=\"text/css\">
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-dark style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */.solarized-dark {
  padding: 20px;
  color: #839496;
  font-size: 0.9em;
  font-family: \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNeo-Medium, Verdana, Tahoma, sans-serif;
  background: #002b36;
  -webkit-font-smoothing: antialiased;
}
.solarized-dark a {
  color: #1e6ea7;
}
.solarized-dark a:hover {
  color: #268bd2;
}
.solarized-dark h1,
.solarized-dark h2,
.solarized-dark h3,
.solarized-dark h4,
.solarized-dark h5 {
  color: #b58900;
}
.solarized-dark h2 {
  border-bottom: 1px solid #005469;
}
.solarized-dark h6 {
  color: #694f00;
}
.solarized-dark hr {
  border: 1px solid #001f27;
}
.solarized-dark pre > code {
  font-size: .9em;
  font-family: Consolas, Inconsolata, Courier, monospace;
}
.solarized-dark blockquote {
  border-left: 4px solid #000203;
  padding: 0 15px;
  font-style: italic;
}
.solarized-dark table {
  background-color: #003441;
}
.solarized-dark table tr th,
.solarized-dark table tr td {
  border: 1px solid #005065;
}
.solarized-dark table tr:nth-child(2n) {
  background-color: #003b4b;
}
/**
 * after less
 */
</style>")

(defvar markdown-css-style-clearness "<style type=\"text/css\">
/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.clearness {
  padding: 20px;
  color: #737373;
  font-size: 0.9em;
  font-family: \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNeo-Medium, Verdana, Tahoma, sans-serif;
  background: #ffffff;
  -webkit-font-smoothing: antialiased;
}
.clearness a {
  color: #1e6ea7;
}
.clearness a:hover {
  color: #268bd2;
}
.clearness h1,
.clearness h2,
.clearness h3,
.clearness h4,
.clearness h5 {
  color: #404040;
}
.clearness h2 {
  border-bottom: 1px solid #cccccc;
}
.clearness h6 {
  color: #666666;
}
.clearness hr {
  border: 1px solid #e6e6e6;
}
.clearness pre > code {
  font-size: .9em;
  font-family: Consolas, Inconsolata, Courier, monospace;
}
.clearness blockquote {
  padding: 0 15px;
  font-style: italic;
}
.clearness blockquote:before {
  content: \"\201C\";
  font-size: 40px;
  margin-left: -20px;
  color: #aaa;
}
.clearness table {
  background-color: #ffffff;
  border-collapse: separate;
  border-spacing: 2px;
}
.clearness table tr th,
.clearness table tr td {
  border: 0px;
  padding: .2em 1em;
}
.clearness table tr th {
  border-bottom: 1px solid #bfbfbf;
}
.clearness table tr td {
  border-bottom: 1px solid #d9d9d9;
}
.clearness table tr:nth-child(2n) {
  background-color: #ffffff;
}
/**
 * after less
 */
</style>")

(defvar markdown-css-style-clearness-dark "<style type=\"text/css\">

/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-light style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.clearness-dark {
  padding: 20px;
  color: #ffffff;
  font-size: 0.9em;
  font-family: \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNeo-Medium, Verdana, Tahoma, sans-serif;
  background: #282a36;
  -webkit-font-smoothing: antialiased;
}
.clearness-dark a {
  color: #e03300;
}
.clearness-dark a:hover {
  color: #ff4a14;
}
.clearness-dark h2 {
  border-bottom: 1px solid #21232d;
}
.clearness-dark h6 {
  color: #a4a296;
}
.clearness-dark hr {
  border: 1px solid #21232d;
}
.clearness-dark pre > code {
  font-size: .9em;
  font-family: Consolas, Inconsolata, Courier, monospace;
}
.clearness-dark blockquote {
  border-left: 4px solid #121319;
  padding: 0 15px;
  font-style: italic;
}
.clearness-dark table {
  background-color: #303241;
}
.clearness-dark table tr th,
.clearness-dark table tr td {
  border: 1px solid #4b4e65;
}
.clearness-dark table tr:nth-child(2n) {
  background-color: #373a4b;
}
/**
 * after less
 */
</style>")

(defvar markdown-css-style-node-dark "<style type=\"text/css\">

/*---------------------------------------------------
    LESS Elements 0.9
  ---------------------------------------------------
    A set of useful LESS mixins
    More info at: http://lesselements.com
  ---------------------------------------------------*/
/**
 * https://github.com/rhiokim/markdown-css
 * solarized-dark style
 * made by rhio.kim
 * powered by http://ethanschoonover.com/solarized
 */
.node-dark {
  padding: 20px;
  color: #d2d8ba;
  font-size: 0.9em;
  font-family: \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", AppleSDGothicNfeo-Medium, Verdana, Tahoma, sans-serif;
  background: #33342d;
  -webkit-font-smoothing: antialiased;
}
.node-dark a {
  color: #639400;
}
.node-dark a:hover {
  color: #85c700;
}
.node-dark h1,
.node-dark h2,
.node-dark h3,
.node-dark h4,
.node-dark h5 {
  color: #eee;
}
.node-dark h2 {
  border-bottom: 1px solid #4e4f45;
}
.node-dark h6 {
  color: #694f00;
}
.node-dark hr {
  border: 1px solid #2b2c26;
}
.node-dark pre > code {
  font-size: 1em;
  font-family: Consolas, Inconsolata, Courier, monospace;
  font-weight: bold;
}
.node-dark blockquote {
  border-left: 4px solid #181915;
  padding: 0 15px;
  font-style: italic;
}
.node-dark table {
  background-color: #3d3e36;
}
.node-dark table tr th,
.node-dark table tr td {
  border: 1px solid #5f6154;
}
.node-dark table tr:nth-child(2n) {
  background-color: #46483e;
}
/**
 * after less
 */
</style>")





(defvar markdown-element-js "<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>
")



(setq markdown-css-theme "solarized-dark")
(setq markdown-xhtml-header-content markdown-css-style-solarized-dark )



(provide 'markdown-setting)
;;; markdown-setting.el ends here

