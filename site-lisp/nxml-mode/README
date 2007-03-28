This is a new major mode for GNU Emacs for editing XML documents. It
supports editing well-formed XML documents and also provides
schema-sensitive editing of XML documents using RELAX NG Compact
Syntax.

To use this, you need GNU Emacs version 21.x, preferably 21.3. GNU
Emacs version 20 will not work properly, nor will XEmacs.

To get started, do

  M-x load-file <RET> rng-auto.el <RET>

This defines the necessary autoloads.  Now, visit a file containing
an XML document, and do

  M-x nxml-mode

Now do

  C-h m

for information on how to use nxml-mode.

The beginnings of a manual are in nxml-mode.info.  You can read this
using

  C-u M-x info RET nxml-mode.info RET

It's also installed as an entry at the end of the top-level info
directory.  So you can read it with C-h i as usual.

You can use test.valid.xml and test.invalid.xml as examples of valid
and invalid XML documents.

To get things automatically loaded each time you start Emacs, add

  (load "~/nxml-mode-200YMMDD/rng-auto.el")

to your .emacs, where ~/nxml-mode-200YMMDD is the directory containing
the .elc files.  Note that rng-auto.el does not load all of the
nxml-mode code; it merely sets things up so that all the features of
nxml-mode will be autoloaded properly.  You should not try to autoload
rng-auto.el itself.

To use nxml-mode automatically for files with an extension of xml,
xsl, rng or xhtml, add

  (setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	      auto-mode-alist))

to your .emacs.

If you edit XML using iso-8859-N encodings other than iso-8859-1 and
you are running Emacs 21.3 or later, then I recommend enabling
unify-8859-on-decoding-mode, by adding

  (unify-8859-on-decoding-mode)

to you .emacs.

To get validation and schema-sensitive editing, you need a RELAX NG
Compact Syntax (RNC) schema for you document. The schema directory
includes some schemas for popular document types.

For more on RELAX NG, see

  http://relaxng.org

For a tutorial on RELAX NG Compact Syntax, see

  http://relaxng.org/compact-tutorial.html

For automatically creating RNC schemas, I recommend my Trang program:

  http://www.thaiopensource.com/relaxng/trang.html

You can use this to

- infer an RNC schema from an instance document;

- convert a DTD to an RNC schema;

- convert a RELAX NG XML syntax schema to an RNC schema.

To convert a RELAX NG XML syntax (.rng) schema to a RNC schema, you
can also use the XSLT stylesheet from

  http://www.pantor.com/download.html

To convert a W3C XML Schema to an RNC schema, you need first to
convert it to RELAX NG XML syntax using Sun's RELAX NG converter tool
rngconv (built on top of MSV). See

  https://msv.dev.java.net/

The file NEWS describes recent changes.

Please use the list

  http://groups.yahoo.com/group/emacs-nxml-mode/

for bug reports, discussion. I will announce all new versions there.

James Clark
http://www.jclark.com/contact.html
