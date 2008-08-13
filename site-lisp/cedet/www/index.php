<!-- -*- html -*- -->
<TITLE>Collection of Emacs Development Environment Tools Homepage</TITLE>

<?php
  include ("pagestart.php")
?>

<P>Welcome to the CEDET homepage.  CEDET is a collection of tools
   written with the end goal of creating an advanced development
   environment in Emacs.  CEDET is hosted at
   <a href=http://www.sourceforge.net>Source Forge</a>.  You can view
   CEDET's CVS archive, and project summary
   <a href=http://www.sourceforge.net/projects/cedet>here.</a>
</p>

<P>Emacs already is a great environment for writing software, but
   there are additional areas that need improvement.  Many new ideas
   for integrated environments have been developed in newer products,
   such as Microsoft's Visual environment, JBuilder, Eclipse, or
   KDevelop.  CEDET is a project which brings together several
   different tools needed to implement advanced features.
</p>


<P>Please visit individual project pages for additional information and
   downloads.
</p>

<table width=100% class=BAR><tr><td>
<H3>Articles</h3>
</td></tr></table>

<P>An
   <a href=http://www-106.ibm.com/developerworks/library/j-emacs/?n-j-5241>
   article</a>
   about the
   <a href=http://jde.sunsite.dk/>JDE</a>
   includes some pointers to CEDET, and mentions some of these tools.</p>

<table width=100% class=BAR><tr><td>
<H3>Base Tools are:</h3>
</td></tr></table>

<P><A HREF="eieio.shtml"> <B>EIEIO: Enhanced Implementation of Emacs
   Interpreted Objects</B></A><br> is a package which implements a
   <B>CLOS</B> subset for Emacs.  It includes examples which can draw
   simple tree graphs, and bar charts.
</p>

<P><A HREF="semantic.shtml"><B>Semantic</B></A><br> is a
   Infrastructure for parser based text analysis in Emacs.  It creates
   parsers in Emacs Lisp.  Includes interfaces to all common tag-like
   features in Emacs.
</P>

<P><A HREF="srecode.shtml"> <B>SRecode: Semantic Recoder</B></A><br>
  is a template authoring and cataloging system.  Includes base
  templates for several languages and sample SRecode applications.
  <em>Currently available from CVS only.</em>
</p>

<P><B>CEDET/common:</B><br>
   The CEDET common subpackage contains small utilities recently drawn
   out of some of the other tools.  Some example tools include:
   <ul>
     <li>working - A busy meter.
     <li>sformat - Souped up format.
     <li>cedet-autogen - Autoload generators extended for eieio and
       semantic.
     <li>inversion - Package level versioning system.
     <li>ezimage - Simple way to declare and display images in all
       versions of Emacs.
     <li>pulse - Fancy overlay color pulsing.
     <li>pprint - Pretty printer for Emacs Lisp values.
   </ul>
</p>

<table width=100% class=BAR><tr><td>
<h3>User Interface Tools are:</h3>
</td></tr></table>

<P><A HREF="speedbar.shtml"><B>Speedbar</B></A><br>
   Speedbar is an Everything Browser. It creates special skinny frame
   to display hierarchical data.  Speedbar supports file/directory
   trees, Info manuals, EIEIO class browsing, project browsing,
   context analysis/autocomplete, EIRC groups, xshtml, and VHDL.  It
   can be rigged to display just about anything.
</P>

<P><A href="ede.shtml"> <B>EDE: Emacs Development
   Environment</b></a><br> EDE is a project management system. It
   implements projects under Emacs making it easy to maintain programs
   without learning make.  This depends on EIEIO.</p>

<P><A href="cogre.shtml"> <b>COGRE: COnnected GRaph Editor</b></a><br>
   COGRE, pronounced like <em>cougar</em>, is an interface to managing connected
   graphs, such as UML class diagrams.</p>  It can display simple
   graphs, and UML class diagrams.  It can generate simple class diagrams
   from sources using Semantic.   
</p>

<?php
  include ("download.php")
?>
</p>

<table width=100% class=BAR><tr><td>
<h3>Tools hosted elsewhere:</h3>
</td></tr></table>

<P><a href=http://ecb.sourceforge.net>
   <b>Emacs Code Browser (ECB)</b></a>
   lets you browse your files' contents.
   Uses the Semantic package.</p>

<table width=100% class=BAR><tr><td>
<h3>Other Miscellaneous Emacs Hacks at this web site:</h3>
</td></tr></table>

<P><A HREF="checkdoc.shtml"><B>Checkdoc</B></A> is a program which
   checks the style of your documentation strings and comments.
   Useful if you want to keep other Emacs Lisp gurus from picking on
   you.
</P>

<P><A HREF="cparse.shtml"><B>C-Parse</B></A> is an <B>Emacs Lisp</B>
   program which can parse a c file, and allows searching for
   functions, variables, and types.  CParse is no longer supported.
   It's tools will be ported to Semantic.
</P>

<P><A HREF="ftp/X-0.3a.tar.gz"><b>X11 lib calls for Emacs, V
   0.3a</b></A> Imagine the binary network interface for X windows
   implemented in Emacs Lisp.  Is it useful?  Silly?  I dunno, but it
   was fun to play with.  No documentation.  Byte compile it, load
   "xhello.el" and run the function `XX' for the simple demo.<br>
</p>
<P>
   Newer versions of this X package are in CVS, and can be downloaded
   from the very nifty XWEM project (Window manager written in Emacs Lisp).
   You can download that from 
   <a href="http://www.xwem.org">XWEM home page.</a>
</p>

<P><A HREF="ftp/hangman.el-0.1.gz"><b>Hangman</b></A> game for
   Emacs.  About as simple as it gets.
</p>

<?php
  include ("footer.fsf.shtml")
?>
