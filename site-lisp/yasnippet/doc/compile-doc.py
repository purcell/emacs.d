#!/usr/bin/python
# Compile document to HTML use docutils.

# ========================================
# Pygments syntax highlighting
# ========================================
from pygments.formatters import HtmlFormatter

# Set to True if you want inline CSS styles instead of classes
INLINESTYLES = True

from pygments.formatters import HtmlFormatter

# The default formatter
DEFAULT = HtmlFormatter(noclasses=INLINESTYLES)

# Add name -> formatter pairs for every variant you want to use
VARIANTS = {
    # 'linenos': HtmlFormatter(noclasses=INLINESTYLES, linenos=True),
}

from docutils import nodes
from docutils.parsers.rst import directives

from pygments import highlight
from pygments.lexers import get_lexer_by_name, TextLexer

def pygments_directive(name, arguments, options, content, lineno,
                       content_offset, block_text, state, state_machine):
    try:
        lexer = get_lexer_by_name(arguments[0])
    except ValueError:
        # no lexer found - use the text one instead of an exception
        lexer = TextLexer()
    # take an arbitrary option if more than one is given
    formatter = options and VARIANTS[options.keys()[0]] or DEFAULT
    parsed = highlight(u'\n'.join(content), lexer, formatter)
    return [nodes.raw('', parsed, format='html')]

pygments_directive.arguments = (1, 0, 1)
pygments_directive.content = 1
pygments_directive.options = dict([(key, directives.flag) for key in VARIANTS])

directives.register_directive('sourcecode', pygments_directive)

# =================
# Youtube embedding
# =================

from docutils import nodes
from docutils.parsers.rst import directives

CODE = """\
<object type="application/x-shockwave-flash"
        width="%(width)s"
        height="%(height)s"
        align="%(align)s"
        class="youtube-embed"
        data="http://www.youtube.com/v/%(yid)s">
    <param name="movie" value="http://www.youtube.com/v/%(yid)s"></param>
    <param name="wmode" value="transparent"></param>%(extra)s
</object>
"""

PARAM = """\n    <param name="%s" value="%s"></param>"""

def youtube(name, args, options, content, lineno,
            contentOffset, blockText, state, stateMachine):
    """ Restructured text extension for inserting youtube embedded videos """
    if len(content) == 0:
        return
    string_vars = {
        'yid': content[0],
        'width': 425,
        'height': 344,
        'align': "right",
        'extra': ''
        }
    extra_args = content[1:] # Because content[0] is ID
    extra_args = [ea.strip().split("=") for ea in extra_args] # key=value
    extra_args = [ea for ea in extra_args if len(ea) == 2] # drop bad lines
    extra_args = dict(extra_args)
    if 'width' in extra_args:
        string_vars['width'] = extra_args.pop('width')
    if 'align' in extra_args:
        string_vars['align'] = extra_args.pop('align')
    if 'height' in extra_args:
        string_vars['height'] = extra_args.pop('height')
    if extra_args:
        params = [PARAM % (key, extra_args[key]) for key in extra_args]
        string_vars['extra'] = "".join(params)
    return [nodes.raw('', CODE % (string_vars), format='html')]
youtube.content = True
directives.register_directive('youtube', youtube)


# ========================================
# Command line processing
# ========================================
from docutils.core import publish_cmdline, default_description

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources.  ' + default_description)
overrides = {'stylesheet_path' : 'styles.css',
             'embed_stylesheet' : False,
             'template' : 'doc/template.txt'}

publish_cmdline(writer_name='html',
                description=description,
                settings_overrides=overrides)
