# -*- coding: utf-8 -*-
#
# Yang documentation build configuration file, created by
# sphinx-quickstart on Wed Oct  8 16:14:00 2014.
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

import sys
import os

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#sys.path.insert(0, os.path.abspath('.'))

# -- General configuration ------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.coverage',
    'sphinx.ext.pngmath',
    'sphinx.ext.ifconfig',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix of source filenames.
source_suffix = '.rst'

# The encoding of source files.
#source_encoding = 'utf-8-sig'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = u'Yang'
copyright = u'2014, Stu Taylor'

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
#
# The short X.Y version.
version = '0.0'
# The full version, including alpha/beta/rc tags.
release = version

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#language = None

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
#today = ''
# Else, today_fmt is used as the format for a strftime call.
#today_fmt = '%B %d, %Y'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = []

# The reST default role (used for this markup: `text`) to use for all
# documents.
#default_role = None

# If true, '()' will be appended to :func: etc. cross-reference text.
#add_function_parentheses = True

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
#add_module_names = True

# If true, sectionauthor and moduleauthor directives will be shown in the
# output. They are ignored by default.
#show_authors = False

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'fruity'

# A list of ignored prefixes for module index sorting.
#modindex_common_prefix = []

# If true, keep warnings as "system message" paragraphs in the built documents.
#keep_warnings = False


# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'yang'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#html_theme_options = {}

# Add any paths that contain custom themes here, relative to this directory.
html_theme_path = ['.']

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
#html_title = None

# A shorter title for the navigation bar.  Default is the same as html_title.
#html_short_title = None

# The name of an image file (relative to this directory) to place at the top
# of the sidebar.
#html_logo = None

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
#html_favicon = None

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Add any extra paths that contain custom files (such as robots.txt or
# .htaccess) here, relative to this directory. These files are copied
# directly to the root of the documentation.
#html_extra_path = []

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
#html_last_updated_fmt = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
#html_use_smartypants = True

# Custom sidebar templates, maps document names to template names.
#html_sidebars = {}

# Additional templates that should be rendered to pages, maps page names to
# template names.
#html_additional_pages = {}

# If false, no module index is generated.
#html_domain_indices = True

# If false, no index is generated.
#html_use_index = True

# If true, the index is split into individual pages for each letter.
#html_split_index = False

# If true, links to the reST sources are added to the pages.
#html_show_sourcelink = True

# If true, "Created using Sphinx" is shown in the HTML footer. Default is True.
#html_show_sphinx = True

# If true, "(C) Copyright ..." is shown in the HTML footer. Default is True.
#html_show_copyright = True

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
#html_use_opensearch = ''

# This is the file name suffix for HTML files (e.g. ".xhtml").
#html_file_suffix = None

# Output file base name for HTML help builder.
htmlhelp_basename = 'Yangdoc'


# -- Options for LaTeX output ---------------------------------------------

latex_elements = {
# The paper size ('letterpaper' or 'a4paper').
#'papersize': 'letterpaper',

# The font size ('10pt', '11pt' or '12pt').
#'pointsize': '10pt',

# Additional stuff for the LaTeX preamble.
#'preamble': '',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
  ('index', 'Yang.tex', u'Yang Documentation',
   u'Stu Taylor', 'manual'),
]

# The name of an image file (relative to this directory) to place at the top of
# the title page.
#latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
#latex_use_parts = False

# If true, show page references after internal links.
#latex_show_pagerefs = False

# If true, show URL addresses after external links.
#latex_show_urls = False

# Documents to append as an appendix to all manuals.
#latex_appendices = []

# If false, no module index is generated.
#latex_domain_indices = True


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'yang', u'Yang Documentation',
     [u'Stu Taylor'], 1)
]

# If true, show URL addresses after external links.
#man_show_urls = False


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  ('index', 'Yang', u'Yang Documentation',
   u'Stu Taylor', 'Yang', 'One line description of project.',
   'Miscellaneous'),
]

# Documents to append as an appendix to all manuals.
#texinfo_appendices = []

# If false, no module index is generated.
#texinfo_domain_indices = True

# How to display URL addresses: 'footnote', 'no', or 'inline'.
#texinfo_show_urls = 'footnote'

# If true, do not generate a @detailmenu in the "Top" node's menu.
#texinfo_no_detailmenu = False


# -- Yang custom domain ---------------------------------------------------

import re
from docutils import nodes
from sphinx import addnodes
from sphinx import directives
from sphinx import domains
from sphinx.util import compat

OPERATOR = r'operator *(\(\)|[^(]+)'
IDENTIFIER_RE = re.compile(OPERATOR + r'|(\w+::)*\w+')
AFTER_FUNCTION_RE = re.compile(r' *\(')

def make_source_literal(env, text):
  literal = nodes.literal_block()

  last_index = 0
  match = IDENTIFIER_RE.search(text, last_index)
  while match:
    literal += nodes.Text(text[last_index:match.start()])
    last_index = match.end()

    link_text = text[match.start():last_index]
    target = link_text
    if ('yang:class' in env.ref_context and
        AFTER_FUNCTION_RE.match(text[last_index:])):
      target = env.ref_context['yang:class'] + '::' + link_text

    xref = addnodes.pending_xref(
        refdomain='yang', reftype='yang', reftarget=target)
    xref += nodes.Text(link_text)
    literal += xref

    match = IDENTIFIER_RE.search(text, last_index)

  literal += nodes.Text(text[last_index:])
  return literal

class YangCodeDirective(compat.Directive):
  has_content = True

  def run(self):
    env = self.state.document.settings.env
    self.assert_has_content()
    node = make_source_literal(env, '\n'.join(self.content))
    self.add_name(node)
    return [node]

class YangClassDirective(compat.Directive):
  required_arguments = 1

  def run(self):
    env = self.state.document.settings.env
    self.name = self.arguments[0]
    env.ref_context['yang:class'] = self.name
    name = 'yang::' + self.name

    title = nodes.title()
    title += nodes.literal(text=name)

    section = nodes.section()
    section += title
    section['ids'].append(self.name)

    index = addnodes.index(entries=[])
    index['entries'].append(('single', name, self.name, ''))
    env.domaindata['yang']['objects'][self.name] = env.docname
    return [index, section]

class YangObject(directives.ObjectDescription):
  TEMPLATE_RE = re.compile(r'template\s*<[^>]*>')

  def add_target_and_index(self, name, sig, signode):
    signode['ids'].append(name)
    self.indexnode['entries'].append(('single', 'yang::' + name, name, ''))
    self.env.domaindata['yang']['objects'][name] = self.env.docname

  def handle_signature(self, sig, signode):
    lines = [line for line in sig.split('\\n') if line.strip()]

    # Remove indentation.
    first = True
    for line in lines:
      index = 0
      for char in line:
        if char != ' ':
          break
        index += 1
      if first or index < indent:
        indent = index
      first = False

    text = '\n'.join(line[indent:] for line in lines)
    signode += make_source_literal(self.env, text)

    match = YangObject.TEMPLATE_RE.search(sig)
    if match:
      return self.parse_name(sig[match.end():])
    return self.parse_name(sig)

def function_name(text):
  last_index = 0
  match = IDENTIFIER_RE.search(text, last_index)
  while match:
    last_index = match.end()
    depth = 0
    for char in text[:match.start()]:
      if char == ')':
        depth += 1
      elif char == '(':
        depth -= 1
    if depth == 0 and AFTER_FUNCTION_RE.match(text[last_index:]):
      return match.group()
    match = IDENTIFIER_RE.search(text, last_index)

class YangMemberObject(YangObject):
  def parse_name(self, sig):
    return self.env.ref_context['yang:class'] + '::' + function_name(sig)

class YangFunctionObject(YangObject):
  def parse_name(self, sig):
    return function_name(sig)

class YangDomain(domains.Domain):
  name = 'yang'
  label = 'C++'
  directives = {
    'class': YangClassDirective,
    'function': YangFunctionObject,
    'member': YangMemberObject,
    'code': YangCodeDirective,
  }
  initial_data = {
    'objects': {},
  }

  def resolve_xref_name(self, name, from_docname, builder, cont_node):
    to_docname = self.data['objects'][name]
    node = nodes.reference('', '', internal=True)
    rel_uri = builder.get_relative_uri(from_docname, to_docname)
    node['refuri'] = rel_uri + '#' + name
    node['reftitle'] = name
    node.append(cont_node)
    return node

  def resolve_xref(self, env, from_docname, builder,
                   type, target, node, cont_node):
    if target in self.data['objects']:
      return self.resolve_xref_name(target, from_docname, builder, cont_node)
    return None

  def get_objects(self):
    # TODO: implement for searching?
    return []


# -- Yang autodocumenter --------------------------------------------------

from sphinx.ext import autodoc

class CppDocumenter(autodoc.Documenter):
  objtype = 'cpp'
  titles_allowed = True

  DOC_REGEX = re.compile(r'/\*\*(([^*]|\*[^/])*)\*/')
  DOC_LINE_PREFIX_REGEX = re.compile(r'(\s*\*)')
  END_FUNCTION_RE = re.compile(r';|{}')
  CLASS_RE = re.compile(r'class\s+(\w+)')

  def write(self, text):
    self.add_line(text, '<autocpp>')

  def preprocess(self, text):
    output = []
    for line in text.split('\n'):
      # Strip trailing whitespace.
      t = line.rstrip()
      # Strip the typical Javadoc-style line prefixes.
      match = CppDocumenter.DOC_LINE_PREFIX_REGEX.match(t)
      if match:
        t = t[len(match.group(0)):]
      # Strip first space.
      if len(t) and t[0] == ' ':
        t = t[1:]
      output.append(t)

    # Strip empty lines from beginning and end.
    first = 0
    while first < len(output) and not output[first]:
      first += 1
    last = len(output) - 1
    while last >= 0 and not output[last]:
      last -= 1
    if first > last:
      return None
    return output[first:1 + last]

  def handle(self, lines, rest, preamble, summary, output):
    rest = rest[1 + rest.find('\n'):]

    replaced = [False]
    def substitute(string, keyword, function):
      while string.find(keyword) >= 0:
        string = string.replace(keyword, function() or '', 1)
        replaced[0] = True
      return string

    indent = ['']
    def substitute_syntax(string, keyword, end_re):
      def f():
        # Automatically indent the rest of the documentation text for #function
        # and #member.
        indent[0] += '  '

        match = end_re.search(rest)
        syntax = rest[:match.end()]
        summary.extend(syntax.split('\n'))
        syntax = syntax[:-(match.end() - match.start())].rstrip()
        processed = '\\\\n' + syntax.replace('\n', '\\\\n')
        return '\n\n.. ' + keyword[1:] + ':: ' + processed
      return substitute(string, keyword, f)

    def class_header():
      preamble.append(
          '.. class:: ' + CppDocumenter.CLASS_RE.search(rest).group(1))
      summary.extend(rest[:1 + rest.find('{')].split('\n'))

    for line in lines:
      replaced[0] = False
      line = substitute(line, '#class', class_header)
      line = substitute_syntax(line, '#member', CppDocumenter.END_FUNCTION_RE)
      line = substitute_syntax(line, '#function', CppDocumenter.END_FUNCTION_RE)

      sumline = lambda: summary.append(rest[:rest.find('\n')])
      sumext = lambda: summary.extend(rest[:rest.find('/**')].split('\n'))
      line = substitute(line, '#sumline', sumline)
      line = substitute(line, '#summary', sumext)
      line = substitute(line, '##', lambda: summary.append(''))
      # Only auto-indent if there wasn't a command in it.
      if replaced[0]:
        output.extend(line.split('\n'))
      else:
        output.extend(indent[0] + t for t in line.split('\n'))

  def generate(self, more_content=None, real_modname=None,
               check_module=False, all_members=False):
    with open(self.name) as f:
      text = f.read()

    preamble = []
    summary = []
    output = []

    first = True
    match = CppDocumenter.DOC_REGEX.search(text)
    while match:
      doc = self.preprocess(match.group(1))

      if doc:
        # Add a blank line between each section of doc comments.
        if not first:
          output.append('')
        first = False
        self.handle(doc, text[match.end():], preamble, summary, output)

      match = CppDocumenter.DOC_REGEX.search(text, match.end())

    for line in preamble:
      self.write(line)
    self.write('')
    self.write('.. code::')
    self.write('')
    for line in summary:
      self.write('  ' + line)
    self.write('')
    for line in output:
      self.write(line)


# -- Override ridiculous formatting ---------------------------------------

from docutils import nodes
from sphinx.builders import html as html_builder
from sphinx.writers import html

HTML_ENCODE = {'<': '&lt;', '>': '&gt;', '"': '&quot;'}
def htmlencode(text):
  text = text.replace('&', '&amp;')
  for k, v in HTML_ENCODE.iteritems():
    text = text.replace(k, v)
  return text

def node_string(node, encode):
  """Assemble string from a particular node without formatting."""
  if node.tagname == '#text':
    return htmlencode(str(node)) if encode else str(node)
  return ''.join(node_string(child, encode) for child in node.children)

def node_find_links(node, index):
  """Search node tree for URL references and their positions in the source."""
  links = []
  if node.tagname == 'reference' and 'refuri' in node.attributes:
    url = node.attributes['refuri']
    links = [(node.attributes['refuri'],
              index, index + len(node_string(node, True)))]
  for child in node.children:
    links.extend(node_find_links(child, index))
    index += len(node_string(child, True))
  return links

def combine_links(links, highlighted):
  """Splice the URL references into the Pygments output."""
  for url, start, end in reversed(links):
    index = 0
    raw_index = 0
    # You can't parse HTML with regex, but what the hell. Pygments outputs a
    # very constrained sort of HTML.
    start_index = -1
    tag = False
    for char in highlighted:
      if char == '<':
        tag = True

      if not tag:
        if index == start and start_index < 0:
          start_index = raw_index
        if index == end:
          end_index = raw_index
          break
        index += 1

      raw_index += 1
      if char == '>':
        tag = False

    highlighted = (highlighted[:start_index] +
                   '<a href="%s" class="code-ref">' % url +
                   highlighted[start_index:end_index] +
                   '</a>' +
                   highlighted[end_index:])

  return highlighted

class Html(html.SmartyPantsHTMLTranslator):
  def __init__(self, *args, **kwargs):
    self.desc = False
    html.SmartyPantsHTMLTranslator.__init__(self, *args, **kwargs)

  def visit_desc_signature(self, node):
    self.body.append(self.starttag(node, 'dt'))
    if node.parent['objtype'] != 'describe' and node['ids'] and node['first']:
      self.body.append('<!--[%s]-->' % node['ids'][0])

  def depart_desc_signature(self, node):
    self.body.append('</dt>\n')

  def visit_literal_block(self, node):
    def warner(msg):
      self.builder.warn(msg, (self.builder.current_docname, node.line))
    highlighted = self.highlighter.highlight_block(
        node_string(node, False), self.highlightlang, warn=warner)
    self.body.append(combine_links(node_find_links(node, 0), highlighted))
    raise nodes.SkipNode

  def depart_literal_block(self, node):
    pass


# -- Custom Pygments lexer ------------------------------------------------

from pygments import lexer
from pygments import token
from pygments.lexers import compiled

class YangCppLexer(compiled.CFamilyLexer):
  aliases = ['yang']
  name = 'Yang'

  tokens = {
    # Instead of the default C++ lexer behaviour, treat the whole operator
    # name override as a single token.
    'statements': [
      (OPERATOR, token.Name),
      (r'(asm|catch|const_cast|delete|dynamic_cast|explicit|'
       r'export|friend|mutable|namespace|new|'
       r'private|protected|public|reinterpret_cast|'
       r'restrict|static_cast|template|this|throw|throws|'
       r'typeid|typename|using|virtual)\b', token.Keyword),
      (r'(class)(\s+)', lexer.bygroups(token.Keyword, token.Text), 'classname'),
      (r'(std)(\s*::\s*)'
       r'(string|vector|function|runtime_error|'
       r'set|map|unordered_set|unordered_map)',
       lexer.bygroups(token.Keyword.Type, token.Keyword.Text, token.Keyword.Type)),
      lexer.inherit,
    ],
    # Don't include function lexing, which doesn't work in general and leads to
    # inconsistency.
    'root': [
      lexer.include('whitespace'),
      ('', token.Text, 'statement'),
    ],
    'classname': [
      (r'[a-zA-Z_][a-zA-Z0-9_]*', token.Name.Class, '#pop'),
      (r'\s*(?=>)', token.Text, '#pop'),
    ],
  }


# -- Set up all the extra functionality  ----------------------------------

from pygments.lexers import _mapping

sys.path.append(os.path.dirname(os.path.realpath(__file__)))
primary_domain = 'yang'
highlight_language = 'yang'
html_translator_class = 'conf.Html'

__all__ = ['YangCppLexer']
_mapping.LEXERS['YangCppLexer'] = (
    'conf', YangCppLexer.name, tuple(YangCppLexer.aliases),
    tuple(YangCppLexer.filenames), tuple(YangCppLexer.mimetypes))

import traceback
def setup(app):
  app.add_domain(YangDomain)
  app.add_autodocumenter(CppDocumenter)
