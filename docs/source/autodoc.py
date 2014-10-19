# -- Yang autodocumenter --------------------------------------------------

import re
import sys

DOC_REGEX = re.compile(r'/\*\*(([^*]|\*[^/])*)\*/')
DOC_LINE_PREFIX_REGEX = re.compile(r'(\s*\*)')
END_FUNCTION_RE = re.compile(r';|{}')
CLASS_RE = re.compile(r'(class|struct)\s+(\w+)')

def preprocess(text):
  output = []
  for line in text.split('\n'):
    # Strip trailing whitespace.
    t = line.rstrip()
    # Strip the typical Javadoc-style line prefixes.
    match = DOC_LINE_PREFIX_REGEX.match(t)
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

def handle(lines, rest, preamble, summary, output):
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
      # Automatically indent the rest of the documentation text for #member.
      if keyword == '#member':
        indent[0] += '  '

      match = end_re.search(rest)
      syntax = rest[:match.end()]
      summary.extend(syntax.split('\n'))
      syntax = syntax[:-(match.end() - match.start())].rstrip()
      processed = '\\\\n%s' % syntax.replace('\n', '\\\\n')
      return '\n\n========\n\n.. %s:: %s' % (keyword[1:], processed)
    return substitute(string, keyword, f)

  def class_header():
    preamble.append('.. class:: ' + CLASS_RE.search(rest).group(2))
    summary.extend(rest[:1 + rest.find('{')].split('\n'))

  to_preamble = [False]
  def preamble_start():
    to_preamble[0] = True

  for line in lines:
    replaced[0] = False
    line = substitute(line, '#class', class_header)
    line = substitute_syntax(line, '#member', END_FUNCTION_RE)
    line = substitute_syntax(line, '#toplevel', END_FUNCTION_RE)

    sumline = lambda: summary.append(rest[:rest.find('\n')])
    sumext = lambda: summary.extend(rest[:rest.find('/**')].split('\n'))
    line = substitute(line, '#preamble', preamble_start)
    line = substitute(line, '#sumline', sumline)
    line = substitute(line, '#summary', sumext)
    line = substitute(line, '##', lambda: summary.append(''))
    # Only auto-indent if there wasn't a command in it.
    if to_preamble[0]:
      preamble.extend(line.split('\n'))
    elif replaced[0]:
      output.extend(line.split('\n'))
    else:
      output.extend(indent[0] + t for t in line.split('\n'))

def generate(source):
  preamble = []
  summary = []
  output = []

  first = True
  match = DOC_REGEX.search(source)
  while match:
    doc = preprocess(match.group(1))

    if doc:
      # Add a blank line between each section of doc comments.
      if not first:
        output.append('')
      first = False
      handle(doc, source[match.end():], preamble, summary, output)

    match = DOC_REGEX.search(source, match.end())

  if not preamble and not summary and not output:
    return ''

  output = (
      preamble + ['', '.. code::', ''] +
      ['  ' + line for line in summary] + [''] + output)
  return '\n'.join(output)
  with open(outfile) as f:
    f.write('\n'.join(output))

if __name__ == '__main__':
  if len(sys.argv) != 3 and len(sys.argv) != 2:
    print 'Usage: %s infile [outfile]' % sys.argv[0]
    sys.exit(1)

  infile = sys.argv[1]
  with open(infile) as f:
    source = f.read()
  output = generate(source)

  if len(sys.argv) == 3:
    outfile = sys.argv[2]
    with open(outfile, 'w') as f:
      f.write(output)
  else:
    print output
