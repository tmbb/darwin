from markdownify import markdownify
import requests
import re
import textwrap

with open('absform.html', 'r') as f:
    html = f.read()
replaced_html = re.sub(
    r'<span class="code">([^<]+)</span>',
    r"`\g<1>`",
    html
)
print(replaced_html)
with open('absform-2.html', 'w') as f:
    f.write(replaced_html)
with open('absform.md', 'w') as f:
    md = markdownify(replaced_html)
    f.write(textwrap.dedent(md))
