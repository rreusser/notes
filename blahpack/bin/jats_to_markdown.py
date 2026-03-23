#!/usr/bin/env python3
"""Convert JATS XML (with MathML) to Markdown with LaTeX math."""

import sys
import re
import xml.etree.ElementTree as ET

MML = '{http://www.w3.org/1998/Math/MathML}'

# Map MathML operator/character entities to LaTeX
OPERATOR_MAP = {
    '\u2207': r'\nabla',
    '\u2208': r'\in',
    '\u2264': r'\leq',
    '\u2265': r'\geq',
    '\u2212': r'-',
    '\u00d7': r'\times',
    '\u2026': r'\ldots',
    '\u2211': r'\sum',
    '\u220f': r'\prod',
    '\u222b': r'\int',
    '\u221e': r'\infty',
    '\u2202': r'\partial',
    '\u2225': r'\|',
    '\u03b1': r'\alpha',
    '\u03b2': r'\beta',
    '\u03b3': r'\gamma',
    '\u03b4': r'\delta',
    '\u03b5': r'\varepsilon',
    '\u03b6': r'\zeta',
    '\u03b7': r'\eta',
    '\u03b8': r'\theta',
    '\u03b9': r'\iota',
    '\u03ba': r'\kappa',
    '\u03bb': r'\lambda',
    '\u03bc': r'\mu',
    '\u03bd': r'\nu',
    '\u03be': r'\xi',
    '\u03c0': r'\pi',
    '\u03c1': r'\rho',
    '\u03c3': r'\sigma',
    '\u03c4': r'\tau',
    '\u03c5': r'\upsilon',
    '\u03c6': r'\varphi',
    '\u03c7': r'\chi',
    '\u03c8': r'\psi',
    '\u03c9': r'\omega',
    '\u0393': r'\Gamma',
    '\u0394': r'\Delta',
    '\u0398': r'\Theta',
    '\u039b': r'\Lambda',
    '\u039e': r'\Xi',
    '\u03a0': r'\Pi',
    '\u03a3': r'\Sigma',
    '\u03a6': r'\Phi',
    '\u03a8': r'\Psi',
    '\u03a9': r'\Omega',
    '\u2217': r'*',
    '\u00a0': r'~',
    '\u2032': r"'",
    '\u2033': r"''",
    '\u23df': r'',  # bottom curly brace (underbrace decoration)
    '\u2260': r'\neq',
    '\u2248': r'\approx',
    '\u2261': r'\equiv',
    '\u00b1': r'\pm',
    '\u2229': r'\cap',
    '\u222a': r'\cup',
    '\u2286': r'\subseteq',
    '\u2282': r'\subset',
}

MATHVARIANT_MAP = {
    'normal': r'\mathrm',
    'bold': r'\mathbf',
    'italic': None,  # default
    'bold-italic': r'\boldsymbol',
    'double-struck': r'\mathbb',
    'script': r'\mathcal',
    'fraktur': r'\mathfrak',
    'monospace': r'\mathtt',
}


def tag(el):
    """Strip namespace from tag."""
    t = el.tag
    if t.startswith(MML):
        return t[len(MML):]
    # Also strip any other namespace
    if '}' in t:
        return t.split('}', 1)[1]
    return t


def text(el):
    """Get direct text content of element."""
    return (el.text or '').strip()


def mml_to_latex(el):
    """Recursively convert a MathML element to LaTeX."""
    if el is None:
        return ''

    t = tag(el)

    if t == 'math':
        # Top-level: process children
        return mml_children(el)

    if t == 'semantics':
        # Usually first child is the presentation MathML
        children = list(el)
        if children:
            return mml_to_latex(children[0])
        return ''

    if t in ('mrow', 'mtd', 'mtr'):
        return mml_children(el)

    if t == 'mtable':
        # For display equations, just render the content
        rows = []
        for child in el:
            if tag(child) == 'mtr':
                rows.append(mml_to_latex(child))
        return r' \\ '.join(rows)

    if t == 'mi':
        txt = text(el)
        variant = el.get('mathvariant', '')
        txt = OPERATOR_MAP.get(txt, txt)
        # Standard math functions that should be \func
        MATH_FUNCS = {
            'log', 'ln', 'exp', 'sin', 'cos', 'tan', 'cot', 'sec', 'csc',
            'sinh', 'cosh', 'tanh', 'coth', 'arcsin', 'arccos', 'arctan',
            'det', 'dim', 'ker', 'hom', 'deg', 'tr', 'diag', 'sgn',
            'arg', 'inf', 'sup', 'lim', 'max', 'min', 'Re', 'Im',
        }
        if txt in MATH_FUNCS and not variant:
            return '\\' + txt
        # Multi-char <mi> without variant that are English words → \text{...}
        TEXT_WORDS = {
            'for', 'when', 'where', 'with', 'and', 'or', 'if', 'then',
            'else', 'on', 'is', 'in', 'at', 'to', 'of', 'by', 'as',
            'such', 'that', 'not', 'all', 'some', 'any',
        }
        if txt.lower() in TEXT_WORDS and not variant and len(txt) > 1:
            return r'\text{' + txt + '}'
        if variant == 'normal' and len(txt) > 1:
            return r'\mathrm{' + txt + '}'
        if variant == 'normal' and len(txt) == 1:
            return r'\mathrm{' + txt + '}'
        if variant == 'bold-italic':
            return r'\boldsymbol{' + txt + '}'
        if variant == 'bold':
            return r'\mathbf{' + txt + '}'
        if variant == 'double-struck':
            return r'\mathbb{' + txt + '}'
        return txt

    if t == 'mn':
        return text(el)

    if t == 'mo':
        txt = text(el)
        # Standard math functions that appear as <mo>
        MO_FUNCS = {
            'log', 'ln', 'exp', 'sin', 'cos', 'tan', 'cot', 'sec', 'csc',
            'sinh', 'cosh', 'tanh', 'coth', 'arcsin', 'arccos', 'arctan',
            'det', 'dim', 'ker', 'hom', 'deg', 'tr', 'diag', 'sgn',
            'arg', 'inf', 'sup', 'lim', 'max', 'min',
        }
        if txt in MO_FUNCS:
            return '\\' + txt
        mapped = OPERATOR_MAP.get(txt, txt)
        # Handle some special operators
        if mapped == r'\|':
            return r'\|'
        if txt in ('(', ')', '[', ']', '{', '}', ',', ';', ':', '.', '!', '+', '-', '=', '<', '>', '/', '|'):
            return txt
        if txt == '*':
            return r'*'
        return mapped

    if t == 'msup':
        children = list(el)
        if len(children) >= 2:
            base = mml_to_latex(children[0])
            sup = mml_to_latex(children[1])
            if len(base) > 1 and not base.startswith('\\') and not base.startswith('{'):
                base = '{' + base + '}'
            if len(sup) > 1:
                return base + '^{' + sup + '}'
            return base + '^' + sup
        return mml_children(el)

    if t == 'msub':
        children = list(el)
        if len(children) >= 2:
            base = mml_to_latex(children[0])
            sub = mml_to_latex(children[1])
            if len(base) > 1 and not base.startswith('\\') and not base.startswith('{'):
                base = '{' + base + '}'
            if len(sub) > 1:
                return base + '_{' + sub + '}'
            return base + '_' + sub
        return mml_children(el)

    if t == 'msubsup':
        children = list(el)
        if len(children) >= 3:
            base = mml_to_latex(children[0])
            sub = mml_to_latex(children[1])
            sup = mml_to_latex(children[2])
            return base + '_{' + sub + '}^{' + sup + '}'
        return mml_children(el)

    if t == 'mfrac':
        children = list(el)
        if len(children) >= 2:
            num = mml_to_latex(children[0])
            den = mml_to_latex(children[1])
            return r'\frac{' + num + '}{' + den + '}'
        return mml_children(el)

    if t == 'msqrt':
        return r'\sqrt{' + mml_children(el) + '}'

    if t == 'mroot':
        children = list(el)
        if len(children) >= 2:
            base = mml_to_latex(children[0])
            idx = mml_to_latex(children[1])
            return r'\sqrt[' + idx + ']{' + base + '}'
        return r'\sqrt{' + mml_children(el) + '}'

    if t == 'mover':
        children = list(el)
        if len(children) >= 2:
            base = mml_to_latex(children[0])
            over = mml_to_latex(children[1])
            # Common overscripts
            if over in ('\u0302', '^', '\u005e'):
                return r'\hat{' + base + '}'
            if over in ('\u0303', '~'):
                return r'\tilde{' + base + '}'
            if over in ('\u0304', '\u00af', '-'):
                return r'\overline{' + base + '}'
            if over == '.':
                return r'\dot{' + base + '}'
            if over == '..':
                return r'\ddot{' + base + '}'
            return r'\overset{' + over + '}{' + base + '}'
        return mml_children(el)

    if t == 'munder':
        children = list(el)
        if len(children) >= 2:
            base_el = children[0]
            under = mml_to_latex(children[1])
            base = mml_to_latex(base_el)
            # Check for underbrace
            if tag(base_el) == 'munder' and base_el.get('accentunder') == 'true':
                inner_children = list(base_el)
                if len(inner_children) >= 2:
                    content = mml_to_latex(inner_children[0])
                    # inner_children[1] is the brace character
                    return r'\underbrace{' + content + '}_{' + under + '}'
            # Check if base is an operator (like sum, max, min)
            if tag(base_el) == 'mo':
                op = text(base_el)
                if op in ('max', 'min', 'sup', 'inf', 'lim'):
                    return '\\' + op + '_{' + under + '}'
                if OPERATOR_MAP.get(op, op) == r'\sum':
                    return r'\sum_{' + under + '}'
                if op == 'arg\u2009min' or 'arg' in op:
                    return r'\arg\min_{' + under + '}'
            return r'\underset{' + under + '}{' + base + '}'
        return mml_children(el)

    if t == 'munderover':
        children = list(el)
        if len(children) >= 3:
            base_el = children[0]
            under = mml_to_latex(children[1])
            over = mml_to_latex(children[2])
            base = mml_to_latex(base_el)
            if tag(base_el) == 'mo':
                op = text(base_el)
                mapped = OPERATOR_MAP.get(op, op)
                if mapped == r'\sum':
                    return r'\sum_{' + under + '}^{' + over + '}'
                if mapped == r'\prod':
                    return r'\prod_{' + under + '}^{' + over + '}'
                if mapped == r'\int':
                    return r'\int_{' + under + '}^{' + over + '}'
            return base + '_{' + under + '}^{' + over + '}'
        return mml_children(el)

    if t == 'mfenced':
        children = list(el)
        open_d = el.get('open', '(')
        close_d = el.get('close', ')')
        separators = el.get('separators', ',')
        # Map fences
        fence_map = {'|': r'\left|', '||': r'\left\|', '{': r'\left\{',
                     '(': r'\left(', '[': r'\left[', '': ''}
        close_map = {'|': r'\right|', '||': r'\right\|', '}': r'\right\}',
                     ')': r'\right)', ']': r'\right]', '': ''}

        parts = [mml_to_latex(c) for c in children]
        sep = separators.strip() if separators is not None else ','
        if sep:
            inner = (f' {sep} ').join(parts)
        else:
            inner = ' '.join(parts)

        o = fence_map.get(open_d, r'\left' + open_d)
        c = close_map.get(close_d, r'\right' + close_d)
        return o + ' ' + inner + ' ' + c

    if t == 'mspace':
        width = el.get('width', '')
        if '2' in width:
            return r'\qquad '
        if '1' in width:
            return r'\quad '
        return r'\, '

    if t == 'mtext':
        txt = text(el)
        if txt:
            return r'\text{' + txt + '}'
        return ''

    if t == 'mphantom':
        return r'\phantom{' + mml_children(el) + '}'

    if t == 'menclose':
        return mml_children(el)

    if t == 'mpadded':
        return mml_children(el)

    if t == 'mstyle':
        return mml_children(el)

    # Fallback: just process children
    return mml_children(el)


def needs_space_between(prev_latex, next_latex, prev_tag, next_tag):
    r"""Check if a space is needed between two adjacent LaTeX tokens.

    Spaces are needed when:
    - A LaTeX command ending in a letter (e.g. \partial, \in) is followed by
      a token starting with a letter — without a space, LaTeX parses them
      as a single (undefined) command.
    - An <mo> that rendered as a multi-char word (e.g. "for", "when") is
      adjacent to a letter token.
    """
    if not prev_latex or not next_latex:
        return False
    # Rule 1: LaTeX command + letter
    if re.search(r'\\[a-zA-Z]+$', prev_latex) and re.match(r'[a-zA-Z]', next_latex):
        return True
    # Rule 2: multi-char word from <mo> or <mi> adjacent to a letter
    # "for", "when", "on", etc. sometimes appear as <mi> or <mo> elements
    prev_is_word = len(prev_latex) > 1 and re.match(r'^[a-zA-Z]+$', prev_latex)
    next_is_word = len(next_latex) > 1 and re.match(r'^[a-zA-Z]+$', next_latex)
    prev_ends_letter = prev_latex and prev_latex[-1:].isalpha()
    next_starts_letter = next_latex and (next_latex[0:1].isalpha() or next_latex[0:1] == '\\')

    if prev_is_word and next_starts_letter:
        return True
    if next_is_word and prev_ends_letter:
        return True
    # Rule 3: \text{...} or \mathrm{...} ending with } followed by a letter
    if prev_latex.endswith('}') and re.match(r'[a-zA-Z]', next_latex):
        # Check if it's a text/mathrm/operatorname command
        if re.search(r'\\(text|mathrm|operatorname)\{[^}]*\}$', prev_latex):
            return True
    return False


def _is_normal_alpha_mi(child):
    """Check if element is a single-char <mi mathvariant="normal"> letter."""
    return (tag(child) == 'mi'
            and child.get('mathvariant') == 'normal'
            and child.text and len(child.text.strip()) == 1
            and child.text.strip().isalpha())


def mml_children(el):
    """Convert all children of a MathML element.

    Merges consecutive single-char <mi mathvariant="normal"> runs into
    \\text{...} (e.g. R-u-n-g-e -> \\text{Runge}).
    Inserts spaces between tokens where LaTeX requires them.
    """
    children = list(el)

    # First pass: build list of (latex_string, tag_name) with merging
    parts = []
    i = 0
    while i < len(children):
        child = children[i]
        if _is_normal_alpha_mi(child):
            # Collect consecutive normal-variant single-char <mi> elements
            run = child.text.strip()
            j = i + 1
            while j < len(children):
                nxt = children[j]
                # Allow <mo> with just whitespace/nbsp between words
                if (tag(nxt) == 'mo' and nxt.text
                        and nxt.text.strip() == ''
                        and j + 1 < len(children)
                        and _is_normal_alpha_mi(children[j + 1])):
                    run += ' ' + children[j + 1].text.strip()
                    j += 2
                elif _is_normal_alpha_mi(nxt):
                    run += nxt.text.strip()
                    j += 1
                else:
                    break
            if len(run) > 1:
                parts.append(('\\text{' + run + '}', 'mi'))
            else:
                # Single char — use normal conversion
                parts.append((mml_to_latex(child), tag(child)))
            i = j
        else:
            parts.append((mml_to_latex(child), tag(child)))
            i += 1

    if not parts:
        return ''

    # Second pass: join with spacing awareness
    result = parts[0][0]
    for i in range(1, len(parts)):
        prev_latex = parts[i - 1][0]
        next_latex = parts[i][0]
        prev_tag = parts[i - 1][1]
        next_tag = parts[i][1]
        if needs_space_between(prev_latex, next_latex, prev_tag, next_tag):
            result += ' ' + next_latex
        else:
            result += next_latex
    return result


def process_inline_math(el):
    """Convert an inline-formula element to $...$."""
    math_el = el.find(f'.//{MML}math')
    if math_el is not None:
        latex = mml_to_latex(math_el)
        return f'${latex}$'
    return ''


def process_display_math(el):
    """Convert a disp-formula element to $$...$$."""
    label_el = el.find('label')
    label = ''
    if label_el is not None:
        label = label_el.text or ''

    math_el = el.find(f'.//{MML}math')
    if math_el is not None:
        latex = mml_to_latex(math_el)
        if label:
            return f'\n\n$${latex} \\tag{{{label}}}$$\n\n'
        return f'\n\n$${latex}$$\n\n'
    return ''


def process_xref(el):
    """Convert xref elements to readable references."""
    ref_type = el.get('ref-type', '')
    txt = el.text or ''
    tail = el.tail or ''

    if ref_type == 'bibr':
        return f'[{txt}]'
    if ref_type == 'disp-formula':
        return f'({txt})'
    if ref_type == 'sec':
        return txt
    if ref_type == 'fig':
        return txt
    if ref_type == 'table':
        return txt
    return txt


def process_element(el, depth=0):
    """Recursively convert a JATS element to markdown."""
    t = tag(el)
    result = ''

    if t == 'article':
        # Process front matter and body
        front = el.find('front')
        body = el.find('body')
        back = el.find('back')

        if front is not None:
            result += process_front(front)
        if body is not None:
            result += process_body(body)
        if back is not None:
            result += process_back(back)
        return result

    return ''


def process_front(front):
    """Process article front matter."""
    result = ''
    meta = front.find('article-meta')
    if meta is None:
        return result

    # Title
    title_group = meta.find('title-group')
    if title_group is not None:
        title = title_group.find('article-title')
        if title is not None:
            result += f'# {title.text or ""}\n\n'

    # Authors
    contrib_group = meta.find('contrib-group')
    if contrib_group is not None:
        authors = []
        for contrib in contrib_group.findall('contrib'):
            name_el = contrib.find('name')
            if name_el is not None:
                given = name_el.find('given-names')
                surname = name_el.find('surname')
                name = ''
                if given is not None and given.text:
                    name += given.text + ' '
                if surname is not None and surname.text:
                    name += surname.text
                authors.append(name.strip())
        if authors:
            result += f'**{", ".join(authors)}**\n\n'

    # Abstract
    abstract = meta.find('abstract')
    if abstract is not None:
        result += '## Abstract\n\n'
        for p in abstract.findall('p'):
            result += process_paragraph(p) + '\n\n'

    result += '---\n\n'
    return result


def process_body(body):
    """Process article body."""
    result = ''
    for child in body:
        if tag(child) == 'sec':
            result += process_section(child, level=2)
    return result


def process_back(back):
    """Process back matter (references)."""
    result = ''
    ref_list = back.find('.//ref-list')
    if ref_list is not None:
        result += '## References\n\n'
        for ref in ref_list.findall('ref'):
            ref_id = ref.get('id', '')
            label = ref.find('label')
            label_text = label.text if label is not None else ''

            # Try to extract citation text
            citation = ref.find('.//mixed-citation') or ref.find('.//element-citation')
            if citation is not None:
                cite_text = extract_text(citation)
                result += f'{label_text}. {cite_text}\n\n'
            elif label_text:
                result += f'{label_text}.\n\n'
    return result


def extract_text(el):
    """Extract all text content from an element, recursively."""
    parts = []
    if el.text:
        parts.append(el.text)
    for child in el:
        parts.append(extract_text(child))
        if child.tail:
            parts.append(child.tail)
    return ''.join(parts)


def process_section(sec, level=2):
    """Process a section element."""
    result = ''

    title = sec.find('title')
    if title is not None:
        prefix = '#' * level
        result += f'{prefix} {title.text or ""}\n\n'

    for child in sec:
        t = tag(child)
        if t == 'p':
            result += process_paragraph(child) + '\n\n'
        elif t == 'sec':
            result += process_section(child, level=level + 1)
        elif t == 'fig':
            result += process_figure(child)
        elif t == 'table-wrap':
            result += process_table(child)
        elif t == 'list':
            result += process_list(child)
        elif t == 'disp-formula':
            result += process_display_math(child)

    return result


def process_paragraph(p):
    """Process a paragraph, handling mixed content."""
    parts = []

    if p.text:
        parts.append(p.text)

    for child in p:
        t = tag(child)

        if t == 'inline-formula':
            parts.append(process_inline_math(child))
        elif t == 'disp-formula':
            parts.append(process_display_math(child))
        elif t == 'xref':
            parts.append(process_xref(child))
        elif t == 'italic':
            txt = extract_text(child)
            parts.append(f'*{txt}*')
        elif t == 'bold':
            txt = extract_text(child)
            parts.append(f'**{txt}**')
        elif t == 'sc':
            txt = extract_text(child)
            parts.append(txt.upper())
        elif t == 'monospace':
            txt = extract_text(child)
            parts.append(f'`{txt}`')
        elif t == 'ext-link':
            txt = extract_text(child)
            href = child.get('{http://www.w3.org/1999/xlink}href', '')
            parts.append(f'[{txt}]({href})')
        elif t == 'sup':
            txt = extract_text(child)
            parts.append(f'^{txt}')
        elif t == 'sub':
            txt = extract_text(child)
            parts.append(f'~{txt}~')
        elif t == 'break':
            parts.append('\n')
        elif t == 'email':
            txt = extract_text(child)
            parts.append(txt)
        elif t == 'named-content':
            txt = extract_text(child)
            parts.append(txt)
        else:
            # Fallback: extract text
            parts.append(extract_text(child))

        if child.tail:
            parts.append(child.tail)

    text = ''.join(parts)
    # Clean up whitespace
    text = re.sub(r'\n\s*\n', '\n\n', text)
    text = re.sub(r'  +', ' ', text)
    return text.strip()


def process_figure(fig):
    """Process a figure element."""
    result = ''
    label = fig.find('label')
    caption = fig.find('caption')

    label_text = label.text if label is not None else ''
    caption_text = ''
    if caption is not None:
        caption_parts = []
        for p in caption.findall('p'):
            caption_parts.append(process_paragraph(p))
        caption_text = ' '.join(caption_parts)

    if label_text or caption_text:
        result += f'\n> **{label_text}** {caption_text}\n\n'

    return result


def process_table(table_wrap):
    """Process a table element (simplified)."""
    result = ''
    label = table_wrap.find('label')
    caption = table_wrap.find('caption')

    label_text = label.text if label is not None else ''
    if label_text:
        result += f'\n**{label_text}**\n\n'

    if caption is not None:
        for p in caption.findall('p'):
            result += process_paragraph(p) + '\n\n'

    return result


def process_list(list_el):
    """Process a list element."""
    result = ''
    for item in list_el.findall('list-item'):
        for p in item.findall('p'):
            result += f'- {process_paragraph(p)}\n'
    result += '\n'
    return result


def main():
    if len(sys.argv) < 2:
        print("Usage: python jats_to_markdown.py <input.xml> [output.md]", file=sys.stderr)
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None

    tree = ET.parse(input_file)
    root = tree.getroot()

    # Strip namespace from root if needed
    md = process_element(root)

    # Post-process: clean up
    md = re.sub(r'\n{3,}', '\n\n', md)
    # Fix Im/Re rendered as \mathbb{I}\mathrm{m} / \mathbb{R}\mathrm{e}
    md = md.replace(r'\mathbb{I}\mathrm{m}', r'\operatorname{Im}')
    md = md.replace(r'\mathbb{R}\mathrm{e}', r'\operatorname{Re}')

    if output_file:
        with open(output_file, 'w') as f:
            f.write(md)
        print(f"Written to {output_file}", file=sys.stderr)
    else:
        print(md)


if __name__ == '__main__':
    main()
