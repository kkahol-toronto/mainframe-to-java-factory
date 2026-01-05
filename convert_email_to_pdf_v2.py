#!/usr/bin/env python3
"""
Convert ENGINEER_HANDOFF_EMAIL.md to PDF with Mermaid diagrams
Uses Python libraries for better compatibility
"""

import re
import subprocess
import sys
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

BASE_DIR = Path(__file__).parent
EMAIL_MD = BASE_DIR / "docs" / "ENGINEER_HANDOFF_EMAIL.md"
OUTPUT_PDF = BASE_DIR / "docs" / "ENGINEER_HANDOFF_EMAIL.pdf"
OUTPUT_HTML = BASE_DIR / "docs" / "ENGINEER_HANDOFF_EMAIL.html"

def extract_mermaid_blocks(content):
    """Extract all Mermaid code blocks"""
    pattern = r'```mermaid\n(.*?)```'
    mermaid_blocks = re.findall(pattern, content, re.DOTALL)
    return mermaid_blocks

def render_mermaid_to_svg(mermaid_code, output_file):
    """Render Mermaid diagram to SVG using mermaid-cli"""
    try:
        # Write to temp file
        with NamedTemporaryFile(mode='w', suffix='.mmd', delete=False) as f:
            f.write(mermaid_code)
            temp_mmd = f.name
        
        # Render to SVG
        result = subprocess.run([
            'mmdc',
            '-i', temp_mmd,
            '-o', str(output_file),
            '-b', 'white',
            '-w', '1200',
            '-H', '800',
            '-f', 'svg'
        ], capture_output=True, text=True)
        
        Path(temp_mmd).unlink()
        
        if result.returncode == 0:
            return True
        else:
            print(f"  Warning: {result.stderr}")
            return False
    except Exception as e:
        print(f"  Error rendering: {e}")
        return False

def create_html_with_mermaid(md_content, mermaid_blocks, temp_dir):
    """Create HTML with embedded Mermaid diagrams"""
    
    # Replace mermaid blocks with embedded SVG
    html_content = md_content
    for idx, mermaid_code in enumerate(mermaid_blocks):
        svg_file = temp_dir / f"mermaid_{idx}.svg"
        if render_mermaid_to_svg(mermaid_code, svg_file):
            # Read SVG content
            svg_content = svg_file.read_text(encoding='utf-8')
            # Replace mermaid block with SVG
            pattern = f'```mermaid\n{re.escape(mermaid_code)}```'
            html_content = re.sub(pattern, f'<div class="mermaid-diagram">{svg_content}</div>', html_content, flags=re.DOTALL)
        else:
            # Fallback: use mermaid.js in browser
            pattern = f'```mermaid\n{re.escape(mermaid_code)}```'
            html_content = re.sub(pattern, f'<div class="mermaid">\n{mermaid_code}\n</div>', html_content, flags=re.DOTALL)
    
    # Convert markdown to HTML (simple conversion)
    html = f"""<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>COBOL to Java Migration - Engineer Handoff</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown.min.css">
    <style>
        body {{
            box-sizing: border-box;
            min-width: 200px;
            max-width: 980px;
            margin: 0 auto;
            padding: 45px;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
        }}
        .markdown-body {{
            background-color: white;
        }}
        .mermaid-diagram {{
            margin: 20px 0;
            text-align: center;
        }}
        .mermaid-diagram svg {{
            max-width: 100%;
            height: auto;
        }}
        .mermaid {{
            margin: 20px 0;
            text-align: center;
        }}
        @media print {{
            body {{
                padding: 20px;
            }}
            .mermaid-diagram svg {{
                page-break-inside: avoid;
            }}
        }}
    </style>
</head>
<body>
    <article class="markdown-body">
{convert_markdown_to_html(html_content)}
    </article>
    <script>
        mermaid.initialize({{ startOnLoad: true, theme: 'default' }});
    </script>
</body>
</html>"""
    
    return html

def convert_markdown_to_html(md_content):
    """Simple markdown to HTML conversion"""
    # Basic conversions
    html = md_content
    
    # Headers
    html = re.sub(r'^# (.+)$', r'<h1>\1</h1>', html, flags=re.MULTILINE)
    html = re.sub(r'^## (.+)$', r'<h2>\1</h2>', html, flags=re.MULTILINE)
    html = re.sub(r'^### (.+)$', r'<h3>\1</h3>', html, flags=re.MULTILINE)
    html = re.sub(r'^#### (.+)$', r'<h4>\1</h4>', html, flags=re.MULTILINE)
    
    # Bold
    html = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', html)
    
    # Code blocks (non-mermaid)
    html = re.sub(r'```(\w+)?\n(.*?)```', r'<pre><code>\2</code></pre>', html, flags=re.DOTALL)
    
    # Inline code
    html = re.sub(r'`([^`]+)`', r'<code>\1</code>', html)
    
    # Lists
    html = re.sub(r'^- (.+)$', r'<li>\1</li>', html, flags=re.MULTILINE)
    html = re.sub(r'(<li>.*</li>)', r'<ul>\1</ul>', html, flags=re.DOTALL)
    
    # Paragraphs
    html = re.sub(r'\n\n', r'</p><p>', html)
    html = '<p>' + html + '</p>'
    
    # Clean up empty paragraphs
    html = re.sub(r'<p>\s*</p>', '', html)
    
    return html

def html_to_pdf_with_chrome(html_file, pdf_file):
    """Convert HTML to PDF using Chrome/Chromium headless"""
    try:
        # Try chrome/chromium
        for browser in ['google-chrome', 'chromium', 'chromium-browser', '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome']:
            try:
                subprocess.run([
                    browser,
                    '--headless',
                    '--disable-gpu',
                    '--print-to-pdf=' + str(pdf_file),
                    'file://' + str(html_file.absolute())
                ], check=True, capture_output=True, timeout=30)
                return True
            except (subprocess.CalledProcessError, FileNotFoundError):
                continue
        return False
    except Exception as e:
        print(f"  Chrome conversion error: {e}")
        return False

def main():
    print("=" * 60)
    print("Converting Email to PDF")
    print("=" * 60)
    
    if not EMAIL_MD.exists():
        print(f"❌ Email file not found: {EMAIL_MD}")
        return 1
    
    with TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        
        try:
            # Read markdown
            print("\n[1] Reading markdown file...")
            md_content = EMAIL_MD.read_text(encoding='utf-8')
            
            # Extract Mermaid diagrams
            print("\n[2] Processing Mermaid diagrams...")
            mermaid_blocks = extract_mermaid_blocks(md_content)
            print(f"  Found {len(mermaid_blocks)} Mermaid diagrams")
            
            # Create HTML
            print("\n[3] Creating HTML with embedded diagrams...")
            html_content = create_html_with_mermaid(md_content, mermaid_blocks, temp_path)
            OUTPUT_HTML.write_text(html_content, encoding='utf-8')
            print(f"  ✅ HTML created: {OUTPUT_HTML}")
            
            # Convert to PDF
            print("\n[4] Converting HTML to PDF...")
            if html_to_pdf_with_chrome(OUTPUT_HTML, OUTPUT_PDF):
                print(f"  ✅ PDF created: {OUTPUT_PDF}")
                print(f"     File size: {OUTPUT_PDF.stat().st_size / 1024:.1f} KB")
            else:
                print("  ⚠️  Could not convert to PDF automatically")
                print("  📄 HTML file created - you can:")
                print("     1. Open the HTML file in a browser")
                print("     2. Print to PDF (Cmd+P / Ctrl+P)")
                print(f"     File: {OUTPUT_HTML}")
                return 0
            
        except Exception as e:
            print(f"❌ Error: {e}")
            import traceback
            traceback.print_exc()
            return 1
    
    print("\n" + "=" * 60)
    return 0

if __name__ == "__main__":
    sys.exit(main())

