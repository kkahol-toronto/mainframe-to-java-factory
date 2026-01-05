#!/usr/bin/env python3
"""
Convert ENGINEER_HANDOFF_EMAIL.md to PDF with Mermaid diagrams
"""

import re
import subprocess
import sys
from pathlib import Path
from tempfile import NamedTemporaryFile

BASE_DIR = Path(__file__).parent
EMAIL_MD = BASE_DIR / "docs" / "ENGINEER_HANDOFF_EMAIL.md"
OUTPUT_PDF = BASE_DIR / "docs" / "ENGINEER_HANDOFF_EMAIL.pdf"

def check_dependencies():
    """Check if required tools are available"""
    tools = {
        'pandoc': 'pandoc',
        'mermaid-cli': 'mmdc',
    }
    
    missing = []
    for name, cmd in tools.items():
        result = subprocess.run(['which', cmd], capture_output=True)
        if result.returncode != 0:
            missing.append(name)
    
    if missing:
        print("⚠️  Missing dependencies:")
        for tool in missing:
            print(f"   - {tool}")
        print("\nInstalling mermaid-cli...")
        try:
            subprocess.run(['npm', 'install', '-g', '@mermaid-js/mermaid-cli'], check=True)
        except:
            print("❌ Could not install mermaid-cli automatically")
            print("   Please install manually: npm install -g @mermaid-js/mermaid-cli")
            return False
    
    return True

def extract_mermaid_blocks(content):
    """Extract all Mermaid code blocks and replace with image references"""
    mermaid_blocks = []
    pattern = r'```mermaid\n(.*?)```'
    
    def replace_with_image(match):
        mermaid_code = match.group(1)
        idx = len(mermaid_blocks)
        mermaid_blocks.append(mermaid_code)
        return f'![Mermaid Diagram {idx+1}](mermaid_{idx}.png)'
    
    modified_content = re.sub(pattern, replace_with_image, content, flags=re.DOTALL)
    return modified_content, mermaid_blocks

def render_mermaid_diagrams(mermaid_blocks, output_dir):
    """Render Mermaid diagrams to PNG images"""
    images = []
    for idx, mermaid_code in enumerate(mermaid_blocks):
        # Write mermaid code to temp file
        mermaid_file = output_dir / f"mermaid_{idx}.mmd"
        mermaid_file.write_text(mermaid_code, encoding='utf-8')
        
        # Render to PNG
        png_file = output_dir / f"mermaid_{idx}.png"
        try:
            subprocess.run([
                'mmdc',
                '-i', str(mermaid_file),
                '-o', str(png_file),
                '-b', 'white',
                '-w', '1200',
                '-H', '800'
            ], check=True, capture_output=True)
            images.append(png_file)
            print(f"  ✓ Rendered diagram {idx+1}")
        except subprocess.CalledProcessError as e:
            print(f"  ✗ Failed to render diagram {idx+1}: {e}")
            print(f"    stderr: {e.stderr.decode() if e.stderr else 'N/A'}")
            return None
    
    return images

def convert_md_to_pdf(md_file, pdf_file, temp_dir):
    """Convert markdown to PDF using pandoc"""
    try:
        # Use pandoc to convert markdown to PDF
        subprocess.run([
            'pandoc',
            str(md_file),
            '-o', str(pdf_file),
            '--pdf-engine=wkhtmltopdf',
            '--standalone',
            '--toc',
            '--variable', 'geometry:margin=1in',
            '--variable', 'fontsize=11pt',
        ], check=True, capture_output=True)
        return True
    except subprocess.CalledProcessError:
        # Try with default PDF engine
        try:
            subprocess.run([
                'pandoc',
                str(md_file),
                '-o', str(pdf_file),
                '--standalone',
                '--toc',
                '--variable', 'geometry:margin=1in',
            ], check=True, capture_output=True)
            return True
        except subprocess.CalledProcessError as e:
            print(f"❌ Pandoc conversion failed: {e}")
            return False

def main():
    print("=" * 60)
    print("Converting Email to PDF")
    print("=" * 60)
    
    if not EMAIL_MD.exists():
        print(f"❌ Email file not found: {EMAIL_MD}")
        return 1
    
    # Check dependencies
    if not check_dependencies():
        print("\n⚠️  Some dependencies are missing. Trying alternative approach...")
    
    # Create temp directory for intermediate files
    temp_dir = BASE_DIR / "temp_pdf_export"
    temp_dir.mkdir(exist_ok=True)
    
    try:
        # Read markdown content
        print("\n[1] Reading markdown file...")
        content = EMAIL_MD.read_text(encoding='utf-8')
        
        # Extract and render Mermaid diagrams
        print("\n[2] Extracting Mermaid diagrams...")
        modified_content, mermaid_blocks = extract_mermaid_blocks(content)
        
        if mermaid_blocks:
            print(f"  Found {len(mermaid_blocks)} Mermaid diagrams")
            print("  Rendering diagrams to images...")
            images = render_mermaid_diagrams(mermaid_blocks, temp_dir)
            
            if images is None:
                print("⚠️  Some diagrams failed to render, continuing anyway...")
        else:
            print("  No Mermaid diagrams found")
        
        # Write modified markdown to temp file
        temp_md = temp_dir / "email_with_images.md"
        temp_md.write_text(modified_content, encoding='utf-8')
        
        # Convert to PDF
        print("\n[3] Converting to PDF...")
        if convert_md_to_pdf(temp_md, OUTPUT_PDF, temp_dir):
            print(f"✅ PDF created: {OUTPUT_PDF}")
            print(f"   File size: {OUTPUT_PDF.stat().st_size / 1024:.1f} KB")
        else:
            print("❌ PDF conversion failed")
            print("\nTrying alternative: HTML export...")
            # Fallback: Create HTML
            html_file = BASE_DIR / "docs" / "ENGINEER_HANDOFF_EMAIL.html"
            subprocess.run([
                'pandoc',
                str(temp_md),
                '-o', str(html_file),
                '--standalone',
                '--toc',
                '--css', 'https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown.min.css',
            ], check=False)
            print(f"✅ HTML created: {html_file}")
            print("   You can open this in a browser and print to PDF")
            return 1
        
    finally:
        # Cleanup temp files (optional - comment out to keep for debugging)
        # import shutil
        # if temp_dir.exists():
        #     shutil.rmtree(temp_dir)
        pass
    
    print("\n" + "=" * 60)
    return 0

if __name__ == "__main__":
    sys.exit(main())

