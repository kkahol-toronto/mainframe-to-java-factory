#!/usr/bin/env python3
"""
Post-Process Generated Java Code
=================================
Consolidates all fixes needed to make generated Java code compilable.
This should be integrated into the main pipeline.
"""

import re
from pathlib import Path
from typing import Dict, Set

class JavaPostProcessor:
    """Post-processes generated Java code to fix common issues"""
    
    def __init__(self):
        # File name mappings (COBOL name -> actual file path)
        self.file_mappings = {
            "TEN99-T01R-MISC-TRANS-FILE": "ten99T01rMiscTransFile.txt",
            "TEN99-T03R-BCCW-FILE": "ten99T03rBccwFile.txt",
            "TEN99-T04R-DEFT-FILE": "ten99T04rDeftFile.txt",
            "TEN99-T05R-REJ-CYCLE-FILE": "ten99T05rRejCycleFile.txt",
            "WS-T07R-1099ENTRY-CD-FILE": "wsT07r1099entryCdFile.txt",
            "CC-R01R-CONTROL-CARD": "control.txt",
            "TEN99-T01W-TRANS-OUTPUT-FILE": "ten99T01wTransOutputFile.txt",
            "TEN99-T02W-REJ-CYCLE-OUT-FILE": "ten99T02wRejCycleOutFile.txt",
            "TEN99-C01W-REJ-RPT-OUTPUT-FILE": "ten99C01wRejRptOutputFile.txt",
            "TEN99-T03W-FOREIGN-OUTPUT-FILE": "ten99T03wForeignOutputFile.txt",
            "TEN99-T04W-EXCLUDES-FILE": "ten99T04wExcludesFile.txt",
            "CC-E01W-DISPLAY-FILE": "sysout.txt",
        }
        
        # Fields that should be String, not BigDecimal[]
        self.string_fields = {
            'ccE01wDisplayRcd', 'wsT03wForSourceCode', 'wsT03wForForeignCountry',
            'wsT03wForDetail', 'wsT01wOperLoc', 'wsT01wBranch', 'wsT01wCheckDate',
            'wsT01wCheckNum', 'wsT01wTaxType', 'wsT01wTinInd', 'wsT01wState',
            'wsT03rBccwCompassCode', 'wsT04rDeftCompassCode', 'wsT01wCheckNumKey',
            'wsT01rMiscFormCheckNumKey', 'wsT01rMiscFormBranch', 'wsT02wRejectSourceCode',
            'wsC01wRejRptSourceCode', 'wsT02wRejectKeyOpLoc', 'wsC01wRejRptKeyOpLoc',
            'wsT02wRejectKeyBrDeptCd', 'wsC01wRejRptKeyBrDeptCd', 'wsT02wRejectKeyCheckDt',
            'wsC01wRejRptKeyCheckDt', 'wsT02wRejectKeyCheckNum', 'wsC01wRejRptKeyCheckNum',
            'wsC01wRejRptTaxType', 'wsT02wRejectTaxType', 'wsC01wRejRptTinInd',
            'wsT02wRejectTinInd', 'wsT02wRejectDetail', 'wsC01wRejRptDetail',
            'wsT01wOutputTransHdr', 'wsT01wOutputTransTrl', 'wsT02wRejectRecyclingHdr',
            'wsT02wRejectRecyclingTrl', 'wsT03wForOutputHdr', 'wsT03wForOutputTrl',
            'wsT04wExcludesHdr', 'wsT04wExcludesTrl', 'wsC01wRejRptOutputHdr',
            'wsC01wRejRptOutputTrl', 'wsT01rMiscFormRcd', 'wsT03rBccwRcd',
            'wsT04rDeftRcd', 'wsT05rRejCycleRcd', 'controlCardIn1', 'sarParagraph',
        }
        
        # Fields that should be BigDecimal, not BigDecimal[]
        self.bigdecimal_fields = {
            'msgMiscFileAmount', 'msgTrailerCount', 'msgFileCount', 'msgAmount',
            'msgTrailerAmount', 'msgMiscFileAmt', 'wsT01wNbrDistRcd', 'wsT01w1099Amt',
        }
        
        # Fields that should be boolean, not String or BigDecimal[]
        self.boolean_fields = {
            'ten99T01rMiscFormEof', 'ten99T03rBccwEof', 'ten99T04rDeftEof',
            'ten99T05rRejCycleEof', 'eofControlCard', 'eof1099EntryTable',
            'endOfBccwHeaders', 'endOfBccwDetail', 'endOfDeftHeaders',
            'endOfDeftDetail', 'endOfDeftDetailYes', 'foreignIndicatorCdn',
            'foreignIndicatorPr', 'ten99T01rMiscFormEofYes', 'ten99T03rBccwEofYes',
            'ten99T04rDeftEofYes', 'ten99T05rRejCycleEofYes',
        }
        
        # Fields that should be String[], not String
        self.string_array_fields = {
            'wsT01wDistCompassCode', 'wsT01wDist1099Indic', 'stblStateCodeN',
            'stblStateCodeA', 'stblStateCodeNx', 'stblStateCodeAx', 'stblProvCodeN',
            'stblProvCodeA', 'stblProvCodeNx', 'stblProvCodeAx', 'wsT07r1099EntryCodes',
            'wsT07r1099EntryCode',
        }
        
        # Fields that should be BigDecimal[], not String
        self.bigdecimal_array_fields = {
            'wsT01wDistAmt', 'wsT01wDistAmtArr',
        }
        
        # Fields that should be String, not BigDecimal[]
        self.string_msg_fields = {
            'msgMiscFormSummary', 'msgTrailerCountDisplay', 'msgNoData',
            'msgNoDataFileName', 'msgFileName', 'msgLiteral', 'msgControlTotals',
            'msgBadFileMsg', 'msgFileId', 'msgOutputFileName', 'msgNumRcds',
            'litTransTlsDoNotAgree', 'litForMiscellaneousInput',
        }
    
    def to_java_var_name(self, cobol_name: str) -> str:
        """Convert COBOL file name to Java variable name"""
        parts = cobol_name.replace('-', ' ').split()
        return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])
    
    def fix_file_io_calls(self, code: str) -> str:
        """Replace state.openInputFile()/openOutputFile() with proper I/O"""
        
        # Fix openInputFile calls
        pattern = r'(\s+)state\.openInputFile\("([^"]+)"\);'
        def replace_input(match):
            indent = match.group(1)
            cobol_name = match.group(2)
            file_path = self.file_mappings.get(cobol_name, cobol_name.lower().replace('-', '') + ".txt")
            var_name = self.to_java_var_name(cobol_name) + "Reader"
            return f'''{indent}try {{
{indent}    java.nio.file.Path {var_name}Path = java.nio.file.Paths.get(basePath, "input", "{file_path}");
{indent}    state.{var_name} = java.nio.file.Files.newBufferedReader({var_name}Path);
{indent}}} catch (java.io.IOException e) {{
{indent}    throw new RuntimeException("Failed to open input file {file_path}: " + e.getMessage(), e);
{indent}}}'''
        
        code = re.sub(pattern, replace_input, code)
        
        # Fix openOutputFile calls
        pattern = r'(\s+)state\.openOutputFile\("([^"]+)"\);'
        def replace_output(match):
            indent = match.group(1)
            cobol_name = match.group(2)
            file_path = self.file_mappings.get(cobol_name, cobol_name.lower().replace('-', '') + ".txt")
            var_name = self.to_java_var_name(cobol_name) + "Writer"
            return f'''{indent}try {{
{indent}    java.nio.file.Path {var_name}Path = java.nio.file.Paths.get(basePath, "output", "{file_path}");
{indent}    java.nio.file.Files.createDirectories({var_name}Path.getParent());
{indent}    state.{var_name} = java.nio.file.Files.newBufferedWriter({var_name}Path);
{indent}}} catch (java.io.IOException e) {{
{indent}    throw new RuntimeException("Failed to open output file {file_path}: " + e.getMessage(), e);
{indent}}}'''
        
        code = re.sub(pattern, replace_output, code)
        
        return code
    
    def fix_initialize_calls(self, code: str) -> str:
        """Remove state.initializeXxx() calls"""
        code = re.sub(
            r'(\s+)state\.initialize(\w+)\(\);',
            lambda m: f'{m.group(1)}// Initialize {m.group(2)} - using default values',
            code
        )
        return code
    
    def add_file_fields_to_state(self, code: str) -> str:
        """Add BufferedReader/BufferedWriter fields to ProgramState"""
        
        # Check if fields already exist
        if 'ten99T01rMiscTransFileReader' in code and 'java.io.BufferedReader ten99T01rMiscTransFileReader' in code:
            return code
        
        # Find insertion point
        pattern = r'(// File readers/writers\n\s+java\.io\.BufferedReader inputReader = null;\n\s+java\.io\.BufferedWriter outputWriter = null;)'
        
        file_fields = '''// File readers/writers
        // File readers
        java.io.BufferedReader ten99T01rMiscTransFileReader;
        java.io.BufferedReader ten99T03rBccwFileReader;
        java.io.BufferedReader ten99T04rDeftFileReader;
        java.io.BufferedReader ten99T05rRejCycleFileReader;
        java.io.BufferedReader wsT07r1099entryCdFileReader;
        java.io.BufferedReader ccR01rControlCardReader;
        
        // File writers
        java.io.BufferedWriter ten99T01wTransOutputFileWriter;
        java.io.BufferedWriter ten99T02wRejCycleOutFileWriter;
        java.io.BufferedWriter ten99C01wRejRptOutputFileWriter;
        java.io.BufferedWriter ten99T03wForeignOutputFileWriter;
        java.io.BufferedWriter ten99T04wExcludesFileWriter;
        java.io.BufferedWriter ccE01wDisplayFileWriter;
        
        // Legacy fields (kept for compatibility)
        java.io.BufferedReader inputReader = null;
        java.io.BufferedWriter outputWriter = null;'''
        
        if re.search(pattern, code):
            code = re.sub(pattern, file_fields, code)
        
        return code
    
    def fix_field_types(self, code: str) -> str:
        """Fix field type declarations in ProgramState"""
        
        # Fix BigDecimal[] to String
        for field in self.string_fields:
            pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
            replacement = f'String {field} = "";'
            code = re.sub(pattern, replacement, code)
        
        # Fix BigDecimal[] to BigDecimal
        for field in self.bigdecimal_fields:
            pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
            replacement = f'java.math.BigDecimal {field} = java.math.BigDecimal.ZERO;'
            code = re.sub(pattern, replacement, code)
            # Also fix String to BigDecimal
            pattern2 = rf'String\s+{field}\s*=\s*"[^"]*";'
            code = re.sub(pattern2, replacement, code)
        
        # Fix to boolean
        for field in self.boolean_fields:
            pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
            replacement = f'boolean {field} = false;'
            code = re.sub(pattern, replacement, code)
            pattern2 = rf'String\s+{field}\s*=\s*"[^"]*";'
            code = re.sub(pattern2, replacement, code)
        
        # Fix String to String[]
        for field in self.string_array_fields:
            pattern = rf'String\s+{field}\s*=\s*"[^"]*";'
            replacement = f'String[] {field} = new String[100];'
            code = re.sub(pattern, replacement, code)
        
        # Fix String to BigDecimal[]
        for field in self.bigdecimal_array_fields:
            pattern = rf'String\s+{field}\s*=\s*"[^"]*";'
            replacement = f'java.math.BigDecimal[] {field} = new java.math.BigDecimal[10];'
            code = re.sub(pattern, replacement, code)
        
        # Fix msg fields
        for field in self.string_msg_fields:
            pattern = rf'java\.math\.BigDecimal\[\]\s+{field}\s*=\s*new\s+java\.math\.BigDecimal\[[^\]]+\];'
            replacement = f'String {field} = "";'
            code = re.sub(pattern, replacement, code)
        
        return code
    
    def fix_boolean_comparisons(self, code: str) -> str:
        """Fix boolean comparisons"""
        
        for field in self.boolean_fields:
            # Fix: state.field = "Y" -> state.field = true
            code = re.sub(
                rf'state\.{field}\s*=\s*"Y";',
                f'state.{field} = true;',
                code
            )
            # Fix: state.field = "N" -> state.field = false
            code = re.sub(
                rf'state\.{field}\s*=\s*"N";',
                f'state.{field} = false;',
                code
            )
            # Fix: if ("Y".equals(state.field)) -> if (state.field)
            code = re.sub(
                rf'"Y"\.equals\(state\.{field}\)',
                f'state.{field}',
                code
            )
            # Fix: if (state.field != null && state.field) -> if (state.field)
            code = re.sub(
                rf'state\.{field}\s*!=\s*null\s*&&\s*state\.{field}',
                f'state.{field}',
                code
            )
            # Fix: state.field.booleanValue() -> state.field
            code = re.sub(
                rf'state\.{field}\.booleanValue\(\)',
                f'state.{field}',
                code
            )
        
        return code
    
    def fix_missing_method_stubs(self, code: str) -> str:
        """Add missing method stubs"""
        
        # Check if p8020WriteRejectTransRcd exists
        if 'private void p8020WriteRejectTransRcd' not in code:
            stub = '''
private void p8020WriteRejectTransRcd(ProgramState state) {
    // ===== Original COBOL: 8020-WRITE-REJECT-TRANS-RCD =====
    try {
        if (state.ten99T02wRejCycleOutFileWriter != null) {
            String line = state.wsT02wRejectDetail != null ? state.wsT02wRejectDetail : "";
            state.ten99T02wRejCycleOutFileWriter.write(line);
            state.ten99T02wRejCycleOutFileWriter.newLine();
        }
    } catch (Exception e) {
        throw new RuntimeException("Error in p8020WriteRejectTransRcd: " + e.getMessage(), e);
    }
}

private void p8030WriteRejectReportRcd(ProgramState state) {
    // ===== Original COBOL: 8030-WRITE-REJECT-REPORT-RCD =====
    try {
        if (state.ten99C01wRejRptOutputFileWriter != null) {
            String line = state.wsC01wRejRptDetail != null ? state.wsC01wRejRptDetail : "";
            state.ten99C01wRejRptOutputFileWriter.write(line);
            state.ten99C01wRejRptOutputFileWriter.newLine();
        }
    } catch (Exception e) {
        throw new RuntimeException("Error in p8030WriteRejectReportRcd: " + e.getMessage(), e);
    }
}

private void p9998Coredump(ProgramState state) {
    // COBOL STOP RUN equivalent - for error handling
    throw new RuntimeException("COBOL STOP RUN triggered - check SYSOUT for errors");
}

private void p8999WriteSysout(ProgramState state) {
    // Write SYSOUT messages
    if (state.ccE01wDisplayRcd != null && !state.ccE01wDisplayRcd.isEmpty()) {
        System.out.println(state.ccE01wDisplayRcd);
    }
}

private void p9999CloseFiles(ProgramState state) {
    // Close all files
    try {
        if (state.inputReader != null) state.inputReader.close();
        if (state.outputWriter != null) state.outputWriter.close();
    } catch (Exception e) {
        // Ignore close errors
    }
}
'''
            # Insert before closing brace of class
            pattern = r'(private void closeFile\(Object file\) \{[^}]+\}\n\s+\})'
            code = re.sub(pattern, r'\1' + stub, code)
        
        return code
    
    def fix_structural_issues(self, code: str) -> str:
        """Fix structural issues (try-catch, braces, etc.)"""
        
        # Fix: .write.newLine() -> .newLine()
        code = re.sub(
            r'(\w+FileWriter)\.write\.newLine\(\);',
            r'\1.newLine();',
            code
        )
        
        # Fix: .initialize() on String fields
        code = re.sub(
            r'state\.(\w+)\.initialize\(\);',
            lambda m: f'state.{m.group(1)} = "";',
            code
        )
        
        # Fix: BigDecimal.ZERO -> java.math.BigDecimal.ZERO
        code = re.sub(
            r'\bBigDecimal\.(ZERO|ONE)\b',
            r'java.math.BigDecimal.\1',
            code
        )
        
        # Fix: String.valueOf(line) -> line (when line is already String)
        code = re.sub(
            r'String\.valueOf\(line\)',
            'line',
            code
        )
        
        return code
    
    def add_missing_constants(self, code: str) -> str:
        """Add missing constants to class"""
        
        if 'private static final String HIGH_VALUES' not in code:
            pattern = r'(private final String basePath;\n)'
            constants = '''private final String basePath;
    
    // COBOL constants
    private static final String LOW_VALUES = "";
    private static final String HIGH_VALUES = "\\uFFFF\\uFFFF\\uFFFF\\uFFFF"; // Maximum Unicode characters
'''
            code = re.sub(pattern, constants, code)
        
        # Add to ProgramState
        if 'String highValues =' not in code or 'String lowValues =' not in code:
            pattern = r'(String returnCode = "0";\n\s+\})'
            state_constants = '''String returnCode = "0";
        
        // COBOL constants
        String lowValues = "";
        String highValues = "\\uFFFF\\uFFFF\\uFFFF\\uFFFF";
    }'''
            code = re.sub(pattern, state_constants, code)
        
        return code
    
    def process(self, code: str) -> str:
        """Apply all post-processing fixes"""
        
        print("  Post-processing Java code...")
        
        code = self.fix_file_io_calls(code)
        code = self.fix_initialize_calls(code)
        code = self.add_file_fields_to_state(code)
        code = self.fix_field_types(code)
        code = self.fix_boolean_comparisons(code)
        code = self.fix_missing_method_stubs(code)
        code = self.fix_structural_issues(code)
        code = self.add_missing_constants(code)
        
        return code


def post_process_java_file(file_path: Path) -> None:
    """Post-process a single Java file"""
    processor = JavaPostProcessor()
    code = file_path.read_text(encoding='utf-8')
    processed = processor.process(code)
    file_path.write_text(processed, encoding='utf-8')
    print(f"  ✅ Post-processed: {file_path.name}")


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        file_path = Path(sys.argv[1])
        post_process_java_file(file_path)
    else:
        print("Usage: post_process_java.py <java_file_path>")

