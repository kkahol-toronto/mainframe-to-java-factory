00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE C2INW003                                           * C2INX003
00003 *    COMMON SYSOUT DISPLAY LINES                                *    LV001
00004 ***************************************************************** C2INX003
00005 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INX003
00006 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INX003
00007 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INX003
00008 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INX003
00009 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INX003
00010 *        C2INP002  - WRAP-UP SYSOUT DISPLAY                     * C2INX003
00011 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INX003
00012 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INX003
00013 *        C2INP005  - CLOSE FILES                                * C2INX003
00014 ***************************************************************** C2INX003
00015                                                                   C2INX003
00016  01  SYSOUT-PANVALET-RCD.                                         C2INX003
00017      05  FILLER                      PIC X(02) VALUE SPACES.      C2INX003
00018      05  PANV-LITERAL                PIC X(19) VALUE              C2INX003
00019          "PANVALET LEVEL IS: ".                                   C2INX003
00020      05  PANV-DATA.                                               C2INX003
00021          10  PANV-LEVEL              PIC 9(03) VALUE ZERO.        C2INX003
00022          10  PANV-PROGRAM            PIC X(08) VALUE SPACES.      C2INX003
00023          10  FILLER                  PIC X(02) VALUE SPACES.      C2INX003
00024          10  PANV-DATE               PIC X(08) VALUE SPACES.      C2INX003
00025      05  FILLER                      PIC X(91) VALUE SPACES.      C2INX003
00026                                                                   C2INX003
00027  01  SYSOUT-PROCESSING-RCD.                                       C2INX003
00028      05  FILLER                      PIC X(02)  VALUE SPACES.     C2INX003
00029      05  SPR-PROGRAM                 PIC X(08)  VALUE SPACES.     C2INX003
00030      05  FILLER                      PIC X(01)  VALUE SPACES.     C2INX003
00031      05  SPR-START-END               PIC X(14)  VALUE SPACES.     C2INX003
00032      05  SPR-DATE.                                                C2INX003
00033          10  SPR-DATE-MO             PIC X(02)  VALUE SPACES.     C2INX003
00034          10  FILLER                  PIC X(01)  VALUE "/".        C2INX003
00035          10  SPR-DATE-DA             PIC X(02)  VALUE SPACES.     C2INX003
00036          10  FILLER                  PIC X(01)  VALUE "/".        C2INX003
00037          10  SPR-DATE-CEN            PIC X(02)  VALUE SPACES.     C2INX003
00038          10  SPR-DATE-YR             PIC X(02)  VALUE SPACES.     C2INX003
00039      05  FILLER                      PIC X(04)  VALUE " AT ".     C2INX003
00040      05  SPR-TIME.                                                C2INX003
00041          10  SPR-TIME-HH             PIC X(02)  VALUE SPACES.     C2INX003
00042          10  FILLER                  PIC X(01)  VALUE ":".        C2INX003
00043          10  SPR-TIME-MM             PIC X(02)  VALUE SPACES.     C2INX003
00044          10  FILLER                  PIC X(01)  VALUE ":".        C2INX003
00045          10  SPR-TIME-SS             PIC X(02)  VALUE SPACES.     C2INX003
00046      05  FILLER                      PIC X(86) VALUE SPACES.      C2INX003
00047                                                                   C2INX003
00048  01  SYSOUT-ABEND-RCD.                                            C2INX003
00049      05  FILLER                      PIC X(02)  VALUE SPACES.     C2INX003
00050      05  SAR-PARAGRAPH-LIT           PIC X(10)                    C2INX003
00051                                      VALUE "ABEND IN:".           C2INX003
00052      05  SAR-PARAGRAPH               PIC X(31)  VALUE SPACES.     C2INX003
00053      05  FILLER                      PIC X(01)  VALUE SPACES.     C2INX003
00054      05  SAR-RETURN-CODE-LIT         PIC X(13)                    C2INX003
00055                                      VALUE "RETURN CODE:".        C2INX003
00056      05  SAR-RETURN-CODE             PIC X(05)  VALUE SPACES.     C2INX003
00057      05  FILLER                      PIC X(01)  VALUE SPACES.     C2INX003
00058      05  SAR-DB-NAME-LIT             PIC X(10)                    C2INX003
00059                                      VALUE "DATABASE:".           C2INX003
00060      05  SAR-DB-NAME                 PIC X(11)  VALUE SPACES.     C2INX003
00061      05  FILLER                      PIC X(01)  VALUE SPACES.     C2INX003
00062      05  SAR-TABLE-NAME-LIT          PIC X(07)                    C2INX003
00063                                      VALUE "TABLE:".              C2INX003
00064      05  SAR-TABLE-NAME              PIC X(31)  VALUE SPACES.     C2INX003
00065      05  FILLER                      PIC X(10) VALUE SPACES.      C2INX003
00066                                                                   C2INX003
00067  01  SYSOUT-COUNT-RCD.                                            C2INX003
00068      05  FILLER                      PIC X(02)  VALUE SPACES.     C2INX003
00069      05  SCR-LITERAL                 PIC X(30)                    C2INX003
00070                                      VALUE "RECORD COUNT:".       C2INX003
00071      05  SCR-COUNT                   PIC 9(13).                   C2INX003
00072      05  FILLER                      PIC X(88) VALUE SPACES.      C2INX003
00073                                                                   C2INX003
00074  01  SYSOUT-WORK-AREAS.                                           C2INX003
00075      05  SWA-CURRENT-DATE.                                        C2INX003
00076          10  SWA-CURR-YR             PIC X(02) VALUE SPACES.      C2INX003
00077          10  SWA-CURR-MO             PIC X(02) VALUE SPACES.      C2INX003
00078          10  SWA-CURR-DA             PIC X(02) VALUE SPACES.      C2INX003
00079      05  SWA-CURRENT-TIME.                                        C2INX003
00080          10  SWA-CURR-HH             PIC X(02) VALUE SPACES.      C2INX003
00081          10  SWA-CURR-MM             PIC X(02) VALUE SPACES.      C2INX003
00082          10  SWA-CURR-SS             PIC X(02) VALUE SPACES.      C2INX003
00083                                                                   C2INX003
00084  01  SYSOUT-LITERALS.                                             C2INX003
00085      05  SYSOUT-COUNT-LITERALS.                                   C2INX003
00086          10  SCL-READ-LIT            PIC X(13)                    C2INX003
00087                                      VALUE "RECORDS READ:".       C2INX003
00088          10  SCL-WRITTEN-LIT         PIC X(16)                    C2INX003
00089                                      VALUE "RECORDS WRITTEN:".    C2INX003
00090          10  SCL-PAGE-LIT            PIC X(14)                    C2INX003
00091                                      VALUE "PAGES WRITTEN:".      C2INX003
00092          10  SCL-LINE-LIT            PIC X(14)                    C2INX003
00093                                      VALUE "LINES WRITTEN:".      C2INX003
00094      05  SYSOUT-ASTERISKS.                                        C2INX003
00095          10  FILLER                  PIC X(02) VALUE SPACES.      C2INX003
00096          10  FILLER                  PIC X(44)                    C2INX003
00097             VALUE "********************************************". C2INX003
00098          10  FILLER                  PIC X(44)                    C2INX003
00099             VALUE "********************************************". C2INX003
00100          10  FILLER                  PIC X(43)                    C2INX003
00101             VALUE "*******************************************".  C2INX003
00102      05  SYSOUT-ABEND-LIT.                                        C2INX003
00103          10  SAL-PARAGRAPH-LIT       PIC X(09)                    C2INX003
00104                                      VALUE "ABEND IN:".           C2INX003
00105          10  SAL-RETURN-CODE-LIT     PIC X(12)                    C2INX003
00106                                      VALUE "RETURN CODE:".        C2INX003
00107          10  SAL-DB-NAME-LIT         PIC X(09)                    C2INX003
00108                                      VALUE "DATABASE:".           C2INX003
00109          10  SAL-TABLE-NAME-LIT      PIC X(06)                    C2INX003
00110                                      VALUE "TABLE:".              C2INX003
00111      05  SYSOUT-PROCESSING-RCD-LITERALS.                          C2INX003
00112          10  SPRL-START-LIT          PIC X(11)                    C2INX003
00113                                      VALUE "STARTED ON:".         C2INX003
00114          10  SPRL-END-LIT            PIC X(09)                    C2INX003
00115                                      VALUE "ENDED ON:".           C2INX003
00116 *****  ++INCLUDE  C2INW003  END  ******************************** C2INX003
