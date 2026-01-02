00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE C2INW002                                           * C2INX002
00003 *    COMMON SYSOUT FILE DEFINITION SECTION                      *    LV001
00004 ***************************************************************** C2INX002
00005 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INX002
00006 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INX002
00007 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INX002
00008 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INX002
00009 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INX002
00010 *        CWINP002  - WRAP-UP SYSOUT DISPLAY                     * C2INX002
00011 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INX002
00012 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INX002
00013 *        C2INP005  - CLOSE FILES                                * C2INX002
00014 ***************************************************************** C2INX002
00015                                                                   C2INX002
00016  FD  CC-E01W-DISPLAY-FILE                                         C2INX002
00017      BLOCK CONTAINS 0 RECORDS                                     C2INX002
00018      RECORDING MODE IS F                                          C2INX002
00019      LABEL RECORDS ARE STANDARD                                   C2INX002
00020      RECORD CONTAINS 133 CHARACTERS                               C2INX002
00021      DATA RECORD IS CC-E01W-DISPLAY-RCD.                          C2INX002
00022                                                                   C2INX002
00023  01  CC-E01W-DISPLAY-RCD             PIC X(133).                  C2INX002
00024                                                                   C2INX002
00025 *****  ++INCLUDE  C2INW002  END  ******************************** C2INX002
