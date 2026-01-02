00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE  C2INP003                                          * C2INZ003
00003 *    COMMON ENDING SYSOUT MESSAGES                              *    LV001
00004 ***************************************************************** C2INZ003
00005 *   PLACE THIS IN THR PROGRAM WRAP-UP ROUTINE                   * C2INZ003
00006 *     GET TIME AND DISPLAY PROGRAM/DATE/TIME ENDING DATA        * C2INZ003
00007 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INZ003
00008 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INZ003
00009 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INZ003
00010 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INZ003
00011 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INZ003
00012 *        C2INP002  - WRAP-UP SYSOUT DISPLAY                     * C2INZ003
00013 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INZ003
00014 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INZ003
00015 *        C2INP005  - CLOSE FILES                                * C2INZ003
00016 ***************************************************************** C2INZ003
00017                                                                   C2INZ003
00018      MOVE SPACES                      TO CC-E01W-DISPLAY-RCD      C2INZ003
00019      PERFORM 8999-WRITE-SYSOUT                                    C2INZ003
00020      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ003
00021      PERFORM 8999-WRITE-SYSOUT                                    C2INZ003
00022                                                                   C2INZ003
00023      ACCEPT SWA-CURRENT-TIME          FROM TIME                   C2INZ003
00024      MOVE SWA-CURR-HH                 TO SPR-TIME-HH              C2INZ003
00025      MOVE SWA-CURR-MM                 TO SPR-TIME-MM              C2INZ003
00026      MOVE SWA-CURR-SS                 TO SPR-TIME-SS              C2INZ003
00027      MOVE SPRL-END-LIT                TO SPR-START-END            C2INZ003
00028      MOVE SYSOUT-PROCESSING-RCD       TO CC-E01W-DISPLAY-RCD      C2INZ003
00029      PERFORM 8999-WRITE-SYSOUT                                    C2INZ003
00030                                                                   C2INZ003
00031      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ003
00032      PERFORM 8999-WRITE-SYSOUT                                    C2INZ003
00033      .                                                            C2INZ003
00034 *****  ++INCLUDE  C2INP003  END ********************************* C2INZ003
