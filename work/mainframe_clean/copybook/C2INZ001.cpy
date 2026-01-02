00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE  C2INP001                                          * C2INZ001
00003 *    COMMON INITIAL SYSOUT DISPLAYS                             *    LV002
00004 ***************************************************************** C2INZ001
00005 *   PLACE THIS IN THE PROGRAM INITIALIZATION ROUTINE            * C2INZ001
00006 *     GET TIME AND DATE                                         * C2INZ001
00007 *     DISPLAY PANVALET INFORMATION                              * C2INZ001
00008 *     DISPLAY PROGRAM/TIME/DATA INFORMATION                     * C2INZ001
00009 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INZ001
00010 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INZ001
00011 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INZ001
00012 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INZ001
00013 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INZ001
00014 *        C2INP002  - WRAP-UP SYSOUT DISPLAY                     * C2INZ001
00015 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INZ001
00016 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INZ001
00017 *        C2INP005  - CLOSE FILES                                * C2INZ001
00018 ***************************************************************** C2INZ001
00019                                                                   C2INZ001
00020      ACCEPT SWA-CURRENT-TIME          FROM TIME                   C2INZ001
00021      MOVE SWA-CURR-HH                 TO SPR-TIME-HH              C2INZ001
00022      MOVE SWA-CURR-MM                 TO SPR-TIME-MM              C2INZ001
00023      MOVE SWA-CURR-SS                 TO SPR-TIME-SS              C2INZ001
00024                                                                   C2INZ001
00025      ACCEPT SWA-CURRENT-DATE          FROM DATE                   C2INZ001
00026      MOVE SWA-CURR-MO                 TO SPR-DATE-MO              C2INZ001
00027      MOVE SWA-CURR-DA                 TO SPR-DATE-DA              C2INZ001
00028      MOVE SWA-CURR-YR                 TO SPR-DATE-YR              C2INZ001
00029      IF SWA-CURR-YR < 90                                          C2INZ001
00030          MOVE 20                      TO SPR-DATE-CEN             C2INZ001
00031      ELSE                                                         C2INZ001
00032          MOVE 19                      TO SPR-DATE-CEN             C2INZ001
00033      END-IF                                                          CL**2
00034                                                                   C2INZ001
00035      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ001
00036      PERFORM 8999-WRITE-SYSOUT                                    C2INZ001
00037                                                                   C2INZ001
00038      MOVE PAN-VALET                   TO PANV-DATA                C2INZ001
00039      MOVE SYSOUT-PANVALET-RCD         TO CC-E01W-DISPLAY-RCD      C2INZ001
00040      PERFORM 8999-WRITE-SYSOUT                                    C2INZ001
00041                                                                   C2INZ001
00042      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ001
00043      PERFORM 8999-WRITE-SYSOUT                                    C2INZ001
00044                                                                   C2INZ001
00045      MOVE PANV-PROGRAM                TO SPR-PROGRAM              C2INZ001
00046      MOVE SPRL-START-LIT              TO SPR-START-END            C2INZ001
00047      MOVE SYSOUT-PROCESSING-RCD       TO CC-E01W-DISPLAY-RCD      C2INZ001
00048      PERFORM 8999-WRITE-SYSOUT                                    C2INZ001
00049                                                                   C2INZ001
00050      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ001
00051      PERFORM 8999-WRITE-SYSOUT                                    C2INZ001
00052                                                                   C2INZ001
00053      MOVE SPACES                      TO CC-E01W-DISPLAY-RCD      C2INZ001
00054      PERFORM 8999-WRITE-SYSOUT                                    C2INZ001
00055      .                                                            C2INZ001
00056 *****  ++INCLUDE  C2INP001  END  *********************************C2INZ001
