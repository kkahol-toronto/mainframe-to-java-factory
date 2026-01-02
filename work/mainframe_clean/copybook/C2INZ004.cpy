00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE  C2INP004                                          * C2INZ004
00003 *    COMMON ABEND INFORMATION PARAGRAPH                         *    LV001
00004 ***************************************************************** C2INZ004
00005                                                                   C2INZ004
00006  9998-COREDUMP.                                                   C2INZ004
00007                                                                   C2INZ004
00008 ***************************************************************** C2INZ004
00009 *   PLACE THIS ROUTINE AT THE PROGRAM END                       * C2INZ004
00010 *     GET TIME AND DISPLAY PROGRAM/DATA/TIME ENDING DATA        * C2INZ004
00011 *     DISPLAY THE ABEND RECORD DATA WHICH CONTAINS:             * C2INZ004
00012 *       ABEND PARAGRAPH NAME                                    * C2INZ004
00013 *       RETURN CODE                                             * C2INZ004
00014 *       DATABASE NAME                                           * C2INZ004
00015 *       TABLE NAME                                              * C2INZ004
00016 *     PRIOR TO CALLING THIS ROUTINE, FILL IN THE ABOVE FIELDS   * C2INZ004
00017 *     CALL "COREDUMP" TO TERMINATE PROCESSING ABNORMALLY        * C2INZ004
00018 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INZ004
00019 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INZ004
00020 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INZ004
00021 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INZ004
00022 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INZ004
00023 *        C2INP002  - WRAP-UP SYSOUT DISPLAY                     * C2INZ004
00024 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INZ004
00025 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INZ004
00026 *        C2INP005  - CLOSE FILES                                * C2INZ004
00027 ***************************************************************** C2INZ004
00028                                                                   C2INZ004
00029      MOVE SPACES                      TO CC-E01W-DISPLAY-RCD      C2INZ004
00030      PERFORM 8999-WRITE-SYSOUT                                    C2INZ004
00031                                                                   C2INZ004
00032      ACCEPT SWA-CURRENT-TIME          FROM TIME                   C2INZ004
00033      MOVE SWA-CURR-HH                 TO SPR-TIME-HH              C2INZ004
00034      MOVE SWA-CURR-MM                 TO SPR-TIME-MM              C2INZ004
00035      MOVE SWA-CURR-SS                 TO SPR-TIME-SS              C2INZ004
00036      MOVE SPRL-END-LIT                TO SPR-START-END            C2INZ004
00037      MOVE SYSOUT-PROCESSING-RCD       TO CC-E01W-DISPLAY-RCD      C2INZ004
00038      PERFORM 8999-WRITE-SYSOUT                                    C2INZ004
00039                                                                   C2INZ004
00040      MOVE SPACES                      TO CC-E01W-DISPLAY-RCD      C2INZ004
00041      PERFORM 8999-WRITE-SYSOUT                                    C2INZ004
00042                                                                   C2INZ004
00043      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ004
00044      PERFORM 8999-WRITE-SYSOUT                                    C2INZ004
00045                                                                   C2INZ004
00046      MOVE SYSOUT-ABEND-RCD            TO CC-E01W-DISPLAY-RCD      C2INZ004
00047      PERFORM 8999-WRITE-SYSOUT                                    C2INZ004
00048                                                                   C2INZ004
00049      MOVE SYSOUT-ASTERISKS            TO CC-E01W-DISPLAY-RCD      C2INZ004
00050      PERFORM 8999-WRITE-SYSOUT                                    C2INZ004
00051                                                                   C2INZ004
00052      PERFORM 9999-CLOSE-FILES                                     C2INZ004
00053                                                                   C2INZ004
00054      CALL "COREDUMP"                                              C2INZ004
00055      .                                                            C2INZ004
00056 *****  ++INCLUDE  C2INP004  END  ******************************** C2INZ004
