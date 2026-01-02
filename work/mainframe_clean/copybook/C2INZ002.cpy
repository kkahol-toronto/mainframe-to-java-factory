00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE  C2INP002                                          * C2INZ002
00003 *    COMMON PARAGRAPH TO WRITE A SYSOUT RECORD                  *    LV001
00004 ***************************************************************** C2INZ002
00005                                                                   C2INZ002
00006  8999-WRITE-SYSOUT.                                               C2INZ002
00007                                                                   C2INZ002
00008 ***************************************************************** C2INZ002
00009 *   PLACE THIS ROUTINE NEAR THE END OF THE PROGRAM              * C2INZ002
00010 *     USED TO WRITE THE SYSOUT DISPLAY RECORDS ONLY             * C2INZ002
00011 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INZ002
00012 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INZ002
00013 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INZ002
00014 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INZ002
00015 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INZ002
00016 *        C2INP002  - WRAP-UP SYSOUT DISPLAY                     * C2INZ002
00017 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INZ002
00018 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INZ002
00019 *        C2INP005  - CLOSE FILES                                * C2INZ002
00020 ***************************************************************** C2INZ002
00021                                                                   C2INZ002
00022      WRITE CC-E01W-DISPLAY-RCD                                    C2INZ002
00023      MOVE SPACES                  TO CC-E01W-DISPLAY-RCD          C2INZ002
00024      .                                                            C2INZ002
00025 *****  ++INCLUDE  C2INP002  END  ******************************** C2INZ002
