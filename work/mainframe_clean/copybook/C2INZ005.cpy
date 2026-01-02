00001 ***************************************************************** 06/10/99
00002 *  ++INCLUDE  C2INP005                                          * C2INZ005
00003 *    COMMON CLOSE FILES PARAGRAPH                               *    LV001
00004 ***************************************************************** C2INZ005
00005                                                                   C2INZ005
00006  9999-CLOSE-FILES.                                                C2INZ005
00007                                                                   C2INZ005
00008 ***************************************************************** C2INZ005
00009 *   PLACE THIS ROUTINE AT THE PROGRAM END                       * C2INZ005
00010 *     CLOSES THE COMMON SYSOUT MESSAGE FILE                     * C2INZ005
00011 *     AFTER THIS INCLUDE MEMBER -                               * C2INZ005
00012 *       PLACE THE REST OF YOUR FILENAMES YOU WANT TO CLOSE      * C2INZ005
00013 *       FOLLOWED BY A STRUCTURED PERIIOD.                       * C2INZ005
00014 * USE IN CONJUNCTION WITH COPY MEMBERS:                         * C2INZ005
00015 *        C2INW001  - SELECT FOR SYSOUT DISPLAY FILE             * C2INZ005
00016 *        C2INW002  - FILE DESCRIPTION FOR SYSOUT DISPLAY FILE   * C2INZ005
00017 *        C2INW003  - WORKING STORAGE SYSOUT RECORDS             * C2INZ005
00018 *        C2INP001  - INITIALIZE SYSOUT DISPLAY                  * C2INZ005
00019 *        C2INP002  - WRAP-UP SYSOUT DISPLAY                     * C2INZ005
00020 *        C2INP003  - SYSOUT DISPLAY FILE WRITE STATEMENT        * C2INZ005
00021 *        C2INP004  - ABNORMAL TERMINATION ROUTINE               * C2INZ005
00022 *        C2INP005  - CLOSE FILES                                * C2INZ005
00023 ***************************************************************** C2INZ005
00024                                                                   C2INZ005
00025      CLOSE  CC-E01W-DISPLAY-FILE                                  C2INZ005
00026                                                                   C2INZ005
00027 *****  ++INCLUDE  C2INP005  END  ******************************** C2INZ005
