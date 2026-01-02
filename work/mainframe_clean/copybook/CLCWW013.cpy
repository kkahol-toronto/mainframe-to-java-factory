      *****************  START OF COPYLIB CLCWW013  ********************00010001
      *----------------------------------------------------------------*00020001
      *      BRANCH & CENTRALIZED CHECKWRITING SYSTEM (B&CCW)          *00030001
      *----------------------------------------------------------------*00040001
      *  COPYLIB MEMBER NAME:  CLCWW013            USED BY: CCCW9100   *00050001
      *                                                     CCCW9150   *00060001
      *                                                     CCMR6320   *00070001
      * FILE LAYOUT IS HARD-CODED IN CCMR6320.                         *00080001
      *----------------------------------------------------------------*00090001
      *  1099 FILE                                                     *00100001
      *  RECORD LENGTH = 320                                           *00110001
      *  THIS FILE IS CREATED IN CCCW9100 AND/OR CCCW9150.             *00120001
      *  THE FILE CONTAINS 1099 INFORMATION.                           *00130001
      ******************************************************************00140001
      *                          REVISIONS                             *00150001
      *  DATE/LEVEL/11058    AUTHOR     DESCRIPTION OF CHANGE          *00160001
      ******************************************************************00170001
      *  9/94  001  930207  P.HERFI      NEW INCLUDE MEMBER            *00180001
      *  2/00  002          CH.SUDHEER   INCREASED THE LENGTH OF THE   *00190001
      *                                  FOLLOWING FIELDS.             *00200001
      *                                  NAME                          *00210001
      *                                  CITY                          *00220001
      *                                  ADDRESS1                      *00230001
      *                                  ADDRESS2 -> ADDED NEW FIELD   *00240001
      * 11/1/2004  EPM#1371 CH.SUDHEER   RENAMED ACCOUNTING UNIT TO    *00241001
      *                                  BUSINESS UNIT AND INCREASED   *00242001
      *                                  THE FIELD SIZE FROM 4 TO 5 BYTE00243001
      * 01/3/2011  EPM#2666 R. FORTUNATE ADD 2 NEW FIELDS:             *00244102
      *                                  TEN99-TAX-TYPE                *00244202
      *                                  TEN99-TIN-IND                 *00244302
      *                                  REDUCE FILLER FROM 26 TO 24   *00244402
      ******************************************************************00244502
       01  FILLER                                PIC X(14)              00245001
           VALUE "***1099 RCD***".                                      00246001
                                                                        00247001
       01  TEN99-RECORD-HDR.                                            00248001
           05  TEN99-LOW-VALUE-HDR               PIC X(10)              00249001
                                                 VALUE LOW-VALUES.      00250001
           05  FILLER                            PIC X(02) VALUE SPACES.00260001
           05  TEN99-FILE-ID-HDR                 PIC X(17) VALUE        00270001
                                                 "B&CCW 1099 FILE  ".   00280001
           05  FILLER                            PIC X(10) VALUE SPACES.00290001
           05  TEN99-PROCESSING-DATE-HDR.                               00300001
               10  TEN99-CC-HDR                  PIC X(02).             00310001
               10  TEN99-YY-HDR                  PIC X(02).             00320001
               10  TEN99-MM-HDR                  PIC X(02).             00330001
               10  TEN99-DD-HDR                  PIC X(02).             00340001
                                                                        00350001
       01  TEN99-RCD.                                                   00360001
           05  TEN99-CO-BUS-CODE-DATA            PIC X(04).             00370001
           05  TEN99-FIN-LABEL-CODE-DATA         PIC 9(04).             00380001
           05  TEN99-ISSUING-BR-CODE-DATA        PIC X(03).             00390001
           05  FILLER                            PIC X(01).             00400001
           05  TEN99-ONSITE-CHK-NUM-DATA         PIC X(08).             00410001
           05  TEN99-NAME-DATA                   PIC X(40).             00420001
           05  TEN99-ADDRESS-LINE1-DATA          PIC X(40).             00430001
           05  TEN99-ADDRESS-LINE2-DATA          PIC X(40).             00440001
           05  TEN99-CITY-DATA                   PIC X(28).             00450001
           05  TEN99-STATE-DATA                  PIC X(05).             00460001
           05  TEN99-ZIP-DATA                    PIC X(11).             00470001
           05  TEN99-COMPASS-ENT-CDE-DATA        PIC X(03).             00480001
           05  TEN99-RSN-FOR-DISB-DATA           PIC X(60).             00490001
           05  TEN99-RPT-DISB-AMT-DATA           PIC S9(10)V99.         00500001
           05  TEN99-DISB-REQ-SEQ-NUM-DATA       PIC 9(09).             00510001
           05  TEN99-SSN-TAX-ID-DATA             PIC X(16).             00520001
           05  TEN99-PS-BUS-UNIT                 PIC X(05).             00530001
           05  TEN99-PS-OPERATION-LOC            PIC X(04).             00540001
           05  FILLER                            PIC X(24).             00550001
           05  TEN99-TAX-TYPE                    PIC X(01).             00551001
           05  TEN99-TIN-IND                     PIC X(01).             00552001
           05  TEN99-A-CONSTANT-DATA             PIC X(01) VALUE "A".   00560001
                                                                        00570001
       01  TEN99-RECORD-TLR.                                            00580001
           05  TEN99-HIGH-VALUE-TLR              PIC X(10)              00590001
                                                 VALUE HIGH-VALUES.     00600001
           05  FILLER                            PIC X(02) VALUE SPACES.00610001
           05  TEN99-WRITTEN-CNT-TLR             PIC X(08).             00620001
           05  FILLER                            PIC X(10) VALUE SPACES.00630001
           05  TEN99-TOTAL-DOLLAR-AMT-TLR        PIC S9(13)V99.         00640001
           05  FILLER                            PIC X(180) VALUE SPACE.00650001
      *******************  END OF COPYLIB CLCWW013  ********************00660001
