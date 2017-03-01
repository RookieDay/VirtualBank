//VSAMCRE  JOB SUMMER,CLASS=A,MSGCLASS=A,REGION=4096K,MSGLEVEL=(1,1),   00010000
//       NOTIFY=&SYSUID                                                 00020000
//STEP010 EXEC PGM=IDCAMS                                               00030000
//SYSPRINT DD  SYSOUT=*                                                 00040000
//SYSIN    DD  *                                                        00050000
 DELETE CLPS.VBS.RND.CI.OVS.CIAPP.AIX1 PURGE                            00060002
 IF MAXCC = 8 THEN SET MAXCC = 0                                        00070000
//*                                                                     00080000
//STEP020 EXEC PGM=IDCAMS                                               00090000
//SYSPRINT DD  SYSOUT=*                                                 00100000
//SYSIN    DD  *                                                        00110000
 DEFINE AIX                                                     -       00120000
       ( NAME(CLPS.VBS.RND.CI.OVS.CIAPP.AIX1)                   -       00130002
         VOLUME(DMTU06)                                         -       00140001
         KEYS(18 34)                                            -       00150002
         RELATE(CLPS.VBS.RND.CI.OVS.CIAPP)                      -       00160001
         RECORDS(10000 10000)                                   -       00170002
         RECORDSIZE(205 205)                                    -       00180002
         SHAREOPTIONS(3)                                        -       00190000
         NONUNIQUEKEY                                           -       00200000
         UPGRADE                                                -       00210000
       )                                                                00220000
//*                                                                     00230000
//STEP030 EXEC PGM=IDCAMS                                               00240000
//SYSPRINT DD  SYSOUT=*                                                 00250000
//SYSIN    DD  *                                                        00260000
   DEFINE PATH(                           -                             00270000
          NAME(CLPS.VBS.RND.CI.OVS.CIAPP.PATH1) -                       00280002
          PATHENTRY(CLPS.VBS.RND.CI.OVS.CIAPP.AIX1) -                   00290002
          UPDATE)                                                       00300000
//*                                                                     00310000
//STEP040 EXEC PGM=IDCAMS                                               00320000
//FILE1    DD DSN=CLPS.VBS.RND.CI.OVS.CIAPP,DISP=SHR                    00330002
//FILE2    DD DSN=CLPS.VBS.RND.CI.OVS.CIAPP.AIX1,DISP=SHR               00340002
//SYSPRINT DD SYSOUT=*                                                  00350000
//SYSIN    DD  *                                                        00360000
   BLDINDEX INFILE(FILE1)  -                                            00370000
            OUTFILE(FILE2) -                                            00380000
            INTERNALSORT                                                00390000
//*                                                                     00400000
