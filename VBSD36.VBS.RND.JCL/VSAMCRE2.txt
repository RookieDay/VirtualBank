//VSAMCRE  JOB SUMMER,CLASS=A,MSGCLASS=A,REGION=4096K,MSGLEVEL=(1,1),   00010000
//       NOTIFY=&SYSUID                                                 00020000
//STEP010  EXEC PGM=IDCAMS                                              00030000
//SYSPRINT DD SYSOUT=X                                                  00040000
//SYSIN    DD *                                                         00050000
   DELETE CLPS.VBS.RND.CI.OVS.CICUS CL PURGE ERASE          -           00060001
                                                                        00070000
   IF MAXCC = 8 THEN SET MAXCC = 0                                      00080000
                                                                        00090000
   DEFINE CLUSTER(NAME(CLPS.VBS.RND.CI.OVS.CICUS)          -            00100001
                      KEYS(18 105)                         -            00110001
                      TRACKS(10 10)                       -             00120000
                      VOLUME(OS39M1)                      -             00130000
                      RECSZ(498 498)                  -                 00140001
                      FREESPACE(0 2)                      -             00150000
                      NOREUSE  SHR(2,3) SPEED)             -            00160000
                 DATA(NAME(CLPS.VBS.RND.CI.OVS.CICUS.D)      -          00170001
                      CISZ(4096))                           -           00180000
                 INDEX(NAME(CLPS.VBS.RND.CI.OVS.CICUS.I))               00190001
/*                                                                      00200000
