//VSAMCRE  JOB SUMMER,CLASS=A,MSGCLASS=A,REGION=4096K,MSGLEVEL=(1,1),   00010000
//       NOTIFY=&SYSUID                                                 00020000
//STEP010  EXEC PGM=IDCAMS                                              00030000
//SYSPRINT DD SYSOUT=X                                                  00040000
//SYSIN    DD *                                                         00050000
   DELETE CLPS.VBS.RND.CI.OVS.CICARD CL PURGE ERASE          -          00060000
                                                                        00070000
   IF MAXCC = 8 THEN SET MAXCC = 0                                      00080000
                                                                        00090000
   DEFINE CLUSTER(NAME(CLPS.VBS.RND.CI.OVS.CIAULOG)        -            00100000
                      KEYS(36 0)                           -            00110000
                      TRACKS(10 10)                       -             00120000
                      VOLUME(OS39M1)                      -             00130000
                      RECSZ(57 57)                    -                 00140000
                      FREESPACE(0 2)                      -             00150000
                      NOREUSE  SHR(2,3) SPEED)             -            00160000
                 DATA(NAME(CLPS.VBS.RND.CI.OVS.CIAULOG.D)      -        00170000
                      CISZ(4096))                           -           00180000
                 INDEX(NAME(CLPS.VBS.RND.CI.OVS.CIAULOG.I))             00190000
/*                                                                      00200000
