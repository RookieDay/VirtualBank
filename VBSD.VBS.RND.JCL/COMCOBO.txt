//COMCOBO  JOB 'COMPILE',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A,             00010000
//         NOTIFY=&SYSUID,COND=(4,LT)                                   00020000
//PROCLIB JCLLIB ORDER=SERS.MSI.BG.PROCLIB                              00030000
//********************************************************************* 00040000
//*        COMPILE ONLINE CICS COBOL PROGRAMS                          *00050000
//* PROCEDURE:      PRCCOBO                                            *00060000
//* SRCELIB:        THE DATASET OF COBOL SOURCE PROGRAM                *00070000
//* COPYLIB:        THE DATASET OF COPYBOOK                            *00080000
//* SUBLOAD:        THE DATASET OF SUBPROGRAMS LOAD MODULE             *00090000
//* LOADLIB:        THE DATASET OF LOAD MODULE                         *00100000
//* MEMBER:         THE MEMBER NAME OF COBOL SOURCE PROGRAM (ALSO THE * 00110000
//* LOAD MODULE NAME)                                                  *00120000
//**********************************************************************00130000
//         EXEC PRCCOBO,                                                00140000
//          TRNOPT='COBOL3,CICS,OP,DEBUG,SOURCE,SP,',                   00141066
//          SRCELIB='VBSD30.VBS.RND.SRC',                               00150080
//          COPYLIB='CLPS.VBS.RND.CPY',                                 00160000
//          SUBLOAD='CLPS.VBS.RND.LOADLIB.TEMP',                        00170082
//          LOADLIB='CLPS.VBS.RND.LOADLIB.TEMP',                        00180082
//          MEMBER='CIOH0016'                                           00190099
//*                                                                     00200000
