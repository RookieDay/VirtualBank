//COMMAPVB JOB 'AUTHOR',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A,              00010022
//    NOTIFY=&SYSUID                                                    00020000
//PROCLIB JCLLIB ORDER=SERS.MSI.BG.PROCLIB                              00030033
//*************************************************************         00040000
//*     COMPILE PHYSICAL MAP AND GENERATE SYMBOLIC MAP        *         00050000
//* PROCEDURE: PRCMAP2                                        *         00060000
//* JCLLIB:    THE DATASET OF PROCEDURE                       *         00070000
//* SRCLIB:    THE DATASET OF PHYSICAL MAP                    *         00080000
//* CPYLIB:    THE DATASET OF SYMBOLIC MAP                    *         00081000
//* LOADLIB:   THE DATASET OF LOAD MODULE                     *         00090000
//* MEMBER:   THE MEMBER NAME OF PHYSICAL MAP,ALSO LAOD MODULE*         00100000
//*                                                  NAME     *         00110000
//*************************************************************         00120000
//STEP EXEC PROC=PRCMAP2,                                               00130000
//          SRCELIB='VBSD30.VBS.RND.MAP',    <<<CHANGED                 00140069
//          CPYLIB='VBSD30.VBS.RND.CPY.BK',     <<<CHANGED              00141072
//          LOADLIB='CLPS.VBS.RND.LOADLIB.TEMP',   <<<CHANGED           00150063
//          MEMBER='CICP03'         <<<CHANGED                          00160070
//                                                                      00170000
