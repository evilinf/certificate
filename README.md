# certificate
### 用COBOL和cics编写的一个简易的凭证信息管理系统，包括用户凭证的管理以及数据的汇总
### 开发工具用的IBM的pcomm
### 要想使用该代码，需要将所有的文档下载，先看详细设计说明书，然后再启动开发工具，先将源代码文件夹中的每个程序拷到系统中，依次用JCL提交VSAM和MAPSET，再提交COBOL源程序，成功提交之后，再在cics上定义安装，每个源程序对应的program名和transid在程序名xls表中和详细说明书中。
### 主要包括的功能是：对凭证的增删改查，对角色的判定，对VSAM文件中的数据的汇总，对用户的增删改查
### 第一个事务号“PZXT”
### 主要有特色的功能：
# LAB2 功能选择界面

000100 IDENTIFICATION DIVISION.       
000200 PROGRAM-ID. LAB2.              
000300 DATA DIVISION.                 
000400 WORKING-STORAGE SECTION.       
       01 TSTAMP PIC S9(15) COMP-3.   
       01 RCODE PIC S9(4) COMP.       
       01 CERREC.                     
          02 CERNUM PIC 9(4).         
          02 CERDATE PIC X(10).       
          02 SUBDE PIC X(20).         
          02 NUMDE PIC 9(8).          
          02 SUBCR PIC X(20).         
          02 NUMCR PIC 9(8).          
          02 FILLOR PIC X(8).         
          02 ADUITOR PIC X(8).        
        COPY STU090A.                                     
        COPY STU090B.                                     
        COPY STU090C.                                     
        COPY STU090D.                                     
        COPY DFHAID.                                      
    LINKAGE SECTION.                                      
    01 DFHCOMMAREA.                                       
        02 CNAME PIC X(8).                                
        02 CRIGHT PIC X.                                  
    PROCEDURE DIVISION.                                                                    
        IF EIBAID = DFHPF12                                         
          PERFORM SEND-FUNMAP                                       
        ELSE IF EIBAID = DFHENTER                                   
          PERFORM PROCESS-FUN                                       
        ELSE IF EIBAID = DFHPF1                                     
          EXEC CICS RETURN END-EXEC                                 
        ELSE IF EIBAID = DFHPF3                                     
          PERFORM RETURN-LOGIN                                      
        END-IF.                                                                                                       
    SEND-FUNMAP.                                                    
        MOVE LOW-VALUE TO SELECTFO.                                 
        MOVE DFHCOMMAREA TO FUNMSGO.                                
        EXEC CICS SEND MAP('SELECTF') MAPSET('STU090A') ERASE       
            END-EXEC.                                               
    PROCESS-FUN.                                                     
        EXEC CICS RECEIVE MAP('SELECTF') MAPSET('STU090A') END-EXEC. 
        IF FUNCODEI = 0                                              
          MOVE LOW-VALUE TO CERTIFIO                                 
   ***********************RECORD LENGTH************                  
          EXEC CICS STARTBR FILE('PINGZ090') RIDFLD(CERNUM)          
            RESP(RCODE) END-EXEC                                     
          IF RCODE EQUAL DFHRESP(NORMAL)                             
            PERFORM UNTIL RCODE EQUAL DFHRESP(ENDFILE)               
              EXEC CICS READNEXT FILE('PINGZ090') INTO(CERREC)       
                RIDFLD(CERNUM) RESP(RCODE) END-EXEC                  
            END-PERFORM                                              
          END-IF                                                     
          EXEC CICS ENDBR FILE('PINGZ090') END-EXEC                  
          MOVE CERNUM TO CER-NUMO                                    
          ADD 1 TO CER-NUMO                                          
   *********************************************************         
          EXEC CICS ASKTIME ABSTIME(TSTAMP) END-EXEC                 
         EXEC CICS FORMATTIME ABSTIME(TSTAMP)                      
             DATESEP MMDDYYYY(CE-DATEO) END-EXEC                   
         MOVE CNAME TO FILLERO                                     
         EXEC CICS SEND MAP('CERTIFI') MAPSET('STU090B')           
              ERASE END-EXEC                                       
         EXEC CICS RETURN TRANSID('0903')                          
             COMMAREA(DFHCOMMAREA) LENGTH(9) END-EXEC              
       ELSE IF FUNCODEI = 1                                        
         MOVE LOW-VALUE TO CERTFUNO                                
         MOVE 'UPDATE' TO CERFUNO                                  
         EXEC CICS SEND MAP('CERTFUN') MAPSET('STU090B') ERASE     
           END-EXEC                                                
         EXEC CICS RETURN TRANSID('0904') COMMAREA(DFHCOMMAREA)    
           END-EXEC                                                
       ELSE IF FUNCODEI = 2                                        
         MOVE LOW-VALUE TO CERTFUNO                                
         MOVE 'INQUIRE' TO CERFUNO                                 
         EXEC CICS SEND MAP('CERTFUN') MAPSET('STU090B') ERASE     
            END-EXEC                                               
          EXEC CICS RETURN TRANSID('0905') COMMAREA(DFHCOMMAREA)   
            END-EXEC                                               
        ELSE IF FUNCODEI = 3                                       
          MOVE LOW-VALUE TO CERTFUNO                               
          MOVE 'DELETE' TO CERFUNO                                 
          EXEC CICS SEND MAP('CERTFUN') MAPSET('STU090B') ERASE    
            END-EXEC                                               
          EXEC CICS RETURN TRANSID('0906') COMMAREA(DFHCOMMAREA)   
            END-EXEC                                               
        ELSE IF FUNCODEI = 4                                       
          MOVE LOW-VALUE TO SUMDATAO                               
          EXEC CICS SEND MAP('SUMDATA') MAPSET('STU090D') ERASE    
            END-EXEC                                               
          EXEC CICS RETURN TRANSID('0908') COMMAREA(DFHCOMMAREA)   
            END-EXEC                                               
        ELSE IF FUNCODEI = 5                                       
          MOVE LOW-VALUE TO MONEYSO                                
         EXEC CICS SEND MAP('MONEYS') MAPSET('STU090D') ERASE       
           END-EXEC                                                 
         EXEC CICS RETURN TRANSID('0909') COMMAREA(DFHCOMMAREA)     
           END-EXEC                                                 
       ELSE IF FUNCODEI = 6                                         
         IF CRIGHT = 0                                              
           MOVE LOW-VALUE TO CERTFUNO                                                               
           EXEC CICS SEND MAP('CERTFUN') MAPSET('STU090B') ERASE    
             END-EXEC                                               
           EXEC CICS RETURN TRANSID('0907') COMMAREA(DFHCOMMAREA)   
             END-EXEC                                               
         ELSE                                                       
           MOVE 'NO RIGHT!!!' TO FUNMSGO                            
           EXEC CICS SEND MAP('SELECTF') MAPSET('STU090A')          
             ERASE END-EXEC                                         
           EXEC CICS RETURN TRANSID('0902') COMMAREA(DFHCOMMAREA)   
             END-EXEC                                               
         END-IF                                                     
       ELSE IF FUNCODEI = 7                                         
         IF CRIGHT = 0                                              
           MOVE LOW-VALUE TO USERINFO                                                               
           EXEC CICS SEND MAP('USERINF') MAPSET('STU090C') ERASE    
             END-EXEC                                               
           EXEC CICS RETURN TRANSID('0910') COMMAREA(DFHCOMMAREA)   
             END-EXEC                                               
         ELSE                                                       
           MOVE 'NO RIGHT!!!' TO FUNMSGO                            
           EXEC CICS SEND MAP('SELECTF') MAPSET('STU090A')          
             ERASE END-EXEC                                         
           EXEC CICS RETURN TRANSID('0902') COMMAREA(DFHCOMMAREA)   
             END-EXEC                                               
         END-IF                                                     
       ELSE IF FUNCODEI = 8                                         
         IF CRIGHT = 0                                              
           MOVE LOW-VALUE TO USERFUNO                               
           MOVE 'UPDATE' TO USER-FUO                                
           EXEC CICS SEND MAP('USERFUN') MAPSET('STU090C') ERASE    
             END-EXEC                                               
           EXEC CICS RETURN TRANSID('0911') COMMAREA(DFHCOMMAREA)   
             END-EXEC                                               
         ELSE                                                       
           MOVE 'NO RIGHT!!!' TO FUNMSGO                            
           EXEC CICS SEND MAP('SELECTF') MAPSET('STU090A')          
             ERASE END-EXEC                                         
           EXEC CICS RETURN TRANSID('0902') COMMAREA(DFHCOMMAREA)   
             END-EXEC                                               
         END-IF                                                     
       ELSE IF FUNCODEI = 9                                         
         IF CRIGHT = 0                                              
           MOVE LOW-VALUE TO USERFUNO                               
           MOVE 'DELETE' TO USER-FUO                                
           EXEC CICS SEND MAP('USERFUN') MAPSET('STU090C') ERASE    
               END-EXEC                                               
             EXEC CICS RETURN TRANSID('0912') COMMAREA(DFHCOMMAREA)   
               END-EXEC                                               
           ELSE                                                       
             MOVE 'NO RIGHT!!!' TO FUNMSGO                            
             EXEC CICS SEND MAP('SELECTF') MAPSET('STU090A')          
               ERASE END-EXEC                                         
             EXEC CICS RETURN TRANSID('0902') COMMAREA(DFHCOMMAREA)   
               END-EXEC                                               
           END-IF                                                     
         END-IF.                                                      
     RETURN-LOGIN.                                                    
         MOVE LOW-VALUE TO LOGINPZO.                                  
         EXEC CICS SEND MAP('LOGINPZ') MAPSET('STU090A') ERASE        
            END-EXEC.                                                 
         EXEC CICS RETURN TRANSID('PZXT') END-EXEC. 
         
# LAB8  输入起始日期将数据汇总

000100 IDENTIFICATION DIVISION.            
000200 PROGRAM-ID. LAB8.                   
000300 DATA DIVISION.                      
000400 WORKING-STORAGE SECTION.            
             COPY STU090A.                 
             COPY STU090D.                 
             COPY DFHAID.                  
       01 TSTAMP PIC S9(15) COMP-3.        
       01 RCODE PIC S9(4) COMP.            
       01 RETURNS PIC 9(12).               
       01 SDATE.                           
          02 SYEA PIC 9(4).                
          02 FILLER PIC X.                 
          02 SMON PIC 9(2).                
          02 FILLER PIC X.                 
        02 SDAY PIC 9(2).                 
     01 EDATE.                            
        02 EYEA PIC 9(4).                 
        02 FILLER PIC X.                  
        02 EMON PIC 9(2).                 
        02 FILLER PIC X.                  
        02 EDAY PIC 9(2).                 
     01 CERREC.                           
        02 CERNUM PIC 9(4).               
        02 FDATE.                         
           03 FMON PIC 9(2).              
           03 FILLER PIC X.               
           03 FDAY PIC 9(2).              
           03 FILLER PIC X.               
           03 FYEA PIC 9(4).              
        02 SUBDE PIC X(20).               
        02 NUMDE PIC 9(8).                
        02 SUBCR PIC X(20).               
      02 NUMCR PIC 9(8).                                       
      02 FILLOR PIC X(8).                                      
      02 ADUITOR PIC X(8).                                     
   LINKAGE SECTION.                                            
   01 DFHCOMMAREA.                                             
       02 CNAME PIC X(8).                                      
       02 CRIGHT PIC X.                                        
   PROCEDURE DIVISION.                                         
       EVALUATE TRUE                                           
            WHEN EIBAID = DFHPF12                              
              MOVE LOW-VALUE TO SUMDATAO                       
              PERFORM SEND-SUMDATA                             
            WHEN EIBAID = DFHENTER                             
              PERFORM PROCESS-COMPUTE                          
            WHEN EIBAID = DFHPF1                               
              EXEC CICS RETURN END-EXEC                        
            WHEN EIBAID = DFHPF3                               
              PERFORM RETURN-FUN                               
       END-EVALUATE.                                               
   SEND-SUMDATA.                                                   
       MOVE LOW-VALUE TO SUMDATAO.                                 
       EXEC CICS SEND MAP('SUMDATA') MAPSET('STU090D') ERASE       
           END-EXEC.                                               
       EXEC CICS RETURN TRANSID('0908') COMMAREA(DFHCOMMAREA)      
         END-EXEC.                                                 
   PROCESS-COMPUTE.                                                
       EXEC CICS RECEIVE MAP('SUMDATA') MAPSET('STU090D') END-EXEC.
       MOVE SDATEI TO SDATE.                                       
       MOVE EDATEI TO EDATE.                                       
       MOVE 0 TO RETURNS.                                          
       EXEC CICS STARTBR FILE('PINGZ090') RIDFLD(CERNUM)           
         RESP(RCODE) END-EXEC                                      
       IF RCODE EQUAL DFHRESP(NORMAL)                              
         PERFORM UNTIL RCODE EQUAL DFHRESP(ENDFILE)                
           EXEC CICS READNEXT FILE('PINGZ090') INTO(CERREC)        
             RIDFLD(CERNUM) RESP(RCODE) END-EXEC                                                                
          IF FYEA = SYEA AND FYEA = EYEA AND FMON = SMON AND     
             FMON = EMON AND FDAY >= SDAY AND FDAY <= EDAY       
              ADD NUMDE TO RETURNS                               
          END-IF                                                 
          IF FYEA = SYEA AND FYEA = EYEA AND FMON > SMON AND     
             FMON = EMON AND FDAY <= EDAY                        
              ADD NUMDE TO RETURNS                               
          END-IF                                                 
          IF FYEA = SYEA AND FYEA = EYEA AND FMON = SMON AND     
             FMON < EMON AND FDAY >= SDAY                        
             ADD NUMDE TO RETURNS                                
          END-IF                                                 
          IF FYEA = SYEA AND FYEA = EYEA AND FMON > SMON AND     
              FMON < EMON                                          
               ADD NUMDE TO RETURNS                                
           END-IF                                                  
           IF FYEA > SYEA AND FYEA = EYEA AND FMON <= EMON         
               ADD NUMDE TO RETURNS                                
           END-IF                                                  
           IF FYEA = SYEA AND FYEA < EYEA AND FMON >= SMON         
               ADD NUMDE TO RETURNS                                
           END-IF                                                  
           IF FYEA > SYEA AND FYEA < EYEA                          
               ADD NUMDE TO RETURNS                                
           END-IF                                                  
         END-PERFORM                                               
       EXEC CICS ENDBR FILE('PINGZ090') END-EXEC                   
       MOVE RETURNS TO RETURNSO                                    
       END-IF                                                      
       EXEC CICS SEND MAP('SUMDATA') MAPSET('STU090D') END-EXEC.   
       EXEC CICS RETURN TRANSID('0908') COMMAREA(DFHCOMMAREA)      
          END-EXEC.                                                 
    RETURN-FUN.                                                     
        MOVE LOW-VALUE TO SELECTFO.                                 
        EXEC CICS SEND MAP('SELECTF') MAPSET('STU090A')             
           ERASE END-EXEC.                                          
        EXEC CICS RETURN TRANSID('0902') COMMAREA(DFHCOMMAREA)      
           END-EXEC.                                                




