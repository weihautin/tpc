C
      PROGRAM LN
C
      CHARACTER*20 R,S
      WRITE(*,*) 'INPUT FILE'
      READ(*,*) R
      WRITE(*,*) 'OUTPUT FILE'
      READ(*,*) S
      OPEN(4,FILE='LNNEW.TBL')
      OPEN(UNIT=5,FILE=R,STATUS='OLD')
      OPEN(UNIT=6,FILE=S,STATUS='NEW')
C
      CALL IIIIII
C
      STOP
      END
C
      SUBROUTINE  TIMER
C
 201  FORMAT(/1X,'TIME ',2(I2,':'),I2,'.',I2,'   DATE ',I4,2('-',I2)/)
C
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      WRITE(6,201) IHR,IMIN,ISEC,I100TH
     1            ,IYR,IMON,IDAY
C
      RETURN
      END
C-------------------------
      SUBROUTINE IIIIII
C
      CHARACTER*29 MVA
      INTEGER*4    NN
      DIMENSION A(50),D(20,6),IDX(20),AA(50,9,3),BB(50)
      DATA CK1,CK2/'    ',' -- '/
      NN=1
      CALL GETDAT(IYR,IMON,IDAY)
C
 301  FORMAT(A100)
  98  FORMAT(41A4)
  99  FORMAT(1x,A4,I4,39A4,I4,2('-',I2))
 100  FORMAT(1x,50A4)
 101  FORMAT(A1,A2,I4,2X,8A4,F8.3,I2,2X,6F8.4,2X,
     1   3F8.4,2X,6F8.4,2X,F8.3,A29)
 102  FORMAT(A1,A2,A1,I3,2X,8A4,F8.3,I2,18X,6F8.4,2X,
     1   3F8.4,2X,6F8.4,2X,F8.3,A29)
 221  FORMAT( 2I4,3F8.4,14X,I5,'     1  0',4X,4A4  ,2X,4A4,I5)
 222  FORMAT( 2I4,4F8.4,    I5   ,6X,2F5.1,2X,4A4  ,3X,4A4,I5)
 223  FORMAT( 2I5,'  1  ',6I5   ,I5,2F5.1,2(2X,2A4),2X,4A4,I5)
 211  FORMAT(1X,A2,A1,I3,2X,8A4,F8.3,I2,A2,4F8.4,2F8.3,A2,2F8.4,F8.3,
     1   A2,4F8.4,2F8.3,A2)
 250  FORMAT(1X,A2,48X,A2,48X,A2,24X,A2,48X,A2)
 251  FORMAT(1X,A2,A1,I3,2X,8A4,F8.3,I2,A2,4F8.4,2F8.3,A2,2F8.4,F8.3,
     1   A2,48X,A2)
 252  FORMAT(a2,8f8.3,8f8.4)
 264  FORMAT(1X,A2,A1,21X,'TOTAL(Ω,μMHO)=',F8.3,I2,A2,4F8.3,2F8.2,A2,
     1     2F8.3,F8.2,A2,48X,A2)
 265  FORMAT(1X,A2,1X,A1,A29,2X,'(PU)=',F8.3,I2,A2,4F8.4,2F8.4,A2,2F8.4,
     1     F8.4,A2,48X,A2)
 266  FORMAT(1X,A2,A1,21X,'TOTAL(Ω,μMHO)=',F8.3,I2,A2,4F8.3,40X,A2,48X,A2
     1       )
 267  FORMAT(1X,A2,1X,A1,A29,2X,'(PU)=',F8.3,I2,A2,4F8.4,40X,A2,48X,A2)
 274  FORMAT(1X,A2,A1,21X,'TOTAL(Ω,μMHO)=',F8.3,I2,A2,4F8.3,2F8.2,A2,
     1     2F8.3,F8.2,A2,4F8.3,2F8.2,A2)
 275  FORMAT(1X,A2,1X,A1,A29,2X,'(PU)=',F8.3,I2,A2,4F8.4,2F8.4,A2,2F8.4,
     1     F8.4,A2,4F8.4,2F8.4,A2)
C
      ITR= 5
      I0= 0
      A1M = CK1
      A2M = CK1
      A4M = CK1
      A5M = CK1
      A11M= CK1
      A12M= CK1
      A13M= CK1
      A14M= CK1
 9000 IF(I0.EQ.999) RETURN
      LN  = 0
      VBASE= 1
      IP1 = 0
      I1M =-1
      IRA1= 1.0E6
      JEAA= 9
      READ (5,100)  (BB(I),I=1,50)
C
C--------下面迴圈的READ指令為讀取表頭部份
      DO 3 J=1,JEAA
      READ (5,100)  (AA(I,J,1),I=1,50)
 3    CONTINUE
      II1= 0
      IAA= 44
      IF(BB(33).EQ.CK1) II1=  1
      IF(BB(33).EQ.CK1) IAA= 32
C
      GO TO 1001
 1000 IF(I0.GE.900) GO TO 9000
C
C--------下面的WRITE指令為列印每頁頁首上的空白行
 1001 WRITE(6,100)  (AA(I,1,1),I=1,13),(AA(I,1,1),I=18,50)
C
C--------下面的WRITE指令為列印每頁頁首上的頁碼標題行
      WRITE(6,99)   AA(1,2,1),NN,(AA(I,2,1),I=3,41),IYR,IMON,IDAY
      WRITE(6,98)   (AA(I,3,1),I=1,41)
C
C--------下面迴圈的WRITE指令為列印每頁頁首上的表頭
      DO 8 J=4,JEAA
 8    WRITE(6,100)  (AA(I,J,1),I=1,13),(AA(I,J,1),I=18,50)
      CONTINUE
C--------NN為頁碼變數
      NN=NN+1
C--------下面的READ指令為讀取INPUT檔的線路資料
 10   READ(5,102)  M1,CK3,M2,I0,(A(I),I=1,8),DS,I1
     1,R1,X1,R0,X0,B1,B0,RM0,XM0,BM0,R1D,X1D,R0D,X0D,B1D,B0D,VBASE,MVA
      CK3A= CK3
      IF(II1.EQ. 1) CK3A= CK1
C
C--------下面的指令為呼叫TABLE副程式，以取得線路的一般常數值
      IF((R1.EQ.0.0).AND.(X1.EQ.0.0))
     1 CALL TABLE(R1,X1,R0,X0,B1,B0,RM0,XM0,BM0
     1           ,R1D,X1D,R0D,X0D,B1D,B0D,A(6),A(7),A(8),IRA,VBASE,MVA)
C
      IF(I0   .GE. 900) GO TO 52
      IF(VBASE.EQ.0.0) VBASE= VBASEM
      VBASEM= VBASE
      IF((R0D.EQ.0.0).AND.(X0D.EQ.0.0).AND.
     1   (R0 .NE.0.0).AND.(X0 .NE.0.0).AND.
     1   (RM0.NE.0.0).AND.(XM0.NE.0.0)) GO TO 11
      GO TO 12
 11   R0D= (R0 +RM0)/2
      X0D= (X0 +XM0)/2
 12   IF((R0D.NE.0.0).AND.(X0D.NE.0.0).AND.
     1   (R0 .NE.0.0).AND.(X0 .NE.0.0).AND.
     1   (RM0.EQ.0.0).AND.(XM0.EQ.0.0)) GO TO 13
      GO TO 14
 13   RM0= 2*R0D-R0
      XM0= 2*X0D-X0
 14   IF((BM0.EQ.0.0).AND.(B0D.NE.0.0).AND.(B0 .NE.0.0))
     1 BM0= B0D/2-B0
C
C-----將讀入之INPUT檔該行放入D（I,1）矩陣內
      D( 1,1)= R1
      D( 2,1)= X1
      D( 3,1)= R0
      D( 4,1)= X0
      D( 5,1)= B1
      D( 6,1)= B0
      D( 7,1)= RM0
      D( 8,1)= XM0
      D( 9,1)= BM0
      D(10,1)= R1D
      D(11,1)= X1D
      D(12,1)= R0D
      D(13,1)= X0D
      D(14,1)= B1D
      D(15,1)= B0D
      D(16,1)= VBASE
      DS0= DS
C     IF(DS.EQ.0.0) DS0= 1.0
      IF(DS.LE.0.0) DS0= 1.0
C
C-----若線路超過5條，執行下列公式。
      IF(I1.LE.ITR) GO TO 16
      V1 = B1
      V2 = B0
      V1M= B1
      V2M= B0
      IF(I1.EQ.  7) B1= B0
      IRA= BM0/B1/1.7320508E-3
      ZBASE= B1**2/BM0
      D( 1,1)= R1* ZBASE
      D( 2,1)= X1* ZBASE
      D( 3,1)= (R1+R0)* ZBASE
      D( 4,1)= (X1+X0)* ZBASE
C     D( 3,1)= (R1+3*R0)* ZBASE
C     D( 4,1)= (X1+3*X0)* ZBASE
C
C-----將讀入之線路資料乘以線路長度後放入D（I,2）矩陣內
 16   DO 15 I= 1,15
      D(I,2)= D(I,1)*DS0
 15   CONTINUE
C
C-----將線路資料換算成PU值後放入D（I,3）矩陣內
      ZBASE= VBASE*VBASE/100.0
      DO 20 I= 1,4
      D(I  ,3)= D(I  ,2)/ZBASE
      D(I+9,3)= D(I+9,2)/ZBASE
 20   CONTINUE
      DO 21 I= 1,2
      D(I+4 ,3)= D(I+4 ,2)*ZBASE*1.0E-6
      D(I+13,3)= D(I+13,2)*ZBASE*1.0E-6
      D(I+6 ,3)= D(I+6 ,2)/ZBASE
 21   CONTINUE
      D( 9,3)= D( 9,2)*ZBASE*1.0E-6
C
 22   IE= 9
      IF(I1M.EQ. 2) IE= 15
C-----
      IF(I1M.LE.ITR) GO TO 23
      WRITE(6,266)  CK3,M2,DS1,I1M,CK3,(D(I,4),I=1,4),CK3,CK3A
      WRITE(6,267)  CK3,M2,MVA,DS1,I1M,CK3,(D(I,5),I=1,4),CK3,CK3A
      GO TO 30
C----
 23   V1 = VBASE
      V2 = VBASE
      IF((A(1).EQ.CK1).AND.(I1.GE.1)) GO TO 34
      IF(I1M.LE. 0) GO TO 34
      IF(IE.EQ.15) GO TO 29
C
C-------下面的WRITE指令為列印單回線 TOTAL(Ω,μMHO)= 該行
      WRITE(6,264)  CK3,M2,DS1,I1M,CK3,(D(I,4),I=1,6),CK3,
     1              (D(I,4),I=7,9),CK3,CK3A
C
C-------下面的WRITE指令為列印單回線 線路容量-----MVA     (PU)= 該行
      WRITE(6,265)  CK3,M2,MVA,DS1,I1M,CK3,(D(I,5),I=1,6),CK3,
     1              (D(I,5),I=7,9),CK3,CK3A
      GO TO 30
C
C-------下面的WRITE指令為列印雙回線 TOTAL(Ω,μMHO)= 該行
 29   WRITE(6,274)  CK3,M2,DS1,I1M,CK3,(D(I,4),I=1,6),
     1              CK3,(D(I,4),I=7,9),
     1              CK3,(D(I,4),I=10,15),CK3
C
C-------下面的WRITE指令為列印雙回線 線路容量-----MVA     (PU)= 該行
      WRITE(6,275)  CK3,M2,MVA,DS1,I1M,CK3,(D(I,5),I=1,6),
     1              CK3,(D(I,5),I=7,9),
     1              CK3,(D(I,5),I=10,15),CK3
C
 30   ILN= ILN+3
C
C-------下面的WRITE指令為列印空白行
      WRITE(6,250)  CK3,CK3,CK3,CK3,CK3A
      DO 31 J= 1,15
      IDX(J)= D(J,5)*1.0E4+0.5
 31   CONTINUE
      JS= 1
      IF(I1M.EQ.2) JS=10
      JE= JS+5
      IR1= IDX(JS  )
      IX1= IDX(JS+1)
      IR0= IDX(JS+2)
      IX0= IDX(JS+3)
      IB1= IDX(JS+4)
      IB0= IDX(JS+5)
      R1 =  D(JS  ,5)
      X1 =  D(JS+1,5)
      R0 =  D(JS+2,5)
      X0 =  D(JS+3,5)
      B1 =  D(JS+4,5)
      B0 =  D(JS+5,5)
      IRA1= IRA1*I1M
      IRA1= 1.0E6
      IP1= 0
      DO 33 I= 1,15
      D(I,4)= 0.0
      D(I,5)= 0.0
 33   CONTINUE
      DS1= 0.0
C
 34   DO 35 I= 1,15
      D(I,4)= D(I,4)+D(I,2)
      D(I,5)= D(I,5)+D(I,3)
 35   CONTINUE
      DS1 = DS1+DS
      I1M = I1
      IP1 = IP1+ 1
C
      IF(A(1).NE.CK1) A1M= A(1)
      IF(A(1).NE.CK1) A2M= A(2)
      IF(A(4).NE.CK1) A4M= A(4)
      IF(A(4).NE.CK1) A5M= A(5)
      IF(IRA.LT.IRA1) IRA1= IRA
      IF(A(11).NE.CK1) A11M= A(11)
      IF(A(11).NE.CK1) A12M= A(12)
      IF(A(13).NE.CK1) A13M= A(13)
      IF(A(13).NE.CK1) A14M= A(14)
C
      IE= 9
      IF(I1.EQ.2) IE= 15
      IF(I1.GE.1) GO TO 41
      IF(ILN.GE.39) GO TO 52
      GO TO 10
C
 41   IF(A(1).NE.CK1) LN= LN+1
      I0= LN
      IF((A(3).EQ.CK1).AND.(A(1).NE.CK1)) A(3)= CK2
C
C--------下面的WRITE指令為列印單回線資料
      IF(IE.EQ.15) GO TO 50
      WRITE(6,251) CK3,M2,I0,(A(I),I=1,8),DS,I1,CK3,(D(I,2),I=1,6),
     1             CK3,(D(I,2),I=7,9),CK3,CK3A
      GO TO 51
C
C--------下面的WRITE指令為列印雙回線資料
 50   WRITE(6,211) CK3,M2,I0,(A(I),I=1,8),DS,I1,CK3,(D(I,2),I=1,6),
     1             CK3,(D(I,2),I=7,9),CK3,
     1             (D(I,2),I=10,15),CK3
 51   ILN= ILN+1
      IF(ILN.LE.43) GO TO 10
C
C--------下面的WRITE指令為列印每頁的外框下緣部份
 52   WRITE(6,100)  (BB(I),I=1,13),(BB(I),I=18,50)
      IE= 55-ILN-10
C
C--------下面迴圈的WRITE指令為向下跳過(IE)行
      DO 53 I= 1,IE
      WRITE(6,250)
 53   CONTINUE
      ILN= 0
      IF(I0.GE.900) GO TO 9000
      GO TO 1001
C
      RETURN
      END


C
C--------下面為副程式TABLE，功能為將所需線路類型典型值抓進來使用
      SUBROUTINE TABLE(R1,X1,R0,X0,B1,B0,RM0,XM0,BM0
     1              ,R1D,X1D,R0D,X0D,B1D,B0D,CHK1,CHK2,CHK3,IRA,VBASE,M)
C
      DIMENSION A(50)
      DATA DNE/'END '/
C
 101  FORMAT(A2,I4,2X,8A4,F8.3,I2,2X,9F8.4,2X,6F8.4,2X,F8.3,26X,I5)
C
      REWIND 4
 10   READ(4,101)  CK3,I0,(A(I),I=1,8),DS,I1
     1,R1,X1,R0,X0,B1,B0,RM0,XM0,BM0,R1D,X1D,R0D,X0D,B1D,B0D,VB,IRA
      IF(A(6).EQ.DNE) GO TO 501 !完全找不到相同類型線路跳出
      IF(VBASE.NE.VB) GO TO 10  !電壓不同繼續找
      IF((A(6).EQ.CHK1).AND.(A(7).EQ.CHK2).AND.(A(8).EQ.CHK3)) GO TO 501
      !找到了跳出甚參數??
      IF(CK3.NE. 0) GO TO 10
      ! 非到尾部繼續找
      DS= 0
C
 501  RETURN
      END
