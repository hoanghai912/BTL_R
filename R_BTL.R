#       BAI TAP LON MON CAU TRUC ROI RAC CHO KHOA HOC MAY TINH (CO1007)
#                       Lop: L03  Nhom: 37  De: 7625
# ______________________________________________________________________________
#     Ung dung thong ke khao sat ket qua cua kiem tra mon Cau truc roi rac
#histogram in R

#*****************__SOURCE__*****************__SOURCE__*****************************
#Them thu vien 
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  
#Them source file
  GK <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "GK", skip = 4)
  CK <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "CK", skip = 4)
  GK_0 <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "GK_0", range = "B13:AA16", col_names = FALSE)
  CK_0 <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "CK_0", range = "B13:AN16", col_names = FALSE)
  
#*****************__FUNCTION__*****************__FUNCTION__*****************************
 
#Dem so sv co so diem lon hon x diem
  Bo_dem <- function(sourcef, x){
    d <- 0
    for (i in 1 : dim(sourcef)[1]){
      if (sourcef[i, 1] >= x) d = d + 1
    }
    return (d)
  }

#Tinh do xien
  Skewness <- function(sourcef, DTB){
    SK1 <- 0
    SK2 <- 0
    for(i in 1 : 163){
      SK1 = SK1 + (sourcef[i, 1] - DTB)^3
      SK2 = SK2 + (sourcef[i, 1] - DTB)^2
    }
    return ((SK1/163)/(SK2/(163-1))^(3/2))
  }
  
#Tinh do nhon
  Kurtosis <- function(sourcef, DTB){
    Kur1 <- 0
    Kur2 <- 0
    for(i in 1 : 163){
      Kur1 = Kur1 + (sourcef[i, 1] - DTB)^4
      Kur2 = Kur2 + (sourcef[i, 1] - DTB)^2
    }
    return ((Kur1/163)/((Kur2/163)^2))
  }
  
  
#Tinh phuong sai
  Phuongsai <- function(sourcef, tb){
    n <- dim(sourcef) [1]
    tmp <- 0
    for (x in 1 : n){
      tmp <- tmp + (sourcef[x, 1] - tb)^2
    }
    return (tmp/(163-1))
  }
  
#Dem so cau dung theo tung chuong cua tung ma de GK
  Dem_chuong <- function(sourcef, source0, de, chuong){
    n <- dim(sourcef)[1]
    m <- dim(source0)[2]
    sum <- 0
    if (n == 157) x <- 29
    else x <- 42
    for (i in 2:m){
      if (source0[de%%10, i] == chuong) {
        cau <- i - 1 
        for (j in 1 : n){
          if (sourcef[j, x] == de && !is.na(sourcef[j, cau+3])) sum <- sum + sourcef[j, cau+3]
        }
      }
    }
    return(sum[1,1])
  }
  
#Dem so cau dung theo chuong 9 & 10
  Dem_910 <-function(sourcef, de){
    n <- dim(sourcef)[1]
    sum <- 0
    if (de%%10 == 1) cau <- 22
    else if (de%%10 == 2) cau <- 13
    else if (de%%10 == 3) cau <- 32
    else cau <- 5
    for (i in 1:n){
      if (sourcef[i, 42] == de && !is.na(sourcef[i, cau+3])) sum <- sum + sourcef[i, cau+3]
    } 
    return (sum[1,1])
  }
  
#*****************__PREPROCESS__*****************__PREPROCESS__************************  
 #File GK, CK   
  n1 <- dim(GK)[1]
  n2 <- dim(CK)[1] 
  DUNG_GK <- matrix (nrow = n1, ncol = 1)
  SAI_GK <- matrix (nrow = n1, ncol = 1)
  DIEM_GK <- matrix (nrow = n1, ncol = 1)
  for (i in 1 : n1){
    DUNG_GK[i] <- sum(GK[i, 4:28])
    SAI_GK[i] <- 25 - DUNG_GK[i]
    DIEM_GK[i] <- round(DUNG_GK[i]/25*10, 1)
  }
  DataGK <- GK[,c(1:3, 29)]
  DataGK <- cbind(DataGK, DUNG_GK, SAI_GK, DIEM_GK)
  
  DUNG_CK <- matrix (nrow = n2, ncol = 1)
  SAI_CK <- matrix (nrow = n2, ncol = 1)
  DIEM_CK <- matrix (nrow = n2, ncol = 1)
  for (i in 1 : n2){
    if (!is.na(CK[i, 4])){
      DUNG_CK[i] <- sum(CK[i, 4:41])
      SAI_CK[i] <- 38 - DUNG_CK[i]
      DIEM_CK[i] <- round(DUNG_CK[i]/38*10, 1)
    }
    else DIEM_CK[i] <- 0
  }
  DataCK <- CK[,c(1:3, 42)]
  DataCK <- cbind(DataCK, DUNG_CK, SAI_CK, DIEM_CK)
  names(DataCK)[4] <- "MADE"
  BigData <- merge(DataGK,DataCK, by= "No", all= TRUE)
  for (i in 1 : dim(BigData) [1]){
    if (is.na(BigData[i, 7])) BigData[i, 7] <- 0
  }
  
 #File GK_0, CK_0
  Made <- c("2011", "2012", "2013", "2014", "SUM")
  Cau_dung_Chuong <- matrix(nrow = 5, ncol = 11)
  for (i in 1:5){
    de <- 2010 + i
    for (j in 1:6){
      if (i >= 1 && i <= 4)
        Cau_dung_Chuong[i,j] <- Dem_chuong(GK, GK_0, de, j)
      if (i==5) Cau_dung_Chuong[i,j] <-0
    }
    for (k in 7:11){
      if (i >= 1 && i <= 4){
        Cau_dung_Chuong[i,k] <- Dem_chuong(CK, CK_0, de, k)
        if (k==9 || k==10) 
          Cau_dung_Chuong[i,k] <- Cau_dung_Chuong[i,k] + Dem_910(CK, de)
      }
      if (i==5) Cau_dung_Chuong[i,k] <-0
    }
  }
  for (i in 1:11){
    for (j in 1:4)
    Cau_dung_Chuong[5, i] <- Cau_dung_Chuong[5, i] + Cau_dung_Chuong[j,i]
  }
  C1 <- Cau_dung_Chuong[1:5, 1]
  C2 <- Cau_dung_Chuong[1:5, 2]
  C3 <- Cau_dung_Chuong[1:5, 3]
  C4 <- Cau_dung_Chuong[1:5, 4]
  C5 <- Cau_dung_Chuong[1:5, 5]
  C6 <- Cau_dung_Chuong[1:5, 6]
  C7 <- Cau_dung_Chuong[1:5, 7]
  C8 <- Cau_dung_Chuong[1:5, 8]
  C9 <- Cau_dung_Chuong[1:5, 9]
  C10 <- Cau_dung_Chuong[1:5, 10]
  C11 <- Cau_dung_Chuong[1:5, 11]
  Chuong_GK <- data.frame(Made, C1, C2, C3, C4, C5, C6)
  Chuong_CK <- data.frame(Made, C7, C8, C9, C10, C11)
  
#*****************__MAIN__*****************__MAIN__*****************__MAIN__*****************
#i) Xac dinh so luong sv trong tap mau
  So_SV <- dim(BigData)[1]
  
#ii) Nhom cau hoi lien quan den so cau cua cac sv
 #1+2. Tong so cau dung & sai cua moi sv
  View(BigData)
   
 #3. So cau dung nhieu nhat & thap nhat
  So_cau_dung_max <- max(BigData[5], BigData[11], na.rm = TRUE)
  So_cau_dung_min <- min(BigData[5], BigData[11], na.rm = TRUE)
  
 #4. Bieu do: tong so cau dung cua sv theo ma de 
  # Function
   #Tinh so cau dung theo ma de x  
    MA_DE <- function(sourcef, x, n){
      tmp <- filter(sourcef, sourcef[4] == x)
      return (sum(tmp[n]))
    }
   
    #Ve bieu do theo ma de x
    BD_socau_MD <- function(x, name){
      barplot(x,main = name, xlab = "Ma de", ylab = "So luong", 
              ylim=c(0,1000),
              names.arg = c("2011", "2012", "2013", "2014"), 
              col = "darkred")
    }
    
  # GK
    D_2011_GK <- MA_DE(DataGK, 2011, 5)
    D_2012_GK <- MA_DE(DataGK, 2012, 5)
    D_2013_GK <- MA_DE(DataGK, 2013, 5)
    D_2014_GK <- MA_DE(DataGK, 2014, 5)
    DungGK <- c(D_2011_GK, D_2012_GK, D_2013_GK, D_2014_GK)
    nDungGK <- "So luong cau dung theo ma de trong tap mau GK"
    BD_socau_MD(DungGK, nDungGK)
    
  # CK
    D_2011_CK <- MA_DE(DataCK, 2011, 5)
    D_2012_CK <- MA_DE(DataCK, 2012, 5)
    D_2013_CK <- MA_DE(DataCK, 2013, 5)
    D_2014_CK <- MA_DE(DataCK, 2014, 5)
    DungCK <- c(D_2011_CK, D_2012_CK, D_2013_CK, D_2014_CK)
    nDungCK <- "So luong cau dung theo ma de trong tap mau CK"
    BD_socau_MD(DungCK, nDungCK)
  
 #5. Bieu do: tong so cau sai cua sv theo ma de
  # GK
    S_2011_GK <- MA_DE(DataGK, 2011, 6)
    S_2012_GK <- MA_DE(DataGK, 2012, 6)
    S_2013_GK <- MA_DE(DataGK, 2013, 6)
    S_2014_GK <- MA_DE(DataGK, 2014, 6) 
    SaiGK <- c(S_2011_GK, S_2012_GK, S_2013_GK, S_2014_GK)
    nSaiGK <- "So luong cau sai theo ma de trong tap mau GK"
    BD_socau_MD(SaiGK, nSaiGK)
    
  # CK
    S_2011_CK <- MA_DE(DataCK, 2011, 6)
    S_2012_CK <- MA_DE(DataCK, 2012, 6)
    S_2013_CK <- MA_DE(DataCK, 2013, 6)
    S_2014_CK <- MA_DE(DataCK, 2014, 6)
    SaiCK <- c(S_2011_CK, S_2012_CK, S_2013_CK, S_2014_CK)
    nSaiCK <- "So luong cau sai theo ma de trong tap mau CK"
    BD_socau_MD(SaiCK, nSaiCK)
  
#iii) Nhom cau hoi lien quan den diem cua cac sv
 #1. Tinh trung vi mau, cuc dai mau, cuc tieu mau
  #GK
    Trungvimau_GK <- median(BigData[,7])
    Cucdaimau_GK <- max(BigData[7])
    Cuctieumau_GK <- min(BigData[7])
    
  #CK
    Trungvimau_CK <- median(BigData[,13])
    Cucdaimau_CK <- max(BigData[13])
    Cuctieumau_CK <- min(BigData[13])
    
 #2. Dem so sv co diem >= 9
  Diemtr9 <- Bo_dem(BigData[7], 9) + Bo_dem(BigData[13], 9)
 #3. Dem so sv co diem >= 7
  Diemtr7 <- Bo_dem(BigData[7], 7) + Bo_dem(BigData[13], 7)
 #4. Dem so sv co diem >= 5
  Diemtr5 <- Bo_dem(BigData[7], 5) + Bo_dem(BigData[13], 5)
 #5. Dem so sv co diem < 5
  Diemd5 <- So_SV*2 - Diemtr5
 #6. Bieu do: pho diem sv 
  #GK
  DiemGKTheoMuc <- as.data.frame(table(unlist(BigData[7])))
  DiemGKTheoMuc <- DiemGKTheoMuc[order(DiemGKTheoMuc$Var1,decreasing = TRUE),]
  DiemGKTheoMuc[1,2] + DiemGKTheoMuc[2,2]
  
  names(DiemGKTheoMuc)[1] <- "Diem"
  names(DiemGKTheoMuc)[2] <- "SoLuong"
  
  PhoDiemGK <- ggplot(data=DiemGKTheoMuc, aes(x=Diem, y=SoLuong,group = 1))
  PhoDiemGK + geom_bar(stat = "identity",fill ="darkblue", alpha=.3,color = "black")
  
  #CK
  DiemCKTheoMuc <- as.data.frame(table(unlist(BigData[13])))
  DiemCKTheoMuc <- DiemCKTheoMuc[order(DiemCKTheoMuc$Var1,decreasing = TRUE),]
  DiemCKTheoMuc[1,2] + DiemCKTheoMuc[2,2]
  
  names(DiemCKTheoMuc)[1] <- "Diem"
  names(DiemCKTheoMuc)[2] <- "SoLuong"
  
  PhoDiemCK <- ggplot(data=DiemCKTheoMuc, aes(x=Diem, y=SoLuong,group = 1)) 
  PhoDiemCK + geom_bar(stat = "identity",fill ="darkblue", alpha=.3,color = "black")
  
  
 #7. Danh sach sv (stt, ma nhom, to) co diem so lon nhat
  #GK
    selection <- BigData[,7] == max(BigData[,7],na.rm=TRUE) 
    GK_good <- BigData[selection,] 
    GK_good_full <- GK_good[,c(1,2,3)]
  
  #CK
    selectionCK <- BigData[,13] == max(BigData[,13],na.rm=TRUE)
    CK_good <- BigData[selectionCK,]
    CK_good_full <- CK_good[,c(1,8,9)]
  
 #8. Danh sach sv (stt, ma nhom, to) co diem so nho nhat
  # GK
    selection <- BigData[,7] == min(BigData[1:157,7],na.rm=TRUE) 
    GK_bad <- BigData[selection,] 
    GK_bad_full <- GK_bad[,c(1,2,3)]
  
  # CK
    selectionCK <- BigData[,13] == min(BigData[,13],na.rm=TRUE) 
    CK_bad <- BigData[selectionCK,]
    CK_bad_full <- CK_bad[,c(1,8,9)]
  
 #9. Tinh diem so trung binh cua cac sv
  Diem_TB_GK <- round(mean(BigData[,7]), 2)
  Diem_TB_CK <- round(mean(BigData[,13]), 2)
  
 #10. So luong sinh vien co diem so trung binh
  #GK
    selectionGK <- BigData[,7] == Diem_TB_GK
    TB_GK <- BigData[selectionGK,7]
    "So luong sinh vien co diem so TB GK"
    length(TB_GK)
  
  #CK
    selectionCK <- BigData[,13 ]== Diem_TB_CK
    TB_CK <- BigData[selectionGK,9]
    "So luong sinh vien co diem so TB CK"
    length(TB_CK)
  
 #11. Do muc do phan tan diem so
  #GK
    Do_phan_tan_GK <- c ("Khoang gia tri", "Phuong sai", "Do lech chuan")
    a <- 1:3
    a[1] <- Cucdaimau_GK - Cuctieumau_GK
    a[2] <- round(Phuongsai(BigData[7], Diem_TB_GK), 2)
    a[3] <- round(sqrt(a[2]), 2)
  
    Do_phan_tan_GK <- data.frame (
      Tieuchi = c ("Khoang gia tri", "Phuong sai", "Do lech chuan"),
      Giatri = c (a[1], a[2], a[3])
    )
    
   #CK
    Do_phan_tan_CK <- c ("Khoang gia tri", "Phuong sai", "Do lech chuan")
    a <- 1:3
    a[1] <- Cucdaimau_CK - Cuctieumau_CK
    a[2] <- round(Phuongsai(BigData[13], Diem_TB_CK), 2)
    a[3] <- round(sqrt(a[2]), 2)
    
    Do_phan_tan_CK <- data.frame (
      Tieuchi = c ("Khoang gia tri", "Phuong sai", "Do lech chuan"),
      Giatri = c (a[1], a[2], a[3])
    )
  
 #12. Tinh do meo lech, do nhon cua du lieu
  #GK
    Skew_GK <- Skewness(BigData[7], Diem_TB_GK)
    Kur_GK <- Kurtosis(BigData[7], Diem_TB_GK)
    
  #CK
    Skew_CK <- Skewness(BigData[13], Diem_TB_CK)
    Kur_CK <- Kurtosis(BigData[13], Diem_TB_CK)
    
 #13. Tinh tu phan vi thu nhat (Q1) va thu 3 (Q3)
  #GK
    summary(BigData[7])[2]
    summary(BigData[7])[5]
  
  #CK
    summary(BigData[13])[2]
    summary(BigData[13])[5]

 #14. So luong sv co diem so o 2 muc diem cao nhat
  #GK
    DiemGKTheoMuc <- as.data.frame(table(unlist(BigData[7])))
    DiemGKTheoMuc <- DiemGKTheoMuc[order(DiemGKTheoMuc$Var1,decreasing = TRUE),]
    DiemGKTheoMuc[1,2] + DiemGKTheoMuc[2,2]
    
  #CK
    DiemCKTheoMuc <- as.data.frame(table(unlist(BigData[13])))
    DiemCKTheoMuc <- DiemCKTheoMuc[order(DiemCKTheoMuc$Var1,decreasing = TRUE),]
    DiemCKTheoMuc[1,2] + DiemCKTheoMuc[2,2]
 #15. Bieu do: pho diem cac sv co diem so o 2 muc diem cao nhat
  #GK
    DiemGK2Muc <- DiemGKTheoMuc[-c(3:19), ]
    barplot(DiemGK2Muc[ , c(2)],
        main = "Pho diem cac sinh vien o 2 muc diem cao nhat",
        xlab = "Diem",
        ylab = "So luong",
        names.arg = c("1", "2"),
        border = "red",
        col = "blue",
        density = 10
    )
   
  #CK 
    DiemCK2Muc <- DiemCKTheoMuc[-c(3:26), ]
    barplot(DiemCK2Muc[ , c(2)],
        main = "Pho diem cac sinh vien o 2 muc diem cao nhat",
        xlab = "Diem",
        ylab = "So luong",
        names.arg = c("1", "2"),
        border = "red",
        col = "blue",
        density = 10
    )
    
 #16. So luong sv co diem so o muc diem cao thu k cho truoc
  SoSVTheoMuc <- function(data, n){
    return (data[n,2])
  }
    
 #17. Bieu do: pho diem cac sv co diem so o muc diem cao thu k cho truoc
  #GK
    names(DiemGKTheoMuc)[1] <- "Diem"
    names(DiemGKTheoMuc)[2] <- "SoLuong"
    PhoDiemKMuc_GK <- function(k){
      PhoDiemmGK <- ggplot(data=DiemGKTheoMuc[1:k, ], aes(x=Diem, y=SoLuong,group = 1)) 
    PhoDiemmGK + geom_bar(stat = "identity",fill = "darkred",color = "black")
    }
    
    #nhap k vao de ve pho diem den k muc 
    PhoDiemKMuc_GK(15)
  
  #CK
    names(DiemCKTheoMuc)[1] <- "Diem"
    names(DiemCKTheoMuc)[2] <- "SoLuong"
    PhoDiemKMuc_CK <- function(k){
      PhoDiemmCK <- ggplot(data=DiemCKTheoMuc[1:k, ], aes(x=Diem, y=SoLuong,group = 1)) 
      PhoDiemmCK + geom_bar(stat = "identity",fill = "darkred",color = "black")
    }
    
    #nhap k vao de ve pho diem den k muc 
    PhoDiemKMuc_CK(12)

##v) Nhom cau hoi lien quan den tung chuong lien quan trong hoc phan
 #1. So chuong lien quan trong tap mau
  #GK
    Lietke_chuongGK <- as.data.frame(table(unlist(GK_0[,2:26])))
    names(Lietke_chuongGK)[1] <- "Chuong"
    names(Lietke_chuongGK)[2] <- "Tan xuat"
    
  #CK
    Lietke_chuongCK <- as.data.frame(table(unlist(CK_0[,2:39])))
    Lietke_chuongCK[1,2] = Lietke_chuongCK[1,2] + Lietke_chuongCK[6,2]
    Lietke_chuongCK[5,2] = Lietke_chuongCK[5,2] + Lietke_chuongCK[6,2]
    Lietke_chuongCK <- Lietke_chuongCK[1:5,]
    names(Lietke_chuongCK)[1] <- "Chuong"
    names(Lietke_chuongCK)[2] <- "Tan xuat"
    Lietke_chuongCK[1] <- c(10,11,7,8,9)
    Lietke_chuongCK <- Lietke_chuongCK[with(Lietke_chuongCK, order(Chuong)),]
    
  #Tong so chuong
    dim(Lietke_chuongGK)[1] + dim(Lietke_chuongCK)[1]
    
 #2. Liet ke cac chuong lien quan trong tap mau
  View(Lietke_chuongGK)
  View(Lietke_chuongCK)
  
 #3. Xac dinh tan xuat tuong doi cua cac chuong lien quan trong ki thi GK + bieu do
  
  barplot(Lietke_chuongGK[,c(2)],
      main = "Tan xuat tuong doi cua cac chuong trong ki thi giua ky ",
      xlab = "Chuong",
      ylab = "So luong",
      names.arg = c(1:6),
      border = "red",
      col = "blue",
      density = 10)
  
 #4. Xac dinh tan xuat tuong doi cua cac chuong lien quan trong ki thi CK + bieu do
  barplot(Lietke_chuongCK[,c(2)],
          main = "Tan xuat tuong doi cua cac chuong trong ki thi cuoi ky",
          xlab = "Chuong",
          ylab = "So luong",
          ylim = c(0,70),
          names.arg = c(7:11),
          border = "red",
          col = "blue",
          density = 10)
  
 #5. Bieu do theo phan nhom chuong cac sv co cau tra loi dung dua tren diem GK
  barplot(Cau_dung_Chuong[5,1:6], 
      names.arg = c("C1","C2","C3","C4","C5","C6"),
      ylim=range(c(0:800)),
      main="THEO CHUONG GK",
      xlab = "Chuong",
      ylab="So cau dung")
  
 #6. Bieu do theo phan nhom chuong cac sv co cau tra loi dung dua tren diem CK
  barplot(Cau_dung_Chuong[5,7:11], 
      names.arg = c("C7","C8","C9","C10","C11"),
      ylim=range(0,2000),
      main="THEO CHUONG CK",
      xlab = "Chuong",
      ylab="So cau dung")
  
 #7. Bieu do the hien so luong cau hoi cua tung chuong
  Dem_chuong <- function(sourcef, source0, de, chuong){
    n <- dim(sourcef)[1]
    m <- dim(source0)[2]
    sum <- 0
    if (n == 157) x <- 29
    else x <- 42
    for (i in 2:m){
      if (source0[de%%10, i] == chuong) {
        cau <- i - 1 
        for (j in 1 : n){
          if (sourcef[j, x] == de && !is.na(sourcef[j, cau+3])) sum <- sum + 1
        }
      }
    }
    return(sum)
  }
  
  #Dem TONG SO CAU theo chuong 9 & 10
  # Thay sum <- sum + sourcef[i, cau+3]   ===  sum <- sum + 1 ... return(sum)
  Dem_910 <-function(sourcef, de){
    n <- dim(sourcef)[1]
    sum <- 0
    if (de%%10 == 1) cau <- 22
    else if (de%%10 == 2) cau <- 13
    else if (de%%10 == 3) cau <- 32
    else cau <- 5
    for (i in 1:n){
      if (sourcef[i, 42] == de && !is.na(sourcef[i, cau+3])) sum <- sum + 1
    } 
    return (sum)
  }
  
  #-----------------------# 
  
  Made <- c("2011", "2012", "2013", "2014", "SUM")
  Cau_Chuong <- matrix(nrow = 5, ncol = 11)
  for (i in 1:5){
    de <- 2010 + i
    for (j in 1:6){
      if (i >= 1 && i <= 4)
        Cau_Chuong[i,j] <- Dem_chuong(GK, GK_0, de, j)
      if (i==5) Cau_Chuong[i,j] <-0
    }
    for (k in 7:11){
      if (i >= 1 && i <= 4){
        Cau_Chuong[i,k] <- Dem_chuong(CK, CK_0, de, k)
        if (k==9 || k==10) 
          Cau_Chuong[i,k] <- Cau_Chuong[i,k] + Dem_910(CK, de)
      }
      if (i==5) Cau_Chuong[i,k] <-0
    }
  }
  for (i in 1:11){
    for (j in 1:4)
      Cau_Chuong[5, i] <- Cau_Chuong[5, i] + Cau_Chuong[j,i]
  }
  
  
  for (i in 1:11){
    for (j in 1:4)
      Cau_Chuong[5, i] <- Cau_Chuong[5, i] + Cau_Chuong[j,i]
  }
  
  So_cau_chuong <-c(1:11)
  for (i in 1:6){
    if (Lietke_chuongGK$Chuong[i]==i){
      So_cau_chuong[i] <- Lietke_chuongGK[i,2]/4
    }
  }
  
  for (i in 7:11){
    if (Lietke_chuongCK$Chuong[i-6]==i){
      So_cau_chuong[i] <- Lietke_chuongCK[i-6,2]/4
    }
  }
  barplot(So_cau_chuong, 
          names.arg = c(1:11),
          ylim=c(0,20),
          main="Cau hoi phan theo nhom chuong",
          xlab = "Chuong",
          ylab="So cau")
 #8. Bieu do tan xuat tuong doi tich luy so sv theo phan nhom outcome cac sv 
 #   co cau tra loi sai trong ki thi GK
  #So cau sai = Tong so cau - so cau dung
  
  Sai_C1 = Cau_Chuong[5,1] - C1[5];
  Sai_C2 = Cau_Chuong[5,2] - C2[5];
  Sai_C3 = Cau_Chuong[5,3] - C3[5];
  Sai_C4 = Cau_Chuong[5,4] - C4[5];
  Sai_C5 = Cau_Chuong[5,5] - C5[5];
  Sai_C6 = Cau_Chuong[5,6] - C6[5];
  
  Cau_sai_chuong_GK <- c(Sai_C1,Sai_C2,Sai_C3,Sai_C4,Sai_C5,Sai_C6)
  
  Phantram_GK <- c(1:6)
  for (x in 1:6){
    Phantram_GK[x] <- Cau_sai_chuong_GK[x] / sum(Cau_sai_chuong_GK) * 100
  }
  
  data_1 <- data.frame(
    Chuong = c("C1","C2","C3","C4","C5","C6"),
    Phantram = Phantram_GK
  )
  
  data_1 <- data_1[with(data_1, order(Phantram)),] #Sap xep theo phan tram tu be den lon
  Tichluy <- cumsum(data_1$Phantram) #Lay % tich luy
  data_1 <- cbind(data_1,Tichluy)
  
  barplot(data_1$Tichluy, names.arg = data_1$Chuong ,main="TAN XUAT TICH LUY CAU SAI THEO CHUONG GK",xlab = "Chuong",ylab="Phan tram cau sai (%)")
 #9. Bieu do tan xuat tuong doi tich luy so sv theo phan nhom outcome cac sv 
 #   co cau tra loi sai trong ki thi GK
  #Giong GK
  
  Sai_C7 = Cau_Chuong[5,7] - C7[5];
  Sai_C8 = Cau_Chuong[5,8] - C8[5];
  Sai_C9 = Cau_Chuong[5,9] - C9[5];
  Sai_C10 = Cau_Chuong[5,10] - C10[5];
  Sai_C11 = Cau_Chuong[5,11] - C11[5];
  
  Cau_sai_chuong_CK <- c(Sai_C7,Sai_C8,Sai_C9,Sai_C10,Sai_C11)
  
  Phantram_CK <- c(1:5)
  for (x in 1:5){
    Phantram_CK[x] <- Cau_sai_chuong_CK[x] / sum(Cau_sai_chuong_CK) * 100
  }
  
  data_2 <- data.frame(
    Chuong = c("C7","C8","C9","C10","C11"),
    Phantram = Phantram_CK
  )
  
  data_2 <- data_2[with(data_2, order(Phantram)),] #Sap xep theo phan tram tu be den lon
  Tichluy <- cumsum(data_2$Phantram) #Lay % tich luy
  data_2 <- cbind(data_2,Tichluy)
  
  barplot(data_2$Tichluy, names.arg = data_2$Chuong ,main="TAN XUAT TICH LUY CAU SAI THEO CHUONG CK",xlab = "Chuong",ylab="Phan tram cau sai (%)")
  
 #10. Bieu do tan xuat tuong doi tich luy theo phan nhom outcome cac sv co cau tra loi sai
  Cau_sai_chuong <- c(Sai_C1,Sai_C2,Sai_C3,Sai_C4,Sai_C5,Sai_C6,
                      Sai_C7,Sai_C8,Sai_C9,Sai_C10,Sai_C11)
  Phantram_GCK <- c(1:11)
  for (x in 1:11){
    Phantram_GCK[x] <- Cau_sai_chuong[x] / sum(Cau_sai_chuong) * 100
  }
  
  data_GCK <- data.frame(
    Chuong = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11"),
    Phantram = Phantram_GCK
  )
  
  data_GCK <- data_GCK[with(data_GCK, order(Phantram)),] #Sap xep theo phan tram tu be den lon
  Tichluy <- cumsum(data_GCK$Phantram) #Lay % tich luy
  data_GCK <- cbind(data_GCK,Tichluy)
  
  barplot(data_GCK$Tichluy, names.arg = data_GCK$Chuong 
          ,main="TAN XUAT TICH LUY CAU SAI TRONG TAP MAU"
          ,xlab = "Chuong",ylab="Phan tram cau sai (%)")
  
    
  #VI_______________________________________________
  #chuong giua ky
  DataChuongGK <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "GK_0", skip = 11)
  Lietke_chuongGK <- DataChuongGK[ ,c(3:27)]
  ChuongGK<-as.data.frame(table(unlist(Lietke_chuongGK)))
  
  
  #chuong cuoi ky
  DataChuongCK <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "CK_0", skip = 11)
  Lietke_chuongCK <- DataChuongCK[ ,c(3:40)]
  ChuongCK<-as.data.frame(table(unlist(Lietke_chuongCK)))
  #vi) Danh gia va suy luan, phan nhom sv ve do kho cua chuong
  #VI
  #___________________________________________
  #dinh nghia diem tot la tren trung binh cua tap mau
  #danh sach sv co diem tot gk
  library(dplyr)
  g <- mean(DataGK$DIEM_GK)
  
  DiemtotGK <- filter(DataGK, DataGK$DIEM_GK >= g)
  
  g2 <- mean(DataCK$DIEM_CK)
  
  DiemtotCK <- filter(DataCK, DataCK$DIEM_CK >= g)
  
  #so sanh luong diem tot ck va gk
  dim(DiemtotCK)[1] 
  dim(DiemtotGK)[1]
  
  #diem tot ck tang len  
  #khong xac dinh duoc sinh vien trong gk va ck, chi liet ke duoc trong tap gk va ck
  
  #_________________________________________
  #4
  #Tach cac nhom
  DataNhom1CK <-filter(DataCK, DataCK$MANH == "L01")
  DataNhom2CK <-filter(DataCK, DataCK$MANH == "L02")
  
  #diem cua nhom 1
  mean(DataNhom1CK$DIEM_CK)
  
  #diem cua nhom 2
  mean(DataNhom2CK$DIEM_CK)
  
  #diem tb cua nhom 2 hon ro rang so voi nhom 1
  mean(DataNhom2CK$DIEM_CK) - mean(DataNhom1CK$DIEM_CK)
  
  #so luong cac sinh vien trong nhom 2 va nhom 1 gan nhu nhau,
  #vay do diem cua 1 so sinh vien nhom 2 cao hon nhom 1 cao hon nen diem trung binh nhom 2 cao hon.
  #5____________________________
  library(dplyr)
  library(readxl)
  CKVI <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "CK", skip = 4)
  GKVI <- read_excel("C:/Users/Su Su/Desktop/Report/201_CO1007.xlsx", sheet = "GK", skip = 4)
  
  
  MD_2011_CK <- filter(CKVI,CKVI[42]==2011)
  MD_2012_CK <- filter(CKVI,CKVI[42]==2012)
  MD_2013_CK <- filter(CKVI,CKVI[42]==2013)
  MD_2014_CK <- filter(CKVI,CKVI[42]==2014)
  #####
  Cau_chuong <- function(data,val,n){
    a <- 0
    for (x in 1:n){
      if ( (as.integer(data[x])==val) && (suppressWarnings(!is.na(as.integer(data[x])))) ) a <- append(a,x)
    }
    a <- a[!a %in% 0]
    return (a)
  }
  #__________tim chuong kho nhat o gk va cuoi ky
  Tylesai <- rbind(Cau_Chuong[5, ],Cau_sai_chuong)
  
  Tyle_Sai <- Tylesai[2, ] / Tylesai[1, ]
  
  #__ chuong 5 o giua ky va chuong 7 o cuoi ky la kho nhat theo bang ty le sai
  
  #loc ra nhung nguoi sai chuong 7 de 2011
  CacCauChuongKho_2011 <- suppressWarnings(Cau_chuong(DataChuongCK[1,3:40],7,38))
  CacCauChuongKho_2011 <- append(CacCauChuongKho_2011,22)
  
  Loc_Sai <- function(data,dataXet, n ){
    for( i in 1 : dim(data)[1]){
      for( j in dataXet ){
        if(data[i,j+3] == 0){
          data[i,n] = '0'
        }
      }
    }
    data <- filter(data,data[n] == '0')
    return (data)
  }
  DSS_CK_2011 <- Loc_Sai(MD_2011_CK,CacCauChuongKho_2011,43)
  
  #loc ra nhung nguoi sai chuong 10 de 2012
  CacCauChuongKho_2012 <- suppressWarnings(Cau_chuong(DataChuongCK[2,3:40],7,38))
  CacCauChuongKho_2012 <- append(CacCauChuongKho_2012,13)
  
  DSS_CK_2012 <- Loc_Sai(MD_2012_CK,CacCauChuongKho_2012,43)
  
  #loc ra nhung nguoi sai chuong 10 de 2013
  CacCauChuongKho_2013 <- suppressWarnings(Cau_chuong(DataChuongCK[3,3:40],7,38))
  CacCauChuongKho_2013 <- append(CacCauChuongKho_2013,32)
  
  DSS_CK_2013 <- Loc_Sai(MD_2013_CK,CacCauChuongKho_2013,43)
  
  #loc ra nhung nguoi sai chuong 10 de 2014
  CacCauChuongKho_2014 <- suppressWarnings(Cau_chuong(DataChuongCK[4,3:40],7,38))
  CacCauChuongKho_2014 <- append(CacCauChuongKho_2014,5)
  
  DSS_CK_2014 <- Loc_Sai(MD_2014_CK,CacCauChuongKho_2014,43)
  
  SVSaiC7_CK <- rbind(MD_2011_CK,MD_2012_CK,MD_2013_CK,MD_2014_CK)
  
  #____________________________________GK
  MD_2011_GK <- filter(GKVI,GKVI[29]=="2011")
  MD_2012_GK <- filter(GKVI,GKVI[29]=='2012')
  MD_2013_GK <- filter(GKVI,GKVI[29]=='2013')
  MD_2014_GK <- filter(GKVI,GKVI[29]=='2014')
  
  #chuong 5 la chuong kho nhat gk
  #loc ra nhung nguoi sai chuong 5 de 2011
  CacCauChuongKho_2011_GK <- suppressWarnings(Cau_chuong(DataChuongGK[1,3:27],5,25))
  
  DSS_GK_2011 <- Loc_Sai(MD_2011_GK,CacCauChuongKho_2011_GK,30)
  #____________
  
  
  CacCauChuongKho_2012_GK <- suppressWarnings(Cau_chuong(DataChuongGK[2,3:27],5,25))
  
  DSS_GK_2012 <- Loc_Sai(MD_2012_GK,CacCauChuongKho_2012_GK,30)
  #________________________
  
  CacCauChuongKho_2013_GK <- suppressWarnings(Cau_chuong(DataChuongGK[3,3:27],5,25))
  
  
  DSS_GK_2013 <- Loc_Sai(MD_2013_GK,CacCauChuongKho_2013_GK,30)
  
  #________________
  
  CacCauChuongKho_2014_GK <- suppressWarnings(Cau_chuong(DataChuongGK[4,3:27],5,25))
  
  
  DSS_GK_2014 <- Loc_Sai(MD_2014_GK,CacCauChuongKho_2014_GK,30)
  
  
  SVSaiC5_GK <- rbind(MD_2011_GK,MD_2012_GK,MD_2013_GK,MD_2014_GK)