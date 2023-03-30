Additive_RNG<-function(a,z0,c,m,n){
  Xi<-matrix(NA,n,3)
  colnames(Xi)<-c("aZ(i-1+c","Xi","Ui")
  for(i in 1:100)
  {
    Xi[i,1]<-(a*z0+c)
    Xi[i,2]<-Xi[i,1]%%m
    Xi[i,3]<-Xi[i,2]/m
    z0<-Xi[i,2]
  }
  hist(Xi[,3])
  View(Xi)
}

generate_bern <- function()
{
  # input parameter
  a <- as.integer(readline("Masukkan Nilai a: "))
  z0 <- as.integer(readline("Masukkan Nilai z0: "))
  c <- as.integer(readline("Masukkan Nilai c: "))
  m <- as.integer(readline("Masukkan Nilai m: "))
  n <- as.integer(readline("Masukkan Nilai n: "))
  p <- as.numeric(readline("Masukkan Nilai p: "))
  
  xi <- matrix(NA, n, 4)
  colnames(xi) <- c("aZ(i-1)+c", "Xi", "Ui", "Bernoulli")
  for (i in 1:n)
  {
    Zn <- xi[i, 1] <- (a * z0 + c)    # membuat nilai pada kolom pertama (Zi = aZ0 + c)
    Xi <- xi[i, 2] <- xi[i, 1] %% m   # membuat nilai kolom kedua Zi*mod
    Ui <- xi[i, 3] <- xi[i, 2] / m    # membuat nilai uniform pada kolom ketiga
    z0 <- xi[i, 2]                    # z0 akan diganti sampai perulangan berikutnya
    Y  <- Ui <= p                     # Menghitung nilai distribusi Bernoulli dari tabel Ui
    B <- xi[i, 4] <- as.numeric(Y)         # Menyimpan nilai Bernoulli dalam kolom keempat
  }
  View(xi)
}

generate_bern()