rm(list = ls())
library(ggplot2)
library(ggneuro) # from muschellij2/ggneuro
library(RNifti)
library(kirby21.t1)
library(kirby21.fmri)
library(matrixStats)
library(shiny)
library(neurobase)

# fmri.nii.gz is at https://figshare.com/articles/SFO-example/5442298
fname = "fmri.nii.gz"

# read an fMRI image and then take the mean
full = readNifti(fname)
orientation(full) = "LAS"
# make a V by time matrix!
mat = apply(full, 4, c)

# img = apply(full, 1:3, mean)
img = rowMeans(mat)
# med = apply(full, 1:3, median)
# med = rowMedians(mat)

# hdr = check_nifti_header(fname)
# hdr@.Data = hdr@.Data[,,,1]
# dim_(hdr)[5] = 1
# dim_(hdr)[1] = 3
# pixdim(hdr)[5:8] = 1
res = dumpNifti(full)
d = dim(full)[1:3]
make_nifti = function(vec) {
  nifti(array(vec, dim = d),
        pixdim = res$pixdim
  )
}
img = make_nifti(img)

writenii(img, filename = "mean.nii.gz")
mat = round(mat * 1000)
saveRDS(mat, file = "fmri.rds", compress = "xz")
