# Installation How-To

This file is a description on how to setup a VM on Azure with 4 x K80.

Source:
https://blogs.technet.microsoft.com/machinelearning/2016/09/15/building-deep-neural-networks-in-the-cloud-with-azure-gpu-vms-mxnet-and-microsoft-r-server/

# What you should do?

Attention: For installation, we assume all the packages (CUDA, cuDNN, MKL and
MXNet) are in the user’s home directory. [if not, check source above.]

1. Update apt-get:

  `sudo apt-get update`

2. Install everything needed:

  `sudo apt-get install -y libatlas-base-dev libopencv-dev libprotoc-dev
  python-numpy python-scipy make unzip git gcc g++ libcurl4-openssl-dev
  libssl-dev`

3. Update to alternatives for cc:

  `sudo update-alternatives --install /usr/bin/cc cc /usr/bin/gcc 50`

4. Install CUDA:

  `chmod 755 cuda_8.0.27_linux.run` and `sudo ./cuda_8.0.27_linux.run –override`

5. If CUDA fails:

  - `sudo apt-get update`
  - `sudo apt-get upgrade -y`
  - `sudo apt-get dist-upgrade -y`
  - `sudo apt-get install linux-image-extra-virtual`
  - `sudo apt-get install linux-source`
  - `sudo apt-get source linux-image-$(uname -r)`
  - `sudo apt-get install linux-headers-$(uname -r)`

  If it still fails:
  **`... Failed. Installed Cuda 9 via DEB package.`**

  [... sth missing ...]

6. MXNET:

  Check out repo version: `e0c7906693f0c79b0ce34a4d777c26a6bf1903c1` from Github.

  If you get an error while compiling, like this:
  ```
  sudo Rscript -e "install.packages(c('Rcpp', 'DiagrammeR', 'data.table',
  'jsonlite', 'magrittr', 'stringr', 'roxygen2'), repos =
  'https://cran.rstudio.com')"
  ```
  ... you have to make sure, that you have the right version on the C++ compiler installed vor X11!!
