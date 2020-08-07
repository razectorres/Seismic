#bin/bash

clear

#=============================================================================
#=============================Dir. Programs===================================
#=============================================================================
 sub=./src
#=============================================================================
#============================area do usuario==================================
#=============================================================================
main=$sub/RTM.f90 
objp=RTM.o
exe=RTM
  
obj="campovel.o cerjan.o coeff_dampz.o Reynolds.o wave_equation.o wavelet.o global.o acoustic_forward_problem.o acoustic_forward_problem_real.o acoustic_backward_problem.o Imag_mod.o progress_bar.o filtro_laplaciano.o fonte_virtual.o"
                          
#==============================================================================
#========================flag of the module global=============================
#==============================================================================
flagm=" "
#==============================================================================
#========================flag of the main program =============================
#==============================================================================
flag="-Ofast -fbackslash"
flagsonp="-fopenmp"
#==============================================================================
#==============================Paths do compilador=============================
#==============================================================================
ifort=f95
#==============================================================================
#===========================libera espaço de RAM===============================
#============================================================================== 
 ulimit -s unlimited 
#==============================================================================
#======================Delete output data======================================
#============================================================================== 
#rm -f ./output_mod/campo_desloc_x.bin
#rm -f ./output_mod/campo_desloc_z.bin
#==============================================================================
#======================Delete executable=======================================
#============================================================================== 
 rm -f $exe
 
echo "============================================"
echo "===========copilacao de modulos============="
echo "============================================"
     
#set -x

#$ifort $flagm $flag -c $sub/global.f90 -o global.o
$ifort -c $sub/global.f90 -o global.o

#set +x
 
echo "============================================"
echo "-------compilacao de subrotinas-------------"
echo "============================================"

set -x

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/campovel.f90 -o campovel.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/cerjan.f90 -o cerjan.o 

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/coeff_dampz.f90 -o coeff_dampz.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/Reynolds.f90 -o Reynolds.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag $flagsonp -c $sub/wave_equation.f90 -o wave_equation.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/wavelet.f90 -o wavelet.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/acoustic_forward_problem.f90 -o acoustic_forward_problem.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/acoustic_forward_problem_real.f90 -o acoustic_forward_problem_real.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/acoustic_backward_problem.f90 -o acoustic_backward_problem.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/Imag_mod.f90 -o Imag_mod.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/filtro_laplaciano.f90 -o filtro_laplaciano.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/progress_bar.f90 -o progress_bar.o

# Compilando subroutina 
# ===================================================================
$ifort $flagm $flag -c $sub/fonte_virtual.f90 -o fonte_virtual.o


echo "============================================"
echo "==========copilacao e likagem==============="
echo "============================================"

set -x
$ifort $obj $flag $flagm $flagsonp -o $exe $main 
set +x

rm *.o
rm *.f90
rm *.mod

echo "============================================"
echo "===============OPENMP========================="
echo "============================================"

if [ ! -r /proc/cpuinfo ]; then
  echo "Is this Linux? Cannot find or read /proc/cpuinfo"
  exit 1
fi

num_cpus=`grep 'physical id' /proc/cpuinfo | sort -u | wc -l` 
num_cores_per_cpu=`grep 'core id' /proc/cpuinfo | sort -u | wc -l`
num_cpu_cores=`grep '^processor' /proc/cpuinfo | sort -u | wc -l`

NUM_THREADS=2 

echo "  "
echo "  --> Total de cpus -> $num_cpus"
echo "  --> Total de cores fisicos -> $num_cores_per_cpu"
echo "  --> Total de cores fisicos e virtualizados -> $num_cpu_cores "
# read -p "  --> ?? Forneca o numero de cores -> "  NUM_THREADS

export OMP_NUM_THREADS=$NUM_THREADS
export omp_nested=true
export OMP_DYNAMIC=TRUE 
echo "================================"
echo "-----------exucação-------------"
echo "================================"

time ./$exe #> saida.txt


exit 
