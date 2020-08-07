### Overview ###
This code execute a 2D isotropic acoustic pre-stack reverse-time migration. The algorithm is divided in three main programs:

1. acoustic_forward_problem_real.f90: It is responsible for create the synthetic seismic data ("real data").
2. acoustic_forward_problem.f90: It model the seismic wave propagation in forward time and save  the  foward wavefield.
3. acoustic_backward_problem.f90: It 'depropagate' the wavefiled in reverse time. Also, execute the zero-lag cross-correlation between the source  and receivers  wavefields. 

### Structure ###
The are three folder:

1. input: It contain the input models (e.g. velocity, density, etc.) and can be set up the acquisition parameters (source and receivers position, frequency, number od samples in 'x' and 'z', etc.)
2. output: It contain the output models (e.g. RTM images)
3. src: It contain the main programs to perform the RTM algorithm 
