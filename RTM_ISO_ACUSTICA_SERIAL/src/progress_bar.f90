subroutine progress_bar(iteration, maximum)
    !
    ! Prints progress bar.
    !
    ! Args: 
    !     iteration - iteration number
    !     maximum - total iterations
    !
        implicit none
        integer :: iteration, maximum
        integer :: counter
        integer :: step, done
    
        step = nint(iteration * 100 / (1.0 * maximum))
        done = floor(step / 10.0)  ! mark every 10%
    
        do counter = 1, 36+15                    ! clear whole line - 36 chars
            write(6,'(a)',advance='no') '\b'  ! (\b - backslash)
        end do
    
        write(6,'(15x,a)',advance='no') '--> In progress... ['
        if (done .LE. 0) then
            do counter = 1, 10
                write(6,'(a)',advance='no') '='
            end do
        else if ((done .GT. 0) .and. (done .LT. 10)) then
            do counter = 1, done
                write(6,'(a)',advance='no') '#'
            end do
            do counter = done+1, 10
                write(6,'(a)',advance='no') '='
            end do 
        else
            do counter = 1, 10
                write(6,'(a)',advance='no') '#'
            end do
        end if
        write(6,'(a)',advance='no') '] '
        write(6,'(I3.1)',advance='no') step
        write(6,'(a)',advance='no') '%'
    end
