!__author__ = 'manmeet'
!
!! Back-Propagation Neural Networks
!!
! ainh      :     activation for inner hidden node
! hinh      :     hidden deltas
! ch        :     momentum 
! N: learning rate
! M: momentum factor

module NN
  implicit none

  type hidden
    real,allocatable,dimension(:) :: ainh
    real,allocatable,dimension(:) :: hinh 
    real,allocatable,dimension(:,:) :: winh
    real,allocatable,dimension(:,:) :: cinh
  end type hidden

integer :: ni, no, no_of_nh
integer,allocatable,dimension(:) :: nh
real,allocatable,dimension(:) :: ai, ao
real,allocatable,dimension(:,:) :: wi, wo, ci, co, inputs, targets
type(hidden),allocatable,dimension(:) :: ah, wh, ch
integer :: N, M, iterations, no_of_training, no_of_test

character(len=100) :: hidden_file 

namelist/main_nml/ni, no, no_of_nh, hidden_file, N, M, iterations, no_of_training, no_of_test

contains

    subroutine NN__init()
        ! number of input, hidden, and output nodes
    integer :: i, j
    open(10,file='input.nml')
    read(10,nml=main_nml)
    close(10)
    
    allocate(nh(no_of_nh)) ! Its location has to be this only because it is
                           ! being used in the next line 

    open(21,file=hidden_file)
    do i=1,no_of_nh
!      print *,"Check 1"
!      print *,no_of_nh
      read(21,*) nh(i)
      print *,nh(i)
    end do
    close(21)

        ni = ni + 1 ! +1 for bias node
        do i=1,no_of_nh
         nh(i) = nh(i) + 1
        end do

! Allocation 

    allocate(ai(ni))
    allocate(inputs(ni-1,no_of_training))

    allocate(ao(no))
    allocate(targets(no,no_of_training))

    allocate(ah(no_of_nh))

    do i=1,no_of_nh

      allocate(ah(i)%ainh(nh(i)))

    end do

    allocate(wi(ni,nh(1)-1))
    allocate(ci(ni,nh(1)-1))

    allocate(wo(nh(no_of_nh),no))
    allocate(co(nh(no_of_nh),no))

    allocate(wh(no_of_nh-1))
    allocate(ch(no_of_nh-1))

    do i=1,no_of_nh-1
  
      allocate(wh(i)%winh(nh(i),nh(i+1)-1))
      allocate(ch(i)%cinh(nh(i),nh(i+1)-1))

    end do

    end subroutine NN__init

    subroutine set_NN() 

      integer :: i, j
!
!        ! activations for nodes
         ai = 1.0
          do i=1,no_of_nh     
            ah(i)%ainh(:) = 1.0     
          end do
         ao = 1.0
!        print *,"Check 1"
        call random(wi,ni,nh(1)-1)
!        print *,"Check 3"
        call fill_random(wi,ni,nh(1)-1)
!        print *,"Check 2"
        call random(wo,nh(no_of_nh),no)
        call fill_random(wo,nh(no_of_nh),no)
      
        do i=1,no_of_nh-1

        call random(wh(i)%winh,nh(i),nh(i+1)-1)
        call fill_random(wh(i)%winh,nh(i),nh(i+1)-1)
    
        end do
        print *,"Random Initialization done"

        ci(:,:) = 0.0
        do i=1,no_of_nh-1
          ch(i)%cinh(:,:) = 0.0
        enddo
        co(:,:) = 0.0
        print *,"Information set in the Neural Network"

    end subroutine set_NN

    subroutine  update(inp, aout)
  
    integer :: i, j, k, l
    real :: sum
    real,dimension(:) :: inp
    real,dimension(:) :: aout
        print *,"Size of inputs,ni-1 in update = ", size(inp), ni-1
        if (size(inp) .ne. ni-1) print *,"wrong number of inputs"

        ! input activations
        ai(:) = 1.0
        do i =1,ni
            !self.ai[i] = sigmoid(inputs[i])
            ai(i) = inp(i)
        enddo

        ! input layer - hidden activations

        do j=1,nh(1)
            sum = 0.0
            do i=1,ni
                sum = sum + ai(i) * wi(i,j)
            end do
            call sigmoid(sum,ah(1)%ainh(j)) 
        end do

        ! hidden activations - hidden activations
      do k=1,no_of_nh-1
        do j=1,nh(k+1)
            sum = 0.0
            do i=1,nh(k)
                sum = sum + ah(k)%ainh(i) * wh(k)%winh(i,j)
            end do
            call sigmoid(sum,ah(k+1)%ainh(j)) 
        end do
      enddo

        ! output activations
        do l=1,no
            sum = 0.0
            do k=1,nh(no_of_nh)
                sum = sum + ah(no_of_nh)%ainh(k) * wo(k,l)
            enddo
            call sigmoid(sum,ao(l))
        enddo
!        return self.ao[:]
        aout = ao
        print *,"End of update "
    end subroutine update

    subroutine backPropagate(error,tar)
      
      real :: change
      real,dimension(:) :: tar
      real,allocatable,dimension(:) :: output_deltas
      type(hidden),allocatable,dimension(:) :: hidden_deltas
      real :: error, dsig
      integer :: i, j, k, l      
      
        if (size(tar) .ne. no) print *,"wrong number of target values"
        allocate(output_deltas(no))
        ! calculate error terms for output

        output_deltas = 0.0 
        do l=1,no
            error = tar(l) - ao(l)
            call dsigmoid(ao(l), dsig)
            output_deltas(l) = dsig * error
        enddo
   print *,"Check 1 from backpropagate" 
       ! update output weights
        do k=1,nh(no_of_nh)
            do l=1,no
                change = output_deltas(l)*ah(no_of_nh)%ainh(k)
                wo(k,l) = wo(k,l) + N*change + M*co(k,l)
                co(k,l) = change
                !print N*change, M*self.co[j][k]
            enddo
        enddo

!        deallocate(output_deltas)

        ! calculate error terms for hidden - output 
        allocate(hidden_deltas(no_of_nh))

        do i=1,no_of_nh
        
          allocate(hidden_deltas(i)%hinh(nh(i)))
        
        end do

      print *,"Check 2 from backpropagate"
      print *,"Size of output_deltas = "
!        hidden_deltas = 0.0

        do k=1,nh(no_of_nh)
            error = 0.0
            do l=1,no
                error = error + output_deltas(l)*wo(k,l)
            enddo
            call dsigmoid(ah(no_of_nh)%ainh(k),dsig)
            hidden_deltas(no_of_nh)%hinh(k) = dsig * error
        enddo
      print *,"Check 3 from backpropagate"
        ! update hidden weights
        do j=1,nh(no_of_nh-1)
            do k=1,nh(no_of_nh)
                change = hidden_deltas(no_of_nh)%hinh(k)*ah(no_of_nh-1)%ainh(j)
                wh(no_of_nh-1)%winh(j,k) = wh(no_of_nh-1)%winh(j,k) + N*change + M*ch(no_of_nh-1)%cinh(j,k)
                ch(no_of_nh-1)%cinh(j,k) = change
                !print N*change, M*self.co[j][k]
            enddo
        enddo

!---------------------------------------------------------------------------------------------------------

    do i=no_of_nh-2,1

       do k=1,nh(i+1)
            error = 0.0
            do l=1,nh(i+2)
                error = error + hidden_deltas(i+2)%hinh(l)*wh(i+1)%winh(k,l)
            enddo
            call dsigmoid(ah(i+1)%ainh(k),dsig)
            hidden_deltas(i+1)%hinh(k) = dsig * error
        enddo

        ! update hidden weights
        do j=1,nh(i)
            do k=1,nh(i+1)
                change = hidden_deltas(i+1)%hinh(k)*ah(i)%ainh(j)
                wh(i)%winh(j,k) = wh(i)%winh(j,k) + N*change + M*ch(i)%cinh(j,k)
                ch(i)%cinh(j,k) = change
                !print N*change, M*self.co[j][k]
            enddo
        enddo

    enddo

!--------------------------------------------------------------------------------------------------------

         ! calculate error terms for hidden
         ! hidden_deltas(1) = 0.0
        do j=1,nh(1)
            error = 0.0
            do k=1,nh(2)
                error = error + hidden_deltas(2)%hinh(k)*wi(j,k)
            enddo
            call dsigmoid(ah(1)%ainh(j),dsig)
            hidden_deltas(1)%hinh(j) = dsig * error

        enddo

       ! update input weights
        do i=1,ni
            do j=1,nh(1)
                change = hidden_deltas(1)%hinh(j)*ai(i)
                wi(i,j) = wi(i,j) + N*change + M*ci(i,j)
                ci(i,j) = change
            enddo
        enddo

        ! calculate error
        error = 0.0
        do k=1,size(targets)
            error = error + 0.5*(tar(k)-ao(k))**2
        enddo
! to be deallocated output_deltas and hidden_deltas
  
  end subroutine backPropagate

  subroutine train()
    integer :: i, j
    real :: err, error
    real,dimension(no) :: aout
    real,dimension(no) :: tar
    real,dimension(ni-1) :: inp
      ! N: learning rate
      ! M: momentum factor
      ! read the inputs 
      ! read the targets

      do i=1,iterations
          error = 0.0
          do j=1,no_of_training

              inp(:) = inputs(:,i)
              print *,"Size of input in train = ",size(inp)
              call update(inp,aout) ! Feed Forward
              tar(:) = targets(:,i)
              print *,"Size of targets in train = ",size(tar)
              call backPropagate(err,tar)   ! Back Propagate
              error = error + err
          enddo
          if (mod(i, 100) == 0)  print *,"error =", error
          if (error < 0.001) exit
      enddo

    end subroutine train

    subroutine test(inp,aout)
        real,dimension(:,:) ::inp
        real,dimension(:,:) :: aout
        integer :: i

        do i=1,no_of_test
            call update(inp(:,i),aout(:,i))
        enddo
!        return self.update(p[0])
    end subroutine test

    subroutine read_data()

    integer :: i, j
    real,dimension(3,4) :: fill_data
    character(len=100) :: file_data 
    file_data = "train"
    open(25,file=file_data)
    do i=1,4
!      print *,"Check 1"
!      print *,no_of_nh
      read(25,*) fill_data(:,i)
      print *,"Read data at i = ",i,fill_data(:,i)
    end do
    close(25)
    do i=1,no_of_training
      do j=1,ni-1
        inputs(j,i)  = fill_data(j,i)
      enddo
    
      do j=1,no
          targets(j,i) = fill_data(j+ni,i)
      enddo
    enddo

    print *,"inputs  = ", inputs
    print *,"targets = ", targets 
! Arrays are stored in fortran column wise as column,row
    end subroutine read_data

subroutine sigmoid(x,y)
  real :: x,y
  y = tanh(x)  
end subroutine sigmoid

subroutine dsigmoid(x,y)
  real :: x,y
  y = 1 - x**2
end subroutine dsigmoid

subroutine random(r,m,n)

integer :: m, n, i, j
real (kind=8) :: seed
real,dimension(:,:) :: r

seed = 0.5D0

!print *, seed
do i = 1, n
  do j=1,m
    r(i,j) =  conrand(seed)
    if(r(i,j).eq.0.0) r(i,j) = 0.5
  enddo
enddo

end subroutine random 

subroutine fill_random(x,m,n)

integer :: m, n, i, j
real,dimension(:,:) :: x

  do i=1,m
    do j=1,n 
      if(x(i,j).le.0.001) x(i,j) = 0.5
    enddo
  enddo

end subroutine fill_random


real (kind=8) function conrand(seed)
!
! Function to generate a sequence of random numbers.
! Adapted from the  "Minimal Standard Method, real version 1 in Pascal"
! Park, S, Miller, K. "Random Number Generators: Good Ones are
! Hard to Find".  Communications of the ACM. vol 31, number 10,
! October 1988. pp. 1192-1201.
!
! Fortran 2003 Version tested on 64 Bit Linux, gfortran compiler
! Andrew J. Pounds, Ph.D.
! Departments of Chemistry and Computer Science
! Mercer University
! Fall 2011
!
real (kind=8) :: seed
real (kind=8) :: a, m
real (kind=8) :: temp
a = 16807.0D0
m = 2147483647.0D0
temp = a*seed
seed = temp - m * int(temp/m)
conrand = seed / m
return
end function conrand

subroutine end_NN()

  deallocate(nh)
  print *,"check 2"
  deallocate(ai, ao)
  print *,"check 3"
  !deallocate(wi, wo, ci, co, inputs, targets)
  
  deallocate(wi, wo, ci, co)
  print *,"check 4"
!  deallocate(ah, wh, ch)
  print *,"check 5"

end subroutine end_NN


end module NN


