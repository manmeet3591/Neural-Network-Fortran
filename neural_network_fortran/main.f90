program main
use NN

call NN__init()

call set_NN() 

!call test_random_seed()
call end_NN()
print *,ni, no
end program main


!def demo1():
!    ! Teach network OR function
!    pat = [
!        [[0,0], [0]],
!        [[0,1], [1]],
!        [[1,0], [1]]
!!        [[1,1], [1]]
!    ]
!
!    ! create a network with two input, two hidden, and one output nodes
!    n = NN(2, 6,3, 1)
!    ! train it with some patterns
!    n.train(pat)
!    ! test it
!!    n.test([[[1,1],[1]]])
!    n.test([[[1,1]]])


!def demo2():
!
!    ! Teach network XOR function
!    pat = [
!        [[0], [0]],
!        [[3.14/6], [np.sin(3.14/6)]],
!        [[3.14/3], [np.sin(3.14/3)]],
!        [[3.14/4], [np.sin(3.14/4)]]
!!        [[1,1], [1]]
!    ]
!
!    ! create a network with two input, two hidden, and one output nodes
!    n = NN(1, 4,2, 1)
!    ! train it with some patterns
!    n.train(pat)
!    ! test it
!!    n.test([ [[1,1],[1]], [[1,1],[1]]])
!    output = n.test([ [[3.14/2],[1]], [[2*3.14],[1]]])
!!    return output
!!    print(test1)
!!    n.test([[[3.14/2]]])
!!    n.test([[[2*3.14]]])
!
!
!
!if __name__ == '__main__':
!!    demo1()
!    print(demo2())
!
!

