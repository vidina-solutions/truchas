program vtkhdf_demo

  use,intrinsic :: iso_fortran_env, only: r8 => real64, int8
  use vtkhdf_file_type
  implicit none

  type(vtkhdf_file) :: vizfile
  integer :: stat, j
  character(:), allocatable :: errmsg, name(:)
  real(r8), allocatable :: x(:,:)
  integer,  allocatable :: cnode(:), xcnode(:)
  integer(int8), allocatable :: types(:)
  real(r8), allocatable :: scalar_cell_data(:), vector_cell_data(:,:)
  real(r8), allocatable :: scalar_point_data(:), vector_point_data(:,:)
  real(r8) :: vec_mold(3,0), sca_mold(0)


  call vizfile%create('demo.vtkhdf', stat, errmsg)
  if (stat /= 0) error stop errmsg

  !! The MultiBlockDataSet VTK mesh will consist of two blocks.
  name = ['part1', 'part2']

  !! The unstructured mesh data for one mesh block; shift it for the second.
  call get_mesh_data(x, cnode, xcnode, types)

  !! Create the mesh blocks and export the mesh for each.
  !! NB: Not all blocks (or any) need to be temporal, but choose to do so here.
  do j = 1, 2
    call vizfile%create_block(name(j), stat, errmsg, temporal=.true.)
    if (stat /= 0) error stop errmsg

    call vizfile%write_block_mesh(name(j), x+(j-1), cnode, xcnode, types, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end do

  !!!! Register the datasets that evolve with time.

  do j = 1, 2
    call vizfile%register_temporal_cell_dataset(name(j), 'cell-radius', sca_mold, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%register_temporal_cell_dataset(name(j), 'cell-velocity', vec_mold, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%register_temporal_point_dataset(name(j), 'point-radius', sca_mold, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%register_temporal_point_dataset(name(j), 'point-velocity', vec_mold, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end do

  !!!! Write the datasets for the first time step !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call vizfile%write_time_step(0.0_r8)

  do j = 1, 2
    !! Generate example datasets for the block.
    call get_scalar_cell_data(x+(j-1), cnode, xcnode, scalar_cell_data)
    call get_vector_cell_data(x+(j-1), cnode, xcnode, vector_cell_data)
    call get_scalar_point_data(x+(j-1), scalar_point_data)
    call get_vector_point_data(x+(j-1), vector_point_data)

    call vizfile%write_temporal_cell_dataset(name(j), 'cell-radius', scalar_cell_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_temporal_cell_dataset(name(j), 'cell-velocity', vector_cell_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_temporal_point_dataset(name(j), 'point-radius', scalar_point_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_temporal_point_dataset(name(j), 'point-velocity', vector_point_data, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end do

  !!!! Write the datasets for the second time step !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call vizfile%write_time_step(10.0_r8)

  do j = 1, 2
    !! Generate example datasets for the block.
    call get_scalar_cell_data(x+(j-1), cnode, xcnode, scalar_cell_data)
    call get_vector_cell_data(x+(j-1), cnode, xcnode, vector_cell_data)
    call get_scalar_point_data(x+(j-1), scalar_point_data)
    call get_vector_point_data(x+(j-1), vector_point_data)

    call vizfile%write_temporal_cell_dataset(name(j), 'cell-radius', 1+scalar_cell_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_temporal_cell_dataset(name(j), 'cell-velocity', 1+vector_cell_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_temporal_point_dataset(name(j), 'point-radius', 1+scalar_point_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_temporal_point_dataset(name(j), 'point-velocity', 1+vector_point_data, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end do

  !! At any point you can write a dataset that isn't time dependent, but its name must
  !! be unique from any other dataset, temporal or not, of the same type (cell or point).

  do j = 1, 2
    !! Generate example datasets for the block.
    call get_scalar_cell_data(x+(j-1), cnode, xcnode, scalar_cell_data)
    call get_vector_cell_data(x+(j-1), cnode, xcnode, vector_cell_data)
    call get_scalar_point_data(x+(j-1), scalar_point_data)
    call get_vector_point_data(x+(j-1), vector_point_data)

    call vizfile%write_cell_dataset(name(j), 'static-cell-scalar', -scalar_cell_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_cell_dataset(name(j), 'static-cell-vector', -vector_cell_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_point_dataset(name(j), 'static-point-scalar', -scalar_point_data, stat, errmsg)
    if (stat /= 0) error stop errmsg

    call vizfile%write_point_dataset(name(j), 'static-point-vector', -vector_point_data, stat, errmsg)
    if (stat /= 0) error stop errmsg
  end do

  call vizfile%close

contains

  ! A 5-tet subdivision of the squished unit cube.
  subroutine get_mesh_data(x, cnode, xcnode, types)
    real(r8), allocatable, intent(out) :: x(:,:)
    integer, allocatable, intent(out) :: cnode(:), xcnode(:)
    integer(int8), allocatable :: types(:)
    x = 0.5_r8*reshape([0,0,0, 1,0,0, 1,1,0, 0,1,0, 0,0,1, 1,0,1, 1,1,1, 0,1,1], shape=[3,8])
    ! distort to catch C/Fortran index ordering errors
    x(2,:) = 0.75_r8*x(2,:)
    x(3,:) = 1.25_r8*x(3,:)
    cnode = [1,2,4,5, 2,3,4,7, 2,5,6,7, 4,5,7,8, 2,4,5,7]
    xcnode = [1,5,9,13,17,21]
    types = [10, 10, 10, 10, 10] ! all tets
  end subroutine

  subroutine get_scalar_point_data(x, pdata)
    real(r8), intent(in) :: x(:,:)
    real(r8), allocatable, intent(out) :: pdata(:)
    integer :: j
    allocate(pdata(size(x,dim=2)))
    do j = 1, size(x,dim=2)
      pdata(j) = norm2(x(:,j))
    end do
  end subroutine

  subroutine get_vector_point_data(x, pdata)
    real(r8), intent(in) :: x(:,:)
    real(r8), allocatable, intent(out) :: pdata(:,:)
    integer :: j
    pdata = x
  end subroutine

  subroutine get_scalar_cell_data(x, cnode, xcnode, cdata)
    real(r8), intent(in) :: x(:,:)
    integer, intent(in) :: cnode(:), xcnode(:)
    real(r8), allocatable, intent(out) :: cdata(:)
    integer :: j
    allocate(cdata(size(xcnode)-1))
    do j = 1, size(cdata)
      associate(pid => cnode(xcnode(j):xcnode(j+1)-1))
        cdata(j) = norm2(sum(x(:,pid),dim=2)/size(pid))
      end associate
    end do
  end subroutine

  subroutine get_vector_cell_data(x, cnode, xcnode, cdata)
    real(r8), intent(in) :: x(:,:)
    integer, intent(in) :: cnode(:), xcnode(:)
    real(r8), allocatable, intent(out) :: cdata(:,:)
    integer :: j
    allocate(cdata(size(x,dim=1),size(xcnode)-1))
    do j = 1, size(cdata,dim=2)
      associate(pid => cnode(xcnode(j):xcnode(j+1)-1))
        cdata(:,j) = sum(x(:,pid),dim=2)/size(pid)
      end associate
    end do
  end subroutine

end program
