!!
!! FDME_VTK_GRAPHICS_PROC
!!
!! This module provides a procedure for writing a vtkhdf-format graphics file
!! of the solution to the frequency-domain Maxwell equations computed by an
!! FDME_SOLVER object and associated derived quantities.
!!
!! Neil Carlson <neil.n.carlson@gmail.com>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! This file is part of Truchas. 3-Clause BSD license; see the LICENSE file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "f90_assert.fpp"

module fdme_vtk_graphics_proc

  use,intrinsic :: iso_fortran_env, only: r8 => real64, int8
  use fdme_solver_type
  use vtkhdf_file_type
  use parallel_communication
  use string_utilities, only: i_to_c
  implicit none
  private

  public :: fdme_vtk_graphics

contains

  !TODO: Give user some control over what fields are output
  subroutine fdme_vtk_graphics(solver, filename, stat, errmsg)

    type(fdme_solver), intent(in) :: solver
    character(*), intent(in) :: filename
    integer, intent(out) :: stat
    character(:), allocatable, intent(out) :: errmsg

    type(vtkhdf_file) :: viz_file
    real(r8), allocatable :: x(:,:)
    integer, allocatable :: xcnode(:), cnode(:), block_cells(:), block_nodes(:)
    integer(int8), allocatable :: types(:)
    real(r8), allocatable :: g_scalar(:), l_scalar(:)
    complex(r8), allocatable :: g_vector(:,:), l_vector(:,:), g_zscalar(:), l_zscalar(:)
    integer, allocatable :: g_iscalar(:)
    real(r8) :: q(solver%mesh%ncell)
    complex(r8) :: e(3,solver%mesh%ncell), h(3,solver%mesh%ncell), dd(solver%mesh%nnode)
    integer :: j, n, bitmask, ncell, nnode

    if (is_IOP) call viz_file%create(filename, stat, errmsg)
    call broadcast(stat)
    if (stat /= 0) then
      call broadcast_alloc_char(errmsg)
      return
    end if

    call solver%get_heat_source(q)
    call solver%get_cell_efield(e)
    call solver%get_cell_hfield(h)
    call solver%get_div_dfield(dd)

    do j = 1, size(solver%mesh%cell_set_name)
      associate (name => solver%mesh%cell_set_name(j)%s)
        if (is_IOP) call viz_file%create_block(name, stat, errmsg)
        call broadcast(stat)
        if (stat /= 0) then
          call broadcast_alloc_char(errmsg)
          return
        end if
        bitmask = ibset(0,pos=j)
        call solver%mesh%get_global_mesh_block(bitmask, x, xcnode, cnode, &
            block_cells, block_nodes)
        if (is_IOP) then
          types = spread(VTK_TETRA, dim=1, ncopies=size(xcnode)-1)
          call viz_file%write_block_mesh(name, x, cnode, xcnode, types, stat, errmsg)
        end if
        call broadcast(stat)
        INSIST(stat == 0)

        ncell = size(block_cells) ! number of local cells in block
        n = global_sum(ncell)
        allocate(g_scalar(merge(n, 0, is_IOP)))
        allocate(g_vector(3,merge(n, 0, is_IOP)))

        l_scalar = q(block_cells)
        call gather(l_scalar, g_scalar)
        if (is_IOP) call viz_file%write_cell_dataset(name, 'Q_EM', g_scalar, stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

        l_vector = e(:,block_cells)
        call gather(l_vector, g_vector)
        if (is_IOP) call viz_file%write_cell_dataset(name, 'E_re', g_vector%re, stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

#ifdef GNU_PR117774
        if (is_IOP) call viz_file%write_cell_dataset(name, 'E_im', reshape([g_vector%im],shape(g_vector)), stat, errmsg)
#else
        if (is_IOP) call viz_file%write_cell_dataset(name, 'E_im', g_vector%im, stat, errmsg)
#endif
        call broadcast(stat)
        INSIST(stat == 0)

        if (is_IOP) call viz_file%write_cell_dataset(name, '|E|', abs(g_vector), stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

        l_vector = h(:,block_cells)
        call gather(l_vector, g_vector)
        if (is_IOP) call viz_file%write_cell_dataset(name, 'H_re', g_vector%re, stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

#ifdef GNU_PR117774
        if (is_IOP) call viz_file%write_cell_dataset(name, 'H_im', reshape([g_vector%im],shape(g_vector)), stat, errmsg)
#else
        if (is_IOP) call viz_file%write_cell_dataset(name, 'H_im', g_vector%im, stat, errmsg)
#endif
        call broadcast(stat)
        INSIST(stat == 0)

        if (is_IOP) call viz_file%write_cell_dataset(name, '|H|', abs(g_vector), stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

        !! Output the mesh partition
        call gather(spread(real(this_PE,kind=r8), dim=1, ncopies=ncell), g_scalar)
        if (is_IOP) call viz_file%write_cell_dataset(name, 'MPI rank', g_scalar, stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

        l_zscalar = dd(block_nodes)
        n = global_sum(size(l_zscalar))
        allocate(g_zscalar(merge(n, 0, is_IOP)))
        call gather(l_zscalar, g_zscalar)
        if (is_IOP) call viz_file%write_point_dataset(name, 'div_D_re', g_zscalar%re, stat, errmsg)
        call broadcast(stat)
#ifdef GNU_PR117774
        if (is_IOP) call viz_file%write_point_dataset(name, 'div_D_im', [g_zscalar%im], stat, errmsg)
#else
        if (is_IOP) call viz_file%write_point_dataset(name, 'div_D_im', g_zscalar%im, stat, errmsg)
#endif
        call broadcast(stat)
        INSIST(stat == 0)

        if (is_IOP) call viz_file%write_point_dataset(name, '|div_D|', abs(g_zscalar), stat, errmsg)
        call broadcast(stat)
        INSIST(stat == 0)

        deallocate(g_scalar, g_vector, g_zscalar)
      end associate
    end do

    if (is_IOP) call viz_file%close

  end subroutine fdme_vtk_graphics

end module fdme_vtk_graphics_proc
