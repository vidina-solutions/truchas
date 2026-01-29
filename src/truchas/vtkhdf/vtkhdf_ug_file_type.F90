!!
!! VTKHDF_FILE_TYPE
!!
!! This module defines a derived type for exporting mesh-based solution data
!! to a VTKHDF format file that can be read by the ParaView visualizaton tool.
!! The format uses HDF5 for on-disk storage.
!!
!! Neil Carlson <neil.n.carlson@gmail.com>
!! March 2024
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! This file is part of Truchas. 3-Clause BSD license; see the LICENSE file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! NOTES
!!
!! The up-to-date specification for VTKHDF is at
!! https://docs.vtk.org/en/latest/design_documents/VTKFileFormats.html#vtkhdf-file-format
!! and is in development and evolving. The VTKHDF development road map is at
!! https://discourse.vtk.org/t/vtkhdf-roadmap/13257
!!
!! This module was written for version 2.1 of the format.
!!
!! This module uses the "UnstructuredGrid" type of format, and supports static
!! and time dependent datasets (node and cell based), but both assume a single
!! static mesh.
!!
!! This module is currently serial only: data must be collated onto a single
!! process and written from that process using this object.
!!

#include "f90_assert.fpp"

module vtkhdf_ug_file_type

  use,intrinsic :: iso_fortran_env
  use hdf5_c_binding
  use hl_hdf5
  use vtkhdf_ug_type
  implicit none
  private

  type, extends(vtkhdf_ug), public :: vtkhdf_ug_file
    private
    integer(hid_t) :: file_id
  contains
    procedure :: create
    procedure :: close
  end type

  !! The VTK cell types that are relevant to Truchas.
  !! NB: The local node ordering for a VTK wedge cell may differ from Truchas
  integer(int8), parameter, public :: VTK_TETRA = 10
  integer(int8), parameter, public :: VTK_HEXAHEDRON = 12
  integer(int8), parameter, public :: VTK_WEDGE = 13
  integer(int8), parameter, public :: VTK_PYRAMID = 14

contains

  !! Create a new VTKHDF format file, overwriting any existing file. A file
  !! supporting time-dependent data is created by default. Set the optional
  !! TEMPORAL argument to false for a static file.

  subroutine create(this, filename, stat, errmsg, temporal)

    class(vtkhdf_ug_file), intent(out) :: this
    character(*), intent(in) :: filename
    integer, intent(out) :: stat
    character(:), allocatable, intent(out) :: errmsg
    logical, intent(in), optional :: temporal

    call init_hdf5

    this%file_id = H5Fcreate(filename, H5F_ACC_TRUNC)
    if (this%file_id < 0) then
      stat = 1
      errmsg = 'h5fcreate error'  !TODO: refine msg
      return
    end if

    call this%vtkhdf_ug%init(this%file_id, 'VTKHDF', stat, errmsg, temporal)

  end subroutine

  subroutine close(this)
    class(vtkhdf_ug_file), intent(inout) :: this
    integer :: istat
    call this%vtkhdf_ug%close
    if (this%file_id > 0) this%file_id = H5Fclose(this%file_id)
  end subroutine

end module vtkhdf_ug_file_type
