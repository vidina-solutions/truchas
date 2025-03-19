!!
!! MESH_MANAGER
!!
!! Neil N. Carlson <nnc@lanl.gov>
!! July 2015
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! This file is part of Truchas. 3-Clause BSD license; see the LICENSE file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "f90_assert.fpp"

module mesh_manager

  use parameter_list_type
  use truchas_logging_services
  use simpl_mesh_type
  implicit none
  private

  public :: enable_mesh, named_mesh_ptr, unstr_mesh_ptr, simpl_mesh_ptr
  public :: read_truchas_mesh_namelists, init_mesh_manager

  public :: simpl_mesh ! re-export; do not want this -- FIXME

  interface init_mesh_manager
    module procedure init_mesh_manager, init_mesh_manager_params
  end interface

  type(parameter_list), target, save :: meshes

  !! The value of the 'mesh' parameter will be of this private type.
  !! A PARAMETER_LIST stores (shallow) copies of parameter values, and thus
  !! cannot properly store a mesh object.  Instead we have it store a pointer
  !! to a mesh object by boxing the pointer in a TYPE(ANY_MESH) variable and
  !! and storing that variable.
  type :: any_mesh
    class(*), pointer :: mesh => null()
  end type any_mesh

contains

  !! NNC, Jun 2014.  A new initialization routine that takes a parameter list
  !! to specify the mesh files and parameters.  This is an alternative to
  !! using READ_MESH_NAMELISTS to read the data from an input file, and was
  !! added to simplify the development of unit tests requiring a mesh.  The
  !! expected syntax of the parameter list is a list of sublists.  The name
  !! of a sublist is taken as the name of the mesh, and the sublist has the
  !! following parameters:
  !!
  !!                 'mesh-file' : string (required)
  !!        'coord-scale-factor' : real-scalar (optional; default 1.0)
  !!    'interface-side-set-ids' : integer-array (optional)

  subroutine init_mesh_manager_params (params)

    use,intrinsic :: iso_fortran_env, only: r8 => real64
    use string_utilities, only: raise_case

    type(parameter_list) :: params

    type(parameter_list_iterator) :: piter
    type(parameter_list), pointer :: plist1, plist2
    integer  :: exodus_block_modulus
    real(r8) :: coord_scale_factor
    character(:), allocatable :: mesh_file
    integer, allocatable :: side_sets(:)

    !! Copy the input parameter list to the modules private parameter list.
    !! We want case-insensitive mesh names in the module, so we convert them
    !! to upper case when copying; later searching will use the upper-cased
    !! version of the specified name.
    piter = parameter_list_iterator(params, sublists_only=.true.)
    do while (.not.piter%at_end())
      plist1 => piter%sublist()
      plist2 => meshes%sublist(raise_case(piter%name()))
      call plist1%get ('mesh-file', mesh_file)
      call plist2%set ('mesh-file', mesh_file)
      call plist2%set ('enabled', .true.)

      if (plist1%is_parameter('coord-scale-factor')) then
        call plist1%get ('coord-scale-factor', coord_scale_factor)
        call plist2%set ('coord-scale-factor', coord_scale_factor)
      end if

      if (plist1%is_parameter('interface-side-set-ids')) then
        call plist1%get ('interface-side-set-ids', side_sets)
        call plist2%set ('interface-side-set-ids', side_sets)
      end if

      if (plist1%is_parameter('exodus-block-modulus')) then
        call plist1%get ('exodus-block-modulus', exodus_block_modulus)
        call plist2%set ('exodus-block-modulus', exodus_block_modulus)
      end if

      call piter%next
    end do

    !! Now call the original initialization procedure.
    call init_mesh_manager

  end subroutine init_mesh_manager_params

  subroutine init_mesh_manager

    use truchas_timers

    integer :: stat
    logical :: enabled
    character(:), allocatable :: errmsg, mesh_type
    type(parameter_list), pointer :: plist
    type(parameter_list_iterator) :: piter
#if defined(GNU_PR49213)
    type(any_mesh) :: box
#endif

    piter = parameter_list_iterator(meshes, sublists_only=.true.)
    do while (.not.piter%at_end())
      plist => piter%sublist()
      call plist%get('enabled', enabled, default=.false.)
      if (enabled) then
        call start_timer('mesh-'//piter%name())
        call TLS_info('')
        call TLS_info('Initializing mesh "' // piter%name() // '" ...')
        call plist%get('mesh-type', mesh_type, default='unstructured')
        select case (mesh_type)
        case ('unstructured')
          block
            use unstr_mesh_factory
            type(unstr_mesh), pointer :: mesh
            mesh => new_unstr_mesh(plist, stat, errmsg)
            if (stat /= 0) call TLS_fatal(errmsg)
#if defined(GNU_PR49213)
            box%mesh => mesh
            call plist%set('mesh', box)
#else
            call plist%set('mesh', any_mesh(mesh))
#endif
            if (TLS_verbosity >= TLS_VERB_NOISY) call mesh%write_profile
            call mesh%check_bndry_face_set
          end block
        case ('simplicial', 'electromagnetics')
          block
            use simpl_mesh_factory
            type(simpl_mesh), pointer :: mesh
            mesh => new_simpl_mesh(plist, stat, errmsg)
            if (stat /= 0) call TLS_fatal(errmsg)
#if defined(GNU_PR49213)
            box%mesh => mesh
            call plist%set('mesh', box)
#else
            call plist%set('mesh', any_mesh(mesh))
#endif
            if (TLS_verbosity >= TLS_VERB_NOISY) call mesh%write_profile
          end block
        case default
          call TLS_fatal('unknown mesh type: ' // mesh_type)
        end select
        call TLS_info ('  mesh "' // trim(piter%name()) // '" initialized', TLS_VERB_NOISY)
        call stop_timer('mesh-'//piter%name())
      end if
      call piter%next
    end do

  end subroutine init_mesh_manager

  !! Read the MESH namelists to populate the private MESHES parameter list.
  subroutine read_truchas_mesh_namelists(lun)
    use mesh_namelist, only: read_mesh_namelists
    integer, intent(in) :: lun
    type(parameter_list), pointer :: plist
    call read_mesh_namelists(lun, meshes)
    ! The MAIN mesh is always enabled
    INSIST(meshes%is_sublist('MAIN'))
    plist => meshes%sublist('MAIN')
    call plist%set('enabled', .true.)
  end subroutine

  !! This auxiliary procedure reads the MESH namelist and stuffs the results
  !! into the passed parameter list PARAMS.

  subroutine enable_mesh(name, exists)
    character(*), intent(in) :: name
    logical, intent(out) :: exists
    type(parameter_list), pointer :: plist
    exists = meshes%is_sublist(name)
    if (exists) then
      plist => meshes%sublist(name)
      call plist%set('enabled', .true.)
    end if
  end subroutine

  !! Returns a pointer to the CLASS(BASE_MESH) mesh object that corresponds to
  !! NAME. A null pointer is returned if NAME is not recognized or if the mesh
  !! that corresponds to NAME is not of class BASE_MESH.
  function named_mesh_ptr(name)
    use base_mesh_class
    character(*), intent(in) :: name
    class(base_mesh), pointer :: named_mesh_ptr
    class(*), pointer :: csptr
    csptr => mesh_ptr(name)
    named_mesh_ptr => null()
    if (.not.associated(csptr)) return
    select type (csptr)
    class is (base_mesh)
      named_mesh_ptr => csptr
    end select
  end function

  !! Returns a pointer to the TYPE(UNSTR_MESH) mesh object that corresponds to
  !! NAME. A null pointer is returned if NAME is not recognized or if the mesh
  !! that corresponds to NAME is not of type UNSTR_MESH.
  function unstr_mesh_ptr(name)
    use unstr_mesh_type
    character(*), intent(in) :: name
    type(unstr_mesh), pointer :: unstr_mesh_ptr
    class(*), pointer :: csptr
    csptr => mesh_ptr(name)
    unstr_mesh_ptr => null()
    if (.not.associated(csptr)) return
    select type (csptr)
    class is (unstr_mesh)
      unstr_mesh_ptr => csptr
    end select
  end function

  !! Returns a pointer to the TYPE(SIMPL_MESH) mesh object that corresponds to
  !! NAME. A null pointer is returned if NAME is not recognized or if the mesh
  !! that corresponds to NAME is not of type SIMPL_MESH.
  function simpl_mesh_ptr (name)
    use simpl_mesh_type
    character(*), intent(in) :: name
    type(simpl_mesh), pointer :: simpl_mesh_ptr
    class(*), pointer :: csptr
    csptr => mesh_ptr(name)
    simpl_mesh_ptr => null()
    if (.not.associated(csptr)) return
    select type (csptr)
    class is (simpl_mesh)
      simpl_mesh_ptr => csptr
    end select
  end function

  !! Auxiliary function returns a CLASS(*) pointer to the arbitrary mesh object
  !! that corresponds to NAME.  NAME is case-insensitive.  A null pointer is
  !! returned if NAME is not recognized.
  function mesh_ptr(name)
    character(*), intent(in) :: name
    class(*), pointer :: mesh_ptr
    type(parameter_list), pointer :: plist
    class(*), allocatable :: cs
    mesh_ptr => null()
    if (meshes%is_sublist(name)) then
      plist => meshes%sublist(name)
      call plist%get_any ('mesh', cs)
      select type (cs)  ! unbox the mesh pointer
      type is (any_mesh)
        mesh_ptr => cs%mesh
      class default
        INSIST(.false.)
      end select
    end if
  end function

end module mesh_manager
