!!
!! EXODUS_C_BINDING
!!
!! Bindings to a subset of the ExodusII C library [1].
!!
!! Neil N. Carlson <nnc@lanl.gov>
!! January 2014; updated February 2015.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! This file is part of Truchas. 3-Clause BSD license; see the LICENSE file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! NOTES
!!
!!  The parameter settings below must match those from the exodusII.h header
!!  file for the version of the library this will use.  These values differ
!!  between versions 5.x and 6.x.
!!
!!  Bindings to just a very small of the Exodus C library are defined; only
!!  what is required to read and write the mesh data used by Truchas/Pececillo.
!!
!!  I've used the Exodus II document (2009 update) as the reference, but a
!!  quick scan of the library source (version 6) suggests that it is out of
!!  date (though not necessarily incorrect).  In particular it appears there
!!  is the possibility of using 64-bit integers for various entity types.
!!  Only a 32-bit integer (C int type) interface is documented, and that's
!!  what I've assumed here. This is a likely place of breakage for this module.
!!
!!  The documented ex_open function is actually a preprocessor macro that
!!  delegates to the auxillary function ex_open_int, passing an additional
!!  constant EX_API_VERS_NODOT defined by the exodusII.h header file.  For
!!  this Fortran binding, exo_open is defined as a module procedure.  The
!!  value of the parameter EX_API_VERS_NODOT must match the header file value
!!  used in the Exodus II library.  Similar situation holds for ex_create.
!!
!! [1] L.A.Schoof and V.R.Yarberry, "Exodus II: A Finite Element Data Model",
!!     Sandia report SAND92-2137.  This can be obtained at
!!     http://endo.sandia.gov/SEACAS/Documentation/exodusII.pdf
!!

module exodus_c_binding

  use,intrinsic :: iso_c_binding
  implicit none
  private

  !! Functions (see Exodus II manual)
  public :: ex_open                 ! open Exodus II file
  public :: ex_create               ! create Exodus II file
  public :: ex_close                ! close Exodus II file
  public :: ex_get_init             ! read initialization parameters
  public :: ex_put_init             ! write initialization parameters
  public :: ex_get_coord            ! read nodal coordinates
  public :: ex_put_coord            ! write nodal coordinates
  public :: ex_get_elem_blk_ids     ! read element block IDs
  public :: ex_get_elem_block       ! read element block parameters
  public :: ex_put_elem_block       ! write element block parameters
  public :: ex_get_elem_conn        ! read element block connectivity
  public :: ex_put_elem_conn        ! write element block connectivity
  public :: ex_get_node_set_ids     ! read node set IDs
  public :: ex_get_node_set_param   ! read node set parameteters
  public :: ex_put_node_set_param   ! write node set parameteters
  public :: ex_get_node_set         ! read node set
  public :: ex_put_node_set         ! write node set
  public :: ex_get_side_set_ids     ! read side set IDs
  public :: ex_get_side_set_param   ! read side set parameteters
  public :: ex_put_side_set_param   ! write side set parameteters
  public :: ex_get_side_set         ! read side set
  public :: ex_put_side_set         ! write side set
  public :: ex_put_qa               ! write QA records
  public :: ex_get_names            ! get names for various entity types
  public :: ex_set_option           ! set value of option
  public :: ex_inquire_int          ! get value of option

  !! Parameters from exodusII.h (version 8.25)
  integer(c_int), parameter :: EX_API_VERS_NODOT = 825 ! MUST MATCH THE INSTALLED LIBRARY

  integer, parameter, public :: MAX_STR_LENGTH  = 32
  integer, parameter, public :: MAX_LINE_LENGTH = 80
  integer, parameter, public :: MAX_NAME_LENGTH = MAX_STR_LENGTH ! default; settable at runtime


  integer(c_int), parameter, public :: EX_READ  = int(z'2')
  integer(c_int), parameter, public :: EX_WRITE = int(z'1')
  integer(c_int), parameter, public :: EX_NOCLOBBER    =  int(z'4')
  integer(c_int), parameter, public :: EX_CLOBBER      =  int(z'8')
  integer(c_int), parameter, public :: EX_NORMAL_MODEL =  int(z'10')
  integer(c_int), parameter, public :: EX_64BIT_OFFSET =  int(z'20')
  integer(c_int), parameter, public :: EX_LARGE_MODEL  =  EX_64BIT_OFFSET
  integer(c_int), parameter, public :: EX_64BIT_DATA   =  int(z'400000')
  integer(c_int), parameter, public :: EX_NETCDF4      =  int(z'40')
  integer(c_int), parameter, public :: EX_NOSHARE      =  int(z'80')
  integer(c_int), parameter, public :: EX_SHARE        =  int(z'100')
  integer(c_int), parameter, public :: EX_NOCLASSIC    =  int(z'200')

  enum, bind(c) ! ex_entity_type
    enumerator :: EX_ELEM_BLOCK = 1
    enumerator :: EX_NODE_SET   = 2
    enumerator :: EX_SIDE_SET   = 3
  end enum
  public :: EX_ELEM_BLOCK, EX_NODE_SET, EX_SIDE_SET

  enum, bind(c) ! ex_option_type
    enumerator :: EX_OPT_MAX_NAME_LENGTH=1
  end enum
  public :: EX_OPT_MAX_NAME_LENGTH

  enum, bind(c) ! ex_inquiry
    enumerator :: EX_INQ_DB_MAX_USED_NAME_LENGTH = 49
    enumerator :: EX_INQ_MAX_READ_NAME_LENGTH = 50
  end enum
  public :: EX_INQ_DB_MAX_USED_NAME_LENGTH, EX_INQ_MAX_READ_NAME_LENGTH

  !! Default is MAX_STR_LENGTH, but can be set at run time

  interface
    function ex_open_int(path, mode, comp_ws, io_ws, version, run_version) &
        result(exoid) bind(c)
      import c_int, c_float, c_char
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value, intent(in) :: mode
      integer(c_int), intent(inout) :: comp_ws, io_ws
      real(c_float), intent(out) :: version
      integer(c_int), value, intent(in) :: run_version
      integer(c_int) :: exoid
    end function
  end interface

  interface
    function ex_create_int(path, cmode, comp_ws, io_ws, run_version) &
        result(exoid) bind(c)
      import c_int, c_float, c_char
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value, intent(in) :: cmode
      integer(c_int), intent(inout) :: comp_ws, io_ws
      integer(c_int), value, intent(in) :: run_version
      integer(c_int) :: exoid
    end function
  end interface

  interface
    function ex_close(exoid) result(error) bind(c)
      import c_int
      integer(c_int), value, intent(in) :: exoid
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_qa (exoid, num_qa_rec, qa_record) result(error) bind(c)
      import c_int, c_ptr
      integer(c_int), value, intent(in) :: exoid, num_qa_rec
      type(c_ptr), intent(in) :: qa_record(4,*)
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_init (exoid, title, num_dim, num_nodes, num_elem, &
        num_elem_blk, num_node_sets, num_side_sets) result(error) bind(c)
      import c_int, c_char
      integer(c_int), value, intent(in) :: exoid
      character(kind=c_char), intent(out) :: title(*) ! length <= MAX_LINE_LENGTH
      integer(c_int), intent(out) :: num_dim, num_nodes, num_elem, num_elem_blk
      integer(c_int), intent(out) :: num_node_sets, num_side_sets
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_init_c (exoid, title, num_dim, num_nodes, num_elem, &
        num_elem_blk, num_node_sets, num_side_sets) result(error) bind(c, name="ex_put_init")
      import c_int, c_char, c_int64_t
      integer(c_int),  value, intent(in) :: exoid
      character(kind=c_char), intent(in) :: title(*) ! length <= MAX_LINE_LENGTH
      integer(c_int64_t),  value, intent(in) :: num_dim, num_nodes, num_elem, num_elem_blk
      integer(c_int64_t),  value, intent(in) :: num_node_sets, num_side_sets
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_elem_blk_ids(exoid, elem_blk_ids) result(error) bind(c)
      import c_int
      integer(c_int), value, intent(in) :: exoid
      integer(c_int), intent(out) :: elem_blk_ids(*)
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_elem_block_c(exoid, elem_blk_id, elem_type, &
        num_elem_this_blk, num_nodes_per_elem, num_attr) result(error) bind(c, name="ex_get_elem_block")
      import c_int, c_char, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: elem_blk_id
      character(kind=c_char), intent(out) :: elem_type(*) ! length <= MAX_STR_LENGTH
      integer(c_int), intent(out) :: num_elem_this_blk, num_nodes_per_elem, num_attr
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_elem_block_c(exoid, elem_blk_id, elem_type, &
        num_elem_this_blk, num_nodes_per_elem, num_attr) result(error) bind(c, name="ex_put_elem_block")
      import c_int, c_char, c_int64_t
      integer(c_int),  value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: elem_blk_id
      character(kind=c_char), intent(in) :: elem_type(*) ! length <= MAX_STR_LENGTH
      integer(c_int64_t),  value, intent(in) :: num_elem_this_blk, num_nodes_per_elem, num_attr
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_side_set_ids(exoid, side_set_ids) result(error) bind(c)
      import c_int
      integer(c_int), value, intent(in) :: exoid
      integer(c_int), intent(out) :: side_set_ids(*)
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_side_set_param_c(exoid, side_set_id, num_side_in_set, &
        num_dist_fact_in_set) result(error) bind(c, name="ex_get_side_set_param")
      import c_int, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: side_set_id
      integer(c_int), intent(out) :: num_side_in_set, num_dist_fact_in_set
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_side_set_param_c(exoid, side_set_id, num_side_in_set, &
        num_dist_fact_in_set) result(error) bind(c, name="ex_put_side_set_param")
      import c_int, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: side_set_id, num_side_in_set, num_dist_fact_in_set
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_side_set_c(exoid, side_set_id, side_set_elem_list, &
        side_set_side_list) result(error) bind(c, name="ex_get_side_set")
      import c_int, c_ptr, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: side_set_id
      type(c_ptr), value :: side_set_elem_list, side_set_side_list
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_side_set_c(exoid, side_set_id, side_set_elem_list, &
        side_set_side_list) result(error) bind(c, name="ex_put_side_set")
      import c_int, c_ptr, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: side_set_id
      type(c_ptr), value :: side_set_elem_list, side_set_side_list
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_node_set_ids(exoid, node_set_ids) result(error) bind(c)
      import c_int
      integer(c_int), value, intent(in) :: exoid
      integer(c_int), intent(out) :: node_set_ids(*)
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_node_set_param_c(exoid, node_set_id, num_node_in_set, &
        num_df_in_set) result(error) bind(c, name="ex_get_node_set_param")
      import c_int, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: node_set_id
      integer(c_int), intent(out) :: num_node_in_set, num_df_in_set
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_node_set_param_c(exoid, node_set_id, num_node_in_set, &
        num_df_in_set) result(error) bind(c, name="ex_put_node_set_param")
      import c_int, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: node_set_id
      integer(c_int64_t), value, intent(in) :: num_node_in_set, num_df_in_set
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_node_set_c(exoid, node_set_id, node_set_node_list) &
        result(error) bind(c, name="ex_get_node_set")
      import c_int, c_ptr, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: node_set_id
      type(c_ptr), value :: node_set_node_list
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_node_set_c(exoid, node_set_id, node_set_node_list) &
        result(error) bind(c, name="ex_put_node_set")
      import c_int, c_ptr, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: node_set_id
      type(c_ptr), value :: node_set_node_list
      integer(c_int) :: error
    end function
  end interface

  interface
     function ex_get_elem_conn_c(exoid, elem_blk_id, connect) &
          result(error) bind(c, name="ex_get_elem_conn")
      import c_int, c_ptr, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: elem_blk_id
      type(c_ptr), value :: connect
      integer(c_int) :: error
    end function
  end interface

  interface
     function ex_put_elem_conn_c(exoid, elem_blk_id, connect) &
          result(error) bind(c, name="ex_put_elem_conn")
      import c_int, c_ptr, c_int64_t
      integer(c_int), value, intent(in) :: exoid
      integer(c_int64_t), value, intent(in) :: elem_blk_id
      type(c_ptr), value :: connect
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_coord(exoid, x_coor, y_coor, z_coor) result(error) bind(c)
      import c_int, c_ptr
      integer(c_int), value, intent(in) :: exoid
      type(c_ptr), value :: x_coor, y_coor, z_coor
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_put_coord(exoid, x_coor, y_coor, z_coor) result(error) bind(c)
      import c_int, c_ptr
      integer(c_int), value, intent(in) :: exoid
      type(c_ptr), value :: x_coor, y_coor, z_coor
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_get_names_c(exoid, ex_entity_type, names) &
        result(error) bind(c,name='ex_get_names')
      import c_int, c_ptr
      integer(c_int), value :: exoid, ex_entity_type
      type(c_ptr) :: names(*)
      integer(c_int) :: error
    end function
  end interface

  interface
    function ex_set_option(exoid, option, value) result(error) bind(c)
      import c_int
      integer(c_int), value :: exoid, option, value
      integer(c_int) :: error
    end function
    function ex_inquire_int(exoid, req_info) result(value) bind(c)
      import c_int
      integer(c_int), value :: exoid, req_info
      integer(c_int) :: value
    end function
  end interface

  interface ! for internal use
    function strnlen_c(s, maxlen) result(n) bind(c,name='strnlen')
      import c_ptr, c_size_t
      type(c_ptr), value :: s
      integer(c_size_t), value :: maxlen
      integer(c_size_t) :: n
    end function
  end interface

contains

  !! EX_OPEN is actually defined as a macro in exodusII.h.
  function ex_open (path, mode, comp_ws, io_ws, version) result(exoid) bind(c)
    character(kind=c_char), intent(in) :: path(*)
    integer(c_int), value, intent(in) :: mode
    integer(c_int), intent(inout) :: comp_ws, io_ws
    real(c_float), intent(out) :: version
    integer(c_int) :: exoid
    exoid = ex_open_int(path, mode, comp_ws, io_ws, version, EX_API_VERS_NODOT)
  end function ex_open

  !! EX_CREATE is actually defined as a macro in exodusII.h.
  function ex_create (path, mode, comp_ws, io_ws) result(exoid) bind(c)
    character(kind=c_char), intent(in) :: path(*)
    integer(c_int), value, intent(in) :: mode
    integer(c_int), intent(inout) :: comp_ws, io_ws
    integer(c_int) :: exoid
    exoid = ex_create_int(path, mode, comp_ws, io_ws, EX_API_VERS_NODOT)
  end function ex_create

  !! Exodus uses c_int for some aspects of the get routines and c_int64_t for
  !! others.  These instances are by-value so we can use a simple conversion
  !! function
  function as64 (x) result (y)
    integer(c_int), intent(in) :: x
    integer(c_int64_t) :: y
    y = x
  end function as64

  function ex_put_init (exoid, title, num_dim, num_nodes, num_elem, &
       num_elem_blk, num_node_sets, num_side_sets) result(error)
    integer(c_int),  value, intent(in) :: exoid
    character(kind=c_char), intent(in) :: title(*) ! length <= MAX_LINE_LENGTH
    integer(c_int),  value, intent(in) :: num_dim, num_nodes, num_elem, num_elem_blk
    integer(c_int),  value, intent(in) :: num_node_sets, num_side_sets
    integer(c_int) :: error
    error = ex_put_init_c(exoid, title, as64(num_dim), as64(num_nodes), as64(num_elem), &
         as64(num_elem_blk), as64(num_node_sets), as64(num_side_sets))
  end function ex_put_init

  function ex_get_elem_block(exoid, elem_blk_id, elem_type, &
       num_elem_this_blk, num_nodes_per_elem, num_attr) result(error)

    integer(c_int), value, intent(in) :: exoid
    integer(c_int), value, intent(in) :: elem_blk_id
    character(kind=c_char), intent(out) :: elem_type(*) ! length <= MAX_STR_LENGTH
    integer(c_int), intent(out) :: num_elem_this_blk, num_nodes_per_elem, num_attr
    integer(c_int) :: error
    error = ex_get_elem_block_c(exoid, as64(elem_blk_id), elem_type, num_elem_this_blk, &
         num_nodes_per_elem, num_attr)
  end function ex_get_elem_block

  function ex_put_elem_block(exoid, elem_blk_id, elem_type, &
       num_elem_this_blk, num_nodes_per_elem, num_attr) result(error)

    integer(c_int),  value, intent(in) :: exoid, elem_blk_id
    character(kind=c_char), intent(in) :: elem_type(*) ! length <= MAX_STR_LENGTH
    integer(c_int),  value, intent(in) :: num_elem_this_blk, num_nodes_per_elem, num_attr
    integer(c_int) :: error
    error = ex_put_elem_block_c(exoid, as64(elem_blk_id), elem_type, &
         as64(num_elem_this_blk), as64(num_nodes_per_elem), as64(num_attr))
  end function ex_put_elem_block

  function ex_get_side_set_param(exoid, side_set_id, num_side_in_set, &
       num_dist_fact_in_set) result(error)

    integer(c_int), value, intent(in) :: exoid, side_set_id
    integer(c_int), intent(out) :: num_side_in_set, num_dist_fact_in_set
    integer(c_int) :: error
    error = ex_get_side_set_param_c(exoid, as64(side_set_id), num_side_in_set, &
         num_dist_fact_in_set)
  end function ex_get_side_set_param

  function ex_put_side_set_param(exoid, side_set_id, num_side_in_set, &
       num_dist_fact_in_set) result(error)

    integer(c_int), value, intent(in) :: exoid
    integer(c_int), value, intent(in) :: side_set_id, num_side_in_set, num_dist_fact_in_set
    integer(c_int) :: error
    error = ex_put_side_set_param_c(exoid, as64(side_set_id), as64(num_side_in_set), &
         as64(num_dist_fact_in_set))
  end function ex_put_side_set_param


  function ex_get_side_set(exoid, side_set_id, side_set_elem_list, &
       side_set_side_list) result(error)

    integer(c_int), value, intent(in) :: exoid, side_set_id
    type(c_ptr), value :: side_set_elem_list, side_set_side_list
    integer(c_int) :: error
    error = ex_get_side_set_c(exoid, as64(side_set_id), side_set_elem_list, &
         side_set_side_list)
  end function ex_get_side_set


  function ex_put_side_set(exoid, side_set_id, side_set_elem_list, &
       side_set_side_list) result(error)

    integer(c_int), value, intent(in) :: exoid, side_set_id
    type(c_ptr), value :: side_set_elem_list, side_set_side_list
    integer(c_int) :: error
    error = ex_put_side_set_c(exoid, as64(side_set_id), side_set_elem_list, &
         side_set_side_list)
  end function ex_put_side_set

  function ex_get_node_set_param(exoid, node_set_id, num_node_in_set, &
       num_df_in_set) result(error)

    integer(c_int), value, intent(in) :: exoid,  node_set_id
    integer(c_int), intent(out) :: num_node_in_set, num_df_in_set
    integer(c_int) :: error
    error = ex_get_node_set_param_c(exoid, as64(node_set_id), num_node_in_set, &
         num_df_in_set)
  end function ex_get_node_set_param

  function ex_put_node_set_param(exoid, node_set_id, num_node_in_set, &
       num_df_in_set) result(error)

    integer(c_int), value, intent(in) :: exoid, node_set_id
    integer(c_int), value, intent(in) :: num_node_in_set, num_df_in_set
    integer(c_int) :: error
    error = ex_put_node_set_param_c(exoid, as64(node_set_id), as64(num_node_in_set), &
         as64(num_df_in_set))
  end function ex_put_node_set_param

  function ex_get_node_set(exoid, node_set_id, node_set_node_list) result(error)

    integer(c_int), value, intent(in) :: exoid, node_set_id
    type(c_ptr), value :: node_set_node_list
    integer(c_int) :: error
    error = ex_get_node_set_c(exoid, as64(node_set_id), node_set_node_list)
  end function ex_get_node_set

  function ex_put_node_set(exoid, node_set_id, node_set_node_list) result(error)

    integer(c_int), value, intent(in) :: exoid, node_set_id
    type(c_ptr), value :: node_set_node_list
    integer(c_int) :: error
    error = ex_put_node_set_c(exoid, as64(node_set_id), node_set_node_list)
  end function ex_put_node_set

  function ex_get_elem_conn(exoid, elem_blk_id, connect) result(error)
    integer(c_int), value, intent(in) :: exoid, elem_blk_id
    type(c_ptr), value :: connect
    integer(c_int) :: error
    error = ex_get_elem_conn_c(exoid, as64(elem_blk_id), connect)
  end function ex_get_elem_conn

  function ex_put_elem_conn(exoid, elem_blk_id, connect) result(error)

    integer(c_int), value, intent(in) :: exoid, elem_blk_id
    type(c_ptr), value :: connect
    integer(c_int) :: error
    error = ex_put_elem_conn_c(exoid, as64(elem_blk_id), connect)
  end function ex_put_elem_conn

  ! Caller can use ex_inquire_int to get the length of names stored in the DB
  ! and pass an appropriately defined NAMES argument
  function ex_get_names(exoid, ex_entity_type, names) result(error)
    integer(c_int), value :: exoid, ex_entity_type
    character(*,kind=c_char), intent(out) :: names(:)
    integer(c_int) :: error
    character(len(names)+1,kind=c_char), target :: buffer(size(names))
    type(c_ptr) :: cnames(size(names))
    integer :: j, n
    do j = 1, size(cnames)
      cnames(j) = c_loc(buffer(j))
    end do
    error = ex_get_names_c(exoid, ex_entity_type, cnames)
    if (error /= 0) return
    do j = 1, size(cnames) ! strip off the null termination
      n = strnlen_c(cnames(j), int(len(names),kind=c_size_t))
      names(j) = buffer(j)(1:n)
    end do
  end function

end module exodus_c_binding
