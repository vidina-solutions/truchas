!!
!! SAFESTEP_TYPE
!!
!! This module defines a type for taking safely-sized time steps.
!!
!! Zach Jibben <zjibben@lanl.gov>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! This file is part of Truchas. 3-Clause BSD license; see the LICENSE file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "f90_assert.fpp"

module safestep_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  type, public :: safestep
    private
    logical :: previous_successful = .true.
    integer :: i1 = 0
    integer :: nfails = 10
    real(r8) :: factor = 0.5_r8
    real(r8) :: dt_safe = huge(1.0_r8)
    real(r8), allocatable :: dt_unsafe(:), t_unsafe(:)
  contains
    procedure :: init
    procedure :: register_failed
    procedure :: stepsize
    procedure, private :: purge_expired
    procedure, private :: update_dt_safe
  end type safestep

contains

  subroutine init(this, nfails)
    class(safestep), intent(out) :: this
    integer, intent(in), optional :: nfails
    if (present(nfails)) this%nfails = nfails
    allocate(this%dt_unsafe(this%nfails), this%t_unsafe(this%nfails))
    this%dt_unsafe = huge(1.0_r8)
    this%t_unsafe = huge(1.0_r8)
  end subroutine init


  ! Note 1: Don't log repeated failures on the same attempted step. Often it's needed to crank down
  ! multiple times and more strictly than needed later, to get the next step to succeed.
  subroutine register_failed(this, dt, t)
    class(safestep), intent(inout) :: this
    real(r8), intent(in) :: dt, t
    ! if (.not.this%previous_successful) return ! note 1
    ! this%previous_successful = .false.
    this%i1 = modulo(this%i1, this%nfails) + 1
    this%dt_unsafe(this%i1) = dt
    this%t_unsafe(this%i1) = t
    ! if (this%dt_unsafe(this%i1) < this%dt_safe) &
    !     this%dt_safe = (1 - this%factor) * this%dt_unsafe(this%i1)
    call this%purge_expired(t)
    call this%update_dt_safe
  end subroutine register_failed


  real(r8) function stepsize(this, t)
    class(safestep), intent(inout) :: this
    real(r8), intent(in) :: t
    call this%purge_expired(t)
    this%previous_successful = .true.
    stepsize = this%dt_safe
  end function stepsize


  subroutine purge_expired(this, t)

    class(safestep), intent(inout) :: this
    real(r8), intent(in) :: t

    real(r8) :: t0, t_dropoff
    integer :: imin
    logical :: purged

    purged = .false.
    ! carefully avoiding erroneous arithmetic errors with intel
    if (this%dt_safe > huge(1.0_r8) / 31) then
      t_dropoff = huge(1.0_r8)
    else
      t_dropoff = 30 * this%dt_safe
    end if
    t0 = t - t_dropoff

    do while (minval(this%t_unsafe) < t0)
      imin = minloc(this%t_unsafe, dim=1)
      purged = .true.
      this%dt_unsafe(imin) = huge(1.0_r8)
      this%t_unsafe(imin) = huge(1.0_r8)
    end do

    if (purged) call this%update_dt_safe

  end subroutine purge_expired


  pure subroutine update_dt_safe(this)
    class(safestep), intent(inout) :: this
    this%dt_safe = (1 - this%factor) * minval(this%dt_unsafe)
  end subroutine update_dt_safe

end module safestep_type
