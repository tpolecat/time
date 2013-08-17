package time
package calendar

package object syntax {
  
  object hasYear extends ToHasYearOps
  object hasMonth extends ToHasMonthOps
  object hasDay extends ToHasDayOps

  object all extends ToHasYearOps
    with ToHasMonthOps
    with ToHasDayOps

}





