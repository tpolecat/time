package object tick {

  type CommonYear = Year.CommonYear
  val CommonYear = Year.CommonYear

  type LeapYear = Year.LeapYear
  val LeapYear = Year.LeapYear

  val Jan = Month.Jan 
  val Feb = Month.Feb 
  val Mar = Month.Mar 
  val Apr = Month.Apr 
  val May = Month.May 
  val Jun = Month.Jun 
  val Jul = Month.Jul 
  val Aug = Month.Aug 
  val Sep = Month.Sep 
  val Oct = Month.Oct 
  val Nov = Month.Nov 
  val Dec = Month.Dec 

  val Sun = Weekday.Sun
  val Mon = Weekday.Mon
  val Tue = Weekday.Tue
  val Wed = Weekday.Wed
  val Thu = Weekday.Thu
  val Fri = Weekday.Fri
  val Sat = Weekday.Sat

  type HasDay[A] = tick.tc.HasDay[A]
  val HasDay = tick.tc.HasDay

  type HasMonth[A] = tick.tc.HasMonth[A]
  val HasMonth = tick.tc.HasMonth

  type HasYear[A] = tick.tc.HasYear[A]
  val HasYear = tick.tc.HasYear

}

