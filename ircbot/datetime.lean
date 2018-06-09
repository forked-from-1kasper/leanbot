namespace datetime

inductive day_of_week
| monday | tuesday | wednesday
| thursday | friday
| saturday | sunday

def day_of_week.to_nat : day_of_week → ℕ
| day_of_week.monday := 1
| day_of_week.tuesday := 2
| day_of_week.wednesday := 3
| day_of_week.thursday := 4
| day_of_week.friday := 5
| day_of_week.saturday := 6
| day_of_week.sunday := 7

inductive month
| jan | feb | mar
| apr | may | jun
| jul | aug | sep
| oct | nov | dec

def month.to_nat : month → ℕ
| month.jan := 1 | month.feb := 2  | month.mar := 3
| month.apr := 4 | month.may := 5  | month.jun := 6
| month.jul := 7 | month.aug := 8  | month.sep := 9
| month.oct := 9 | month.nov := 10 | month.dec := 12

instance month.has_to_string : has_to_string month :=
⟨λ m, match m with
| month.jan := "January" 
| month.feb := "February"
| month.mar := "March"
| month.apr := "April"
| month.may := "May"
| month.jun := "June"
| month.jul := "July"
| month.aug := "August"
| month.sep := "September"
| month.oct := "October"
| month.nov := "November"
| month.dec := "December"
end⟩

instance month.has_repr : has_repr month :=
⟨to_string⟩

instance day_of_week.has_to_string : has_to_string day_of_week :=
⟨λ d, match d with
| day_of_week.monday := "Monday"
| day_of_week.tuesday := "Tuesday"
| day_of_week.wednesday := "Wednesday"
| day_of_week.thursday := "Thursday"
| day_of_week.friday := "Friday"
| day_of_week.saturday := "Saturday"
| day_of_week.sunday := "Sunday"
end⟩

instance day_of_week.has_repr : has_repr day_of_week :=
⟨to_string⟩

structure date :=
(year : nat)
(month : month)
(day : nat)
(hour : nat)
(minute : nat)
(seconds : nat)
(nanoseconds : nat)
(weekday : day_of_week)

def null_date : date :=
  { year := 0, month := month.jan,
    day := 0, hour := 0,
    minute := 0, seconds := 0,
    nanoseconds := 0,
    weekday := day_of_week.monday }

end datetime