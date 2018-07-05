namespace datetime

inductive day_of_week
| monday | tuesday | wednesday
| thursday | friday
| saturday | sunday

def day_of_week.to_nat (dow : day_of_week) : ℕ :=
day_of_week.rec_on dow 1 2 3 4 5 6 7

inductive month
| jan | feb | mar
| apr | may | jun
| jul | aug | sep
| oct | nov | dec

def month.to_nat (m : month) : ℕ :=
month.rec_on m 1 2 3 4 5 6 7 8 9 10 11 12

instance month.has_to_string : has_to_string month :=
⟨λ m,
month.rec_on m
  "January" "February" "March"
  "April" "May" "June"
  "July" "August" "September"
  "October" "November" "December"⟩

instance month.has_repr : has_repr month :=
⟨to_string⟩

instance day_of_week.has_to_string : has_to_string day_of_week :=
⟨λ d,
day_of_week.rec_on d
  "Monday" "Tuesday"
  "Wednesday" "Thursday"
  "Friday" "Saturday"
  "Sunday"⟩

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
