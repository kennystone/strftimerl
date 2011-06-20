THE GOAL
========  

`
Now = now().

strftime:f("Printed on %m/%d/%Y", Now). #=> "Printed on 11/19/2007"

strftime:f("at %I:%M%p", Now).          #=> "at 08:37AM"
`

`strftimerl(now(), FormatString)`

implemented the following from from ruby's 1.9 strftime docs

+ %a - The abbreviated weekday name (``Sun'')
+ %A - The  full  weekday  name (``Sunday'')
+ %b - The abbreviated month name (``Jan'')
+ %B - The  full  month  name (``January'')
+ %C - Century (20 in 2009)
+ %d - Day of the month (01..31)
+ %D - Date (%m/%d/%y)
+ %e - Day of the month, blank-padded ( 1..31)
+ %F - Equivalent to %Y-%m-%d (the ISO 8601 date format)
+ %h - Equivalent to %b
+ %H - Hour of the day, 24-hour clock (00..23)
+ %I - Hour of the day, 12-hour clock (01..12)
+ %k - hour, 24-hour clock, blank-padded ( 0..23)
+ %l - hour, 12-hour clock, blank-padded ( 0..12)
+ %L - Millisecond of the second (000..999)
+ %m - Month of the year (01..12)
+ %M - Minute of the hour (00..59)
+ %N - Fractional seconds digits
+ %p - Meridian indicator (``AM''  or  ``PM'')
+ %P - Meridian indicator (``am''  or  ``pm'')
+ %r - time, 12-hour (same as %I:%M:%S %p)
+ %R - time, 24-hour (%H:%M)
+ %s - Number of seconds since 1970-01-01 00:00:00 UTC.
+ %S - Second of the minute (00..60)
+ %T - time, 24-hour (%H:%M:%S)
+ %u - Day of the week as a decimal, Monday being 1. (1..7)
+ %v - VMS date (%e-%b-%Y)
+ %w - Day of the week (Sunday is 0, 0..6)
+ %y - Year without a century (00..99)
+ %Y - Year with century
+ %% - Literal ``%'' character

