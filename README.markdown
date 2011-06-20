THE GOAL
========  

from ruby's 1.9 strftime docs

strftimerl(now(), String) ->

String -> 
`
X %a - The abbreviated weekday name (``Sun'')
X %A - The  full  weekday  name (``Sunday'')
X %b - The abbreviated month name (``Jan'')
X %B - The  full  month  name (``January'')
X %C - Century (20 in 2009)
X %d - Day of the month (01..31)
X %D - Date (%m/%d/%y)
  %e - Day of the month, blank-padded ( 1..31)
X %F - Equivalent to %Y-%m-%d (the ISO 8601 date format)
  %h - Equivalent to %b
X %H - Hour of the day, 24-hour clock (00..23)
  %I - Hour of the day, 12-hour clock (01..12)
  %k - hour, 24-hour clock, blank-padded ( 0..23)
  %l - hour, 12-hour clock, blank-padded ( 0..12)
X %L - Millisecond of the second (000..999)
X %m - Month of the year (01..12)
X %M - Minute of the hour (00..59)
X %N - Fractional seconds digits
X %p - Meridian indicator (``AM''  or  ``PM'')
X %P - Meridian indicator (``am''  or  ``pm'')
  %r - time, 12-hour (same as %I:%M:%S %p)
X %R - time, 24-hour (%H:%M)
  %s - Number of seconds since 1970-01-01 00:00:00 UTC.
X %S - Second of the minute (00..60)
X %T - time, 24-hour (%H:%M:%S)
X %u - Day of the week as a decimal, Monday being 1. (1..7)
  %U - Week  number  of the current year,
          starting with the first Sunday as the first
          day of the first week (00..53)
  %v - VMS date (%e-%b-%Y)
  %V - Week number of year according to ISO 8601 (01..53)
  %W - Week  number  of the current year,
          starting with the first Monday as the first
          day of the first week (00..53)
  %w - Day of the week (Sunday is 0, 0..6)
X %y - Year without a century (00..99)
X %Y - Year with century
  %z - Time zone as  hour offset from UTC (e.g. +0900)
  %Z - Time zone name
X %% - Literal ``%'' character
`

   Now = now().
   strftimerl:f("Printed on %m/%d/%Y", Now). #=> "Printed on 11/19/2007"
   strftimerl:f("at %I:%M%p", Now).          #=> "at 08:37AM"

