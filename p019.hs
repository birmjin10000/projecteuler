isLeapYear year = ((year `mod` 4 == 0) && (year `mod` 100 /= 0)) || (year `mod` 400 == 0)

countDaysOfYear year
    | (isLeapYear year) = 366
    | otherwise = 365 

countDaysUptoFirstOfMonth year month =
    (sum $ map countDaysOfYear [1901..(year - 1)]) + (sum $ map (daysOfMonth year) [1..(month -1)]) + 1

daysOfMonth year month =
    if (month == 2) then 
        if (isLeapYear year) then 29
        else 28
    else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]!!(month-1)

isFirstOfMonthSunday (year, month) =
    let sumOfDays = countDaysUptoFirstOfMonth year month
    in (sumOfDays `mod` 7 == 6) -- Jan 1 1901 is Tuesday.

main = print $ length $ filter (==True) $ (map isFirstOfMonthSunday [(year, month)|year <- [1901..2000], month <- [1..12]])

