reverseSum n = n + read (reverse (show n))
isPalindrome n = n == read (reverse (show n))
isLychrel n = isLychrel' (reverseSum n) 1
isLychrel' n iteration =
    if (iteration > 400) then True
    else
        if (isPalindrome n) then False
        else isLychrel' (reverseSum n) (iteration + 1)

main = print $ length $ filter (==True) $ map isLychrel [0..9999]
