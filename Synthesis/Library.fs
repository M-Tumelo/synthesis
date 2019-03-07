module Synthesis

let abelar a =
    a>12 && a<3097 && a%12=0 

let area m n=
    match m<0.0 || n<0.0 with
    |true -> failwith "Not implemented"
    |false ->0.5*m*n
    
    

let zollo t =
    match t>0 with
    |true -> t*2
    |false -> t*(-1)

let min a b =
    match a<b with
    |true ->a
    |false -> b

let max d e =
    match d>e with
    |true ->d
    |false -> e

let ofTime a b c = a*3600+b*60+c

let toTime x =
    match x<0 with 
    |true -> (0,0,0)
    |false -> (x/3600,(x%3600/60),x%3600%60)

let digits dig =
     
    failwith "Not implemented"

let minmax (a,b,c,d) =

   (min a b|> min c |> min d,max a b|> max c |> max d)


let isLeap _ =
    failwith "Not implemented"

let month a =
     match a with
     |1 -> ("January",31)
     |2 -> ("Feburary",28)
     |3 -> ("March",31)
     |4 -> ("April",30)
     |5 -> ("May",31)
     |6 -> ("June",30)
     |7 -> ("July",31)
     |8 -> ("August",31)
     |9 -> ("September",30)
     |10 -> ("October",31)
     |11 -> ("November",30)
     |12 -> ("December",31)
     |_ -> failwith "Um something is wrong!"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"