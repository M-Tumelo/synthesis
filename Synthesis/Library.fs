module Synthesis

open System.Xml.Linq

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
    let rec digit value f =
        match value = 0 with 
        |_ -> 
            match value<>dig with 
            |true -> digit (value/10) (f+1)
            |false -> f
        |true -> f
    digit dig 0

let minmax (a,b,c,d) =
   (min a b|> min c |> min d,max a b|> max c |> max d)

let isLeap k =
       match k<=1581 with
       |true-> failwith "Its not a leap year"
       |_->
        match (k%4=0) && (k%100 <>0) || (k%4=0) && (k%100=0) && (k%400=0) with 
        |true-> true
        |_ -> false

let month =function
     |1 -> ("January",31)
     |2 -> ("February",28)
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

let toBinary p =
     let rec bin s h =
        match s<0 with 
        |true ->failwith "Try again"
        |_-> 
            match s=0 && h<>"" with 
            |true-> h
            |false->
              match s%2<>0 with
              |true-> bin (s/2) h+"1"
              |_ -> bin (s/2) h+"0"
     bin p ""

let bizFuzz n =
    let rec fuzz i d x s =
        match i > n with 
        |true -> (d,x,s)
        |_-> 
            match (i%5=0) && (i%3=0) with 
            |true -> fuzz (i+1) (d+1) (x+1) (s+1)
            |_-> 
                match (i%5=0) with 
                 |true -> fuzz (i+1) d (x+1) s
                 |_-> 
                    match (i%3=0) with 
                    |true -> fuzz (i+1) (d+1) x s
                    |_-> fuzz (i+1) d x s
    fuzz 1 0 0 0

let monthDay day year =
     match day<=0 || day>366 || year <1582 with 
     |true ->failwith "Not implemented"
     |_-> 
        match isLeap year with 
        |true->true
        |false->false

let coord _ =
    failwith "Not implemented"