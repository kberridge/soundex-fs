let charGroups = [
    1, ['b'; 'f'; 'p'; 'v'];
    2, ['c'; 'g'; 'j'; 'k'; 'q'; 's'; 'x'; 'z'];
    3, ['d'; 't'];
    4, ['l'];
    5, ['m'; 'n'];
    6, ['r']
  ]

let input = "Ashcraft"

let soundex (input : string) =
  let firstC = input.[0]
  let name = input.[1..]
  let name = name |> Seq.filter (fun c -> not <| List.exists (fun e -> c = e) ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'h'; 'w'])

  firstC, name

let (firstC, name) = soundex input
printfn "%c, %A" firstC name

