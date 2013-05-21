let charGroups = [
    '1', ['b'; 'f'; 'p'; 'v'];
    '2', ['c'; 'g'; 'j'; 'k'; 'q'; 's'; 'x'; 'z'];
    '3', ['d'; 't'];
    '4', ['l'];
    '5', ['m'; 'n'];
    '6', ['r']
  ]

let charListContains char charList = charList |> List.exists(fun listC -> char = listC)
let findCharGroup char = charGroups |> List.tryFind(fun (code, charList) -> charListContains char charList)

let replaceWithCode char =
  let matchingCharGroup = findCharGroup char
  match matchingCharGroup with
    | Some(code, _) -> code
    | None -> char

let removeAdjacentCodes (str : char list) =
  seq { 
    if str.Length > 0 then yield str.[0]
    for i=1 to str.Length-1 do
      if str.[i] <> str.[i-1] then
        yield str.[i] 
  } |> Seq.toList

let removeNonSoundexChars str =
  str |> List.filter (fun c -> not <| List.exists (fun e -> c = e) ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'h'; 'w'])

let soundex (input : string) =
  let firstC = input.[0]
  let name = input.[1..]
  let name = List.map replaceWithCode (Seq.toList name)
  let name = removeAdjacentCodes name
  // remove adjacent codes
  // remove same codes separated by h or w
  let name = removeNonSoundexChars name

  firstC, name

let input = "Ashcraft"
let (firstC, name) = soundex input
printfn "%c, %A" firstC name