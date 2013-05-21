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

let removeNonSoundexChars str =
  str |> Seq.filter (fun c -> not <| List.exists (fun e -> c = e) ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'h'; 'w'])

let soundex (input : string) =
  let firstC = input.[0]
  let name = input.[1..]
  let name = Seq.map replaceWithCode name
  // remove adjacent codes
  // remove same codes separated by h or w
  let name = removeNonSoundexChars name

  firstC, name

let input = "Ashcraft"
let (firstC, name) = soundex input
printfn "%c, %A" firstC name