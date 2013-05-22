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

let removeDups (input: char list) =
  let rec skipToNextNotEqual (char: char) (input: char list) =
    match input with
      | x :: rest when x = char -> skipToNextNotEqual char rest
      | x  -> x
  let rec removeDupsRec (acc: char list) (input: char list) =
    match input with
      | x::y::rest when x = y -> removeDupsRec (x::acc) (skipToNextNotEqual x rest)
      | x::y::z::rest when x = z && (y = 'h' || y = 'w') -> removeDupsRec (x::acc) rest
      | x::rest -> removeDupsRec (x::acc) rest
      | [] -> acc
  removeDupsRec [] input |> List.rev

let removeAllDups input =
  let rec passUntilNoChange (firstPass: char list) =
    if firstPass.Length <= 3 then
      firstPass
    else
      let secondPass = removeDups firstPass
      if firstPass = secondPass then
        firstPass
      else
        passUntilNoChange secondPass
  passUntilNoChange <| removeDups input
  
let removeNonSoundexChars str =
  str |> List.filter (fun c -> not <| List.exists (fun e -> c = e) ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'h'; 'w'])

let padOrChop (str: char list) =
  if str.Length >= 3 then
    Seq.take 3 str |> Array.ofSeq
  else
    (Array.append (Array.ofList str) (Array.create 2 '0')).[0..2]
    
let soundex (input: string) =
  let firstC = System.Char.ToUpper(input.[0])
  let name = 
    input.ToLower() |> Seq.toList
    |> List.map replaceWithCode
    |> removeAllDups
    |> List.tail
    |> removeNonSoundexChars
    |> padOrChop

  new System.String(Array.append [|firstC|] name)

let tests = [
  "Robert", "R163";
  "Rupert", "R163";
  "Rubin", "R150";
  "Ashcraft", "A261";
  "Ashcroft", "A261";
  "Tymczak", "T522";
  "Pfister", "P236";
]

let test (name, expected) =
  let actual = soundex name
  if actual = expected then
    None
  else
    Some(sprintf "%s expected %s got %s" name expected actual)

List.map test tests
  |> List.iter (fun t -> 
      if Option.isSome t then
        printfn "%s" t.Value
      else
        ()
    )
