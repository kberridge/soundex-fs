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

let padOrChop (str: char list) =
  if str.Length >= 3 then
    Seq.take 3 str |> Array.ofSeq
  else
    (Array.append (Array.ofList str) (Array.create 2 '0')).[0..2]

let removeDuplicates (lastChar, currentList) encodedChar =
  let lastListChar = List.head currentList
  match encodedChar with
    | x when x = lastChar && (lastListChar = 'h' || lastListChar = 'w') -> lastChar, (List.tail currentList)
    | x when x = lastChar -> lastChar, currentList
    | x when x = 'h' || x = 'w' -> lastChar, x::currentList
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> encodedChar, currentList
    | x when lastListChar = 'h' || lastListChar = 'w' -> encodedChar, x::(List.tail currentList)
    | x -> encodedChar, x::currentList

let findFirstCompleteList (scanSeq: seq<char * char list>)=
  let rec inner (lastChar, (currentList: char list)) (e: System.Collections.Generic.IEnumerator<_*_>) =
    let currentListHead = List.head currentList
    if currentList.Length >= 4 && (currentListHead <> 'h' || currentListHead <> 'w') then
      currentList
    else
      if e.MoveNext() then
        inner e.Current e
      else
        currentList
  let e = scanSeq.GetEnumerator()
  e.MoveNext() |> ignore
  inner (Seq.head scanSeq) e
    
    
let soundex (input: string) =
  let firstC = System.Char.ToUpper(input.[0])
  let encodedSeq = input.ToLower() |> Seq.map replaceWithCode
  let firstChar = Seq.head encodedSeq
  let result =
    Seq.scan removeDuplicates (firstChar, [firstChar]) (Seq.skip 1 encodedSeq)
    |> findFirstCompleteList
    |> List.rev |> List.tail
    |> padOrChop

  new System.String(Array.append [|firstC|] result)

let tests = [
  "Robert", "R163";
  "Rupert", "R163";
  "Rubin", "R150";
  "Ashcraft", "A261";
  "Ashcroft", "A261";
  "Tymczak", "T522";
  "Pfister", "P236";
  "Berrwridge", "B632";
  "Rhrumble", "R514";
  "Berrirdge", "B663";
  "Berhrrhrrnnn", "B650"
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
