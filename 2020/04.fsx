open System.Text.RegularExpressions

type Passport = {
    BirthYear: Option<string>
    IssueYear: Option<string>
    ExpirationYear: Option<string>
    Height: Option<string>
    HairColor: Option<string>
    EyeColor: Option<string>
    PassportId: Option<string>
    CountryId: Option<string>
}

let tryParseInt (s: string) = 
    try
        s |> int |> Some 
    with _ ->
        None

let withinRange (s: string) minimum maximum = 
    match tryParseInt s with 
    | Some i -> i >= minimum && i <= maximum
    | None -> false

let isValidEyeColor (s: string) = 
    match s with 
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true 
    | _ -> false

let isValidHairColor (s: string) = 
    Regex.IsMatch(s, "^#[0-9a-f]{6}$")

let isNumDigits s numDigits = 
    let d = string numDigits
    Regex.IsMatch(s, "^\d{" + d + "}$")

let isValidHeight (s: string) = 
    let unit = s.[(String.length s - 2)..]
    let num = s.[0..(String.length s - 3)]
    match unit with 
    | "cm" -> withinRange num 150 193
    | "in" -> withinRange num 59 76 
    | _ -> false

let addNewLines s = 
    match s with 
    | "" -> "\n"
    | _ -> s

let split (s:string) = 
    s.Split("\n")

let getField field s = 
    let pattern = field + ":" + "([#\w]+)"
    let m = Regex.Match(s, pattern)
    let value = m.Groups.[1].Value
    match value with 
    | "" -> None 
    | _  -> Some(value)

let parsePassport s = 
    {
        BirthYear = getField "byr" s
        IssueYear = getField "iyr" s
        ExpirationYear = getField "eyr" s
        Height = getField "hgt" s
        HairColor = getField "hcl" s
        EyeColor = getField "ecl" s
        PassportId = getField "pid" s
        CountryId = getField "cid" s
    }

let passports = 
    System.IO.File.ReadLines("2020/inputs/04.txt")
    |> Seq.map addNewLines
    |> Seq.reduce (fun a b -> a + " " + b)
    |> split
    |> Array.map parsePassport

let validate1 (p : Passport) = 
    match p with 
    | { BirthYear = Some a; 
        IssueYear = Some b; 
        ExpirationYear = Some c; 
        Height = Some d; 
        HairColor = Some e; 
        EyeColor = Some f; 
        PassportId = Some g } -> true 
    | _ -> false

let validPassports1 = 
    Array.filter validate1 passports 
    |> Array.length

let validate2 (p: Passport) = 
    match p with 
    | { BirthYear = Some a; 
        IssueYear = Some b; 
        ExpirationYear = Some c; 
        Height = Some d; 
        HairColor = Some e; 
        EyeColor = Some f; 
        PassportId = Some g } -> isNumDigits a 4 && withinRange a 1920 2002 &&
                                 isNumDigits b 4 && withinRange b 2010 2020 &&
                                 isNumDigits c 4 && withinRange c 2020 2030 &&
                                 isValidHeight d &&
                                 isValidHairColor e &&
                                 isValidEyeColor f &&
                                 isNumDigits g 9
    | _ -> false

let validPassports2 = 
    Array.filter validate2 passports 
    |> Array.length