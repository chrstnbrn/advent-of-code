module PasswordProcessing.Tests

open NUnit.Framework
open Program

[<TestCase("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm",
           true)>]
[<TestCase("iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929",
           false)>]
[<TestCase("hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm",
           true)>]
[<TestCase("hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in",
           false)>]
[<TestCase("eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
           true)>]
[<TestCase("iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946",
           true)>]
[<TestCase("hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
           true)>]
[<TestCase("hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007",
           true)>]
let hasRequiredFields passport expected =
    let actual = hasRequiredFields passport
    Assert.AreEqual(expected, actual)
