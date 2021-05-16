val CKBytes = Array.tabulate(32, 4)(4 * _ + _).map(_.map(_ * 7 % 256))
println(CKBytes.map(_.map(_.toHexString).mkString(" ")).mkString("\n"))

257.toHexString

BigInt("F12186F9", 16)

val lut = Array(
  "D690E9FECCE13DB716B614C228FB2C05",
  "2B679A762ABE04C3AA44132649860699",
  "9C4250F491EF987A33540B43EDCFAC62",
  "E4B31CA9C908E89580DF94FA758F3FA6",
  "4707A7FCF37317BA83593C19E6854FA8",
  "686B81B27164DA8BF8EB0F4B70569D35",
  "1E240E5E6358D1A225227C3B01217887",
  "D40046579FD327524C3602E7A0C4C89E",
  "EABF8AD240C738B5A3F7F2CEF96115A1",
  "E0AE5DA49B341A55AD933230F58CB1E3",
  "1DF6E22E8266CA60C02923AB0D534E6F",
  "D5DB3745DEFD8E2F03FF6A726D6C5B51",
  "8D1BAF92BBDDBC7F11D95C411F105AD8",
  "0AC13188A5CD7BBD2D74D012B8E5B4B0",
  "8969974A0C96777E65B9F109C56EC684",
  "18F07DEC3ADC4D2079EE5F3ED7CB3948")
val lutBits = lut.reduce(_ + _).grouped(2).toSeq
lutBits(14 * 16 + 15).toString