package com.ripley.account

import java.io.{BufferedWriter, File, FileWriter}

import com.github.tototoshi.csv.CSVReader

import scala.io.Source

object ParserApp extends App {


  def isAllDigits(x: String) = x forall Character.isDigit

  def isDouble(numberAsString: String) = try {
    numberAsString.toDouble
    true
  } catch {
    case _ => false
  }

  def startsWithXDigitsAndASpace(x: String, numDigits: Integer) = {
    val toDigits = numDigits + 1
    val first6Digits = x.substring(0, numDigits)
    val seventhDigit = x.substring(numDigits, toDigits)
    isAllDigits(first6Digits) && seventhDigit.equals(" ")
  }

  val tradesMen = List("ADRIAN KELLY")

  def classify(item: String, transactionType: String) = {
    if (transactionType == "CHQ") "cheque"
    else {
      item match {
        case b if (b.length > 7 && startsWithXDigitsAndASpace(b, 6)) => "toFindOut"
        case c if (c.contains(" A/C ")) => "toFindOut"
        case d if (d.length > 5 && startsWithXDigitsAndASpace(d, 4)) => "toFindOut"
        case e if (tradesMen.contains(e)) => "tradesman"
        case f if (f.startsWith("ALBURY")) => "albury"
        case g if (g.contains("ARACHAS") || g.contains("GAS") || g.contains("FIRE") || g.contains("DGB SERVICES") || g.contains("ELECTRIC IRELAND") || g.contains("SSEAIRTRICITY")) => "insuranceHeatGasElec"
        case h if (h.contains("DUBLIN FOOD") || h.contains("FRYLITE") || h.contains("JAVA REPUBLIC") || h.contains("FRESH N UP") || h.contains("TOLKA EGG")) => "foodBeverage"
        case i if (i.contains("FAILTE")) => "tourism"
        case k if (k.contains("BOOKING.COM")) => "bookingCom"
        case l if (l.contains("CHARLES STEWART")) => "charlesStewart"
        case m if (m.contains("DRAFT")) => "draft"
        case n if (n.contains("EXPEDIA LODGING PA")) => "expedia"
        case o if (o.contains("ELAVON")) => "elavon"
        case p if (p.contains("FLORAL IMAGE") || p.contains("HUGH_JORDAN") || p.contains("HOTEL INTERIORS") || p.contains("KIERNAN") || p.contains("PREMIER LINEN")) => "interiorsCrockery"
        case q if (q.contains("GALLAGHER TV") || q.contains("VIRGIN MEDIA")) => "hotelEntertainment"
        case r if (r.contains("INK PLUS") || r.contains("INDIANFIELD")) => "officeCosts"
        case s if (s.contains("SITEMINDER") || s.contains("HRS GMBH")) => "websiteIT"
        case t if (t.contains("FREANEY") || t.contains("SMITH")) => "accountancyPension"
        case u if (u.contains("GSD")) => "security"
        case v if (v.contains("WAGES")) => "wages"
        case w if (w.contains("PLANNING OFFICE") || w.contains("OSS") || w.contains("T J O'MAHONY")) => "constructionCosts"
        case x if (x.contains("REHAB") || x.contains("IRISH WATER CORE") || x.contains("SEDGWICK IRE") || x.contains("TERMINAL RENTAL") || x.contains("THE CITYBIN") || x.contains("THREE IRELAND")) => "hotelCostsClaims"
        case y if (y.contains("WOODSBERRY")) => "woodsberry"
        case z if (z.contains("REVENUE COMMISSION")) => "revenueCommission"
        case za if (za.contains("TOURWISE IRELAND")) => "tourOperators"
        case zb if (zb.contains("ULSTER BANK ")) => "bankFees"
        case _ => "toFindOut"
      }
    }
  }

  case class BankRow(date: String, narrativeOne: String, narrativeTwo: String, transactionType: String, debit: String, credit: String,
                     accountancyPension: String = "", albury: String = "", bankFees: String = "", bookingCom: String = "", charlesStewart: String = "", cheque: String = "",
                     constructionCosts: String = "", draft: String = "", elavon: String = "", expedia: String = "", foodBeverage: String = "", hotelCostsClaims: String = "",
                     hotelEntertainment: String = "", insuranceHeatGasElec: String = "", interiorsCrockery: String = "", officeCosts: String = "", revenueCommission: String = "",
                     security: String = "", toFindOut: String = "", tourOperators: String = "", tourism: String = "", tradesman: String = "", wages: String = "",
                     websiteIT: String = "", woodsberry: String = "")

  private def enrichBankRow(row: Map[String, String], threshold: Double) = {
    val itemEntry = row("Narrative #1")
    val transactionType = row("Type")
    val classifedBankEntry = classify(itemEntry, transactionType)

    val debit = row("Debit")
    val credit = row("Credit")
    val amount = if (debit.isEmpty) credit else debit

    if (!debit.isEmpty && isDouble(debit)) {
      if (debit.toDouble < threshold) {
        println(s"Concerning: ${row("Date")} - $itemEntry - $debit")
      }
    }
    val bankRowInitial = BankRow(row("Date"), row("Narrative #1"), row("Narrative #2"), row("Type"), debit, credit)

    val updatedBankRow = classifedBankEntry match {
      case "foodBeverage" => bankRowInitial.copy(foodBeverage = amount)
      case "cheque" => bankRowInitial.copy(cheque = amount)
      case "tradesman" => bankRowInitial.copy(tradesman = amount)
      case "toFindOut" => bankRowInitial.copy(toFindOut = amount)
      case "albury" => bankRowInitial.copy(albury = amount)
      case "insuranceHeatGasElec" => bankRowInitial.copy(insuranceHeatGasElec = amount)
      case "tourism" => bankRowInitial.copy(tourism = amount)
      case "bookingCom" => bankRowInitial.copy(bookingCom = amount)
      case "draft" => bankRowInitial.copy(draft = amount)
      case "charlesStewart" => bankRowInitial.copy(charlesStewart = amount)
      case "expedia" => bankRowInitial.copy(expedia = amount)
      case "elavon" => bankRowInitial.copy(elavon = amount)
      case "expedia" => bankRowInitial.copy(expedia = amount)
      case "interiorsCrockery" => bankRowInitial.copy(interiorsCrockery = amount)
      case "hotelEntertainment" => bankRowInitial.copy(hotelEntertainment = amount)
      case "officeCosts" => bankRowInitial.copy(officeCosts = amount)
      case "accountancyPension" => bankRowInitial.copy(accountancyPension = amount)
      case "constructionCosts" => bankRowInitial.copy(constructionCosts = amount)
      case "websiteIT" => bankRowInitial.copy(websiteIT = amount)
      case "security" => bankRowInitial.copy(security = amount)
      case "wages" => bankRowInitial.copy(wages = amount)
      case "hotelCostsClaims" => bankRowInitial.copy(hotelCostsClaims = amount)
      case "woodsberry" => bankRowInitial.copy(woodsberry = amount)
      case "revenueCommission" => bankRowInitial.copy(revenueCommission = amount)
      case "tourOperators" => bankRowInitial.copy(revenueCommission = amount)
      case "bankFees" => bankRowInitial.copy(revenueCommission = amount)
      case _ => bankRowInitial
    }
    updatedBankRow
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line)
    }
    bw.close()
  }

  private def classifyAndGenerateBankStatement(csvBankStatement: String, threshold: Double) = {
    val reader = CSVReader.open(Source.fromResource(csvBankStatement))
    //  val x = reader.all();
    //  private val value = reader.allWithHeaders()
    val (headers, rows) = reader.allWithOrderedHeaders()
    val bankRows = rows.map(row => enrichBankRow(row, threshold))

    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.generic._

    // File in which we'll be writing the CSV data.
    //    val out = java.io.File.createTempFile("kantan.csv", "csv")
    val out = new File("classified-" + csvBankStatement)
    val writer = out.asCsvWriter[BankRow](rfc.withHeader("date", "narrativeOne", "narrativeTwo", "transactionType", "debit", "credit",
      "accountancyPension", "albury", "bankFees", "bookingCom", "charlesStewart", "cheque", "constructionCosts", "draft", "elavon",
      "expedia", "foodBeverage", "hotelCostsClaims", "hotelEntertainment", "insuranceHeatGasElec", "interiorsCrockery", "officeCosts",
      "revenueCommission", "security", "toFindOut", "tourOperators", "tourism", "tradesman", "wages", "websiteIT", "woodsberry"))

    writer.write(bankRows).close();
    //    val updatedFileName = "classified-" + csvBankStatement
    //    writeFile(updatedFileName,rowsAsCSV)

    reader.close()
  }

  private val charlesStewartBankStatement = "CHARLES STEWART-Statement-Jan-2019-Nov-2019.csv"
  private val threshold: Double = -10000

  private val bankRows = classifyAndGenerateBankStatement(charlesStewartBankStatement, threshold)

  println(bankRows)
}
