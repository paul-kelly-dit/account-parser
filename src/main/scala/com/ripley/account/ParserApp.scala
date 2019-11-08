package com.ripley.account

import com.github.tototoshi.csv.CSVReader

import scala.io.Source


object ParserApp extends App {

  // rules
  // number length 4 - its a cheque ie 4785
  // number length 6 space and more text its a X ie 001496 FAST CASH 985021 or 001493 001493 32055296
  // number contains string 15FEB A/C 89649047 or 16AUG A/C 89649047 or 17MAY A/C 89649047
  // this 9822 02AUG19
  //  ADRIAN KELLY - TRADES
  // starts with ALBURY ie ALBURY or ALBURY LTD or ALBURY LTD or ALBURY WAY LTD
  // starts with DRAFT
  // starts with ELAVON
  // starts with GSD


  def isAllDigits(x: String) = x forall Character.isDigit

  def startsWithXDigitsAndASpace(x: String, numDigits: Integer) = {
    val toDigits = numDigits + 1
    val first6Digits = x.substring(0, numDigits)
    val seventhDigit = x.substring(numDigits, toDigits)
    isAllDigits(first6Digits) && seventhDigit.equals(" ")
  }

  val tradesMen = List("ADRIAN KELLY")

  def classify(item: String) = {
    item match {
      case a if (a.length == 4 && isAllDigits(a)) => "cheque"
      case b if (b.length > 7 && startsWithXDigitsAndASpace(b, 6)) => "toFindOut"
      case c if (c.contains(" A/C ")) => "toFindOut"
      case d if (d.length > 5 && startsWithXDigitsAndASpace(d, 4)) => "toFindOut"
      case e if (tradesMen.contains(e)) => "tradesman"
      case f if (f.startsWith("ALBURY")) => "albury"
      case g if (g.contains("ARACHAS") || g.contains("GAS") || g.contains("FIRE") || g.contains("DGB SERVICES") || g.contains("ELECTRIC IRELAND")|| g.contains("SSEAIRTRICITY")) => "insuranceHeatGasElec"
      case h if (h.contains("DUBLIN FOOD") || h.contains("FRYLITE") || h.contains("JAVA REPUBLIC")  || h.contains("FRESH N UP")|| h.contains("TOLKA EGG")) => "foodBeverage"
      case i if (i.contains("FAILTE")) => "tourism"
      case k if (k.contains("BOOKING.COM")) => "bookingCom"
      case l if (l.contains("CHARLES STEWART")) => "charlesStewart"
      case m if (m.contains("DRAFT")) => "draft"
      case n if (n.contains("EXPEDIA LODGING PA")) => "expedia"
      case o if (o.contains("ELAVON")) => "elavon"
      case p if (p.contains("FLORAL IMAGE") || p.contains("HUGH_JORDAN") || p.contains("HOTEL INTERIORS")  || p.contains("KIERNAN") || p.contains("PREMIER LINEN")) => "interiorsCrockery"
      case q if (q.contains("GALLAGHER TV") || q.contains("VIRGIN MEDIA")) => "hotelEntertainment"
      case r if (r.contains("INK PLUS") || r.contains("INDIANFIELD")) => "officeCosts"
      case s if (s.contains("SITEMINDER") || s.contains("HRS GMBH")) => "websiteIT"
      case t if (t.contains("FREANEY") || t.contains("SMITH")) => "accountancyPension"
      case u if (u.contains("GSD")) => "security"
      case v if (v.contains("WAGES")) => "wages"
      case w if (w.contains("PLANNING OFFICE") || w.contains("OSS")|| w.contains("T J O'MAHONY")) => "constructionCosts"
      case x if (x.contains("REHAB") || v.contains("IRISH WATER CORE")|| v.contains("SEDGWICK IRE")|| v.contains("TERMINAL RENTAL")|| v.contains("THE CITYBIN")|| v.contains("THREE IRELAND")) => "hotelCostsClaims"
      case y if (y.contains("WOODSBERRY")) => "woodsberry"
      case z if (z.contains("REVENUE COMMISSION")) => "revenueCommission"
      case za if (za.contains("TOURWISE IRELAND")) => "tourOperators"
      case zb if (zb.contains("ULSTER BANK ")) => "bankFees"
      case _ => "toFindOut"
    }
  }

  case class BankRow(date: String, narrativeOne: String, narrativeTwo: String, transactionType: String, debit: String, credit: String,
                     cheque: String = "", toFindOut: String = "", tradesman: String = "", albury: String = "", insuranceHeatGasElec: String = "", foodBeverage: String = "",
                     tourism: String = "", elavon: String = "", expedia: String = "", draft: String = "", charlesStewart: String = "", bookingCom: String = "",
                     interiorsCrockery: String = "", hotelEntertainment: String = "", officeCosts: String = "", accountancyPension: String = "",
                     security: String = "", wages: String = "", hotelCostsClaims: String = "", woodsberry: String = "", revenueCommission: String = "",
                     tourOperators: String = "", bankFees: String = "")

  val reader = CSVReader.open(Source.fromResource("charles.csv"))
  //  val x = reader.all();
  //  private val value = reader.allWithHeaders()
  private val (headers, rows) = reader.allWithOrderedHeaders()

  val bankRows = rows map { row =>

    val classifedBankEntry = classify(row("Narrative #1"))

    val debit = row("Debit")
    val credit = row("Credit")
    val amount = if (debit.isEmpty) credit else debit

    val bankRowInitial = BankRow(row("Date"), row("Narrative #1"), row("Narrative #2"), row("Type"), debit, credit)

    classifedBankEntry.match {
      case "foodBeverage" => bankRowInitial.copy(foodBeverage = amount)
    }
  }

  println(bankRows)
}
