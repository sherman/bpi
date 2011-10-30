package ru.sherman.bpi

import java.util.Date
import java.text.SimpleDateFormat
import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: sherman
 * Date: 27.10.11
 * Time: 22:45
 * To change this template use File | Settings | File Templates.
 */

object Operation extends Enumeration {
    type Operation = Value
    val Buy, Sell = Value
}

import Operation._

class StatElt(num: Int, symbol: String, quantity: Int, date: Date, operation: Operation, price: Float) {
    override def toString: String = {
        num + "," + symbol + "," + quantity + "," + date + "," + operation + "," + price
    }
}

object StatisticExtractor {
    def main(args: Array[String]): Unit = {
        if (args.length < 2)
            throw new IllegalArgumentException("Not enough args!");

        val lines = StatisticReader.read("http://investor.rts.ru/ru/statistics/2011/default.aspx?act=deals&nick=" + args(0) + "&date=" + args(1))

        // extract date
        val dateRegex = """<td align=right>Дата:</td><td width='100%' align=left>([0-9]{4}-[0-9]{2}-[0-9]{2})&nbsp;&nbsp;""".r
        val datePart = dateRegex.findFirstMatchIn(lines(0)).get.group(1);

        val statEltRegex = """<tr valign=top class=tr0><td align='right'>(\d+)</td><td><a class=nulink href=\"http://www.rts.ru/ru/forts/contract.html[?]isin=([^"]+)\">([^<]+)</a>&nbsp;</td><td>Futures</td><td align=center>(покупка|продажа)&nbsp;</td><td align=right nowrap>&nbsp;([^&]+)&nbsp;</td><td align=right nowrap>&nbsp;(\d+)&nbsp;</td><td align=right nowrap>&nbsp;([0-9]{2}):([0-9]{2}):([0-9]{2})&nbsp;</td></tr>""".r
        var elts = ListBuffer[StatElt]();

        val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        for (elt <- lines) {
            for (m <- statEltRegex.findAllIn(elt).matchData) {
                elts += new StatElt(
                    m.subgroups(0) toInt,
                    m.subgroups(1),
                    m.subgroups(5) toInt,
                    dateFormat.parse(
                        datePart + " "
                            + m.subgroups(6) + ":"
                            + m.subgroups(7) + ":"
                            + m.subgroups(8)
                    ),
                    extractOperation(m.subgroups(3)),
                    m.subgroups(4) replaceAll("""[ ]+""", "") toFloat
                )
            }
        }

        elts map println _

    }

    def extractOperation(in: String): Operation = in match {
        case "покупка" => Operation.Buy
        case "продажа" => Operation.Sell
        case _ => {
            println("unknown operation type")
            Operation.Buy
        }
    }
}

class StatisticReader(fileName: String) {
    def getStatisticLines(): List[String] = {
        io.Source.fromURL(
            fileName,
            "windows-1251"
        ) getLines() filter (elt => elt contains "Статистика по портфелю") toList
    }
}

object StatisticReader {
    def read(fileName: String): List[String] = new StatisticReader(fileName) getStatisticLines;
}

