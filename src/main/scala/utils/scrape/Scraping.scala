package utils.scrape

import graph.algorithm.CosineSimilarity._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Document
import net.ruippeixotog.scalascraper.scraper.ContentExtractors._
import net.ruippeixotog.scalascraper.scraper.SimpleExtractor
import rafcollections.map.HashMap

/**
  * The scraping utility methods. Fully RT.
  */
object Scraping {

  val ROOT = "https://en.wikipedia.org"

  def extractLinks(document: Document)(implicit browser: JsoupBrowser): Iterable[(String, String, Double)] = {
    (document >> elements("a"))
      .filter(_.attrs.keySet.contains("href"))
      .map(_.attr("href"))
      .filterNot{(link) =>
        link.startsWith("#") || !link.startsWith("/wiki") || link.contains(".jpg")
      }
      .slice(5, 10)
      .map { link =>
        (wikiLink(document.location), link, cosineSimilarity(textFrequency(document), textFrequency(browser.get(ROOT + link))))
      }
  }

  def makeLinkTree(rootDocument: Document, depth: Int)(implicit browser: JsoupBrowser): Iterable[(String, String, Double)] = {
    val canDelve = depth > 0

    canDelve match {
      case true =>
        val childLinks = extractLinks(rootDocument)
        childLinks ++ childLinks.flatMap(edge => makeLinkTree(browser.get(ROOT + edge._2), depth - 1))
      case false => Iterable.empty
    }
  }

  def wikiLink(location: String): String = {
    val dropIndex = location.indexOf("/wiki")
    location.drop(dropIndex)
  }


  def scrapeText(document: Document): Seq[String] = {
    val allBodyText = document >> SimpleExtractor("body")

    val filteredWords = allBodyText.filterNot((element) => StopWords.All.exists(element.equals(_)))

    filteredWords.map(_.replace(".", "")).head.split(' ')
  }

  def textFrequency(document: Document): HashMap[CharSequence, Int] = {
    val map = new HashMap[CharSequence, Int]

    val allText = scrapeText(document)
    allText.foreach(insertOrIncrement(_, map))
    map
  }

  def insertOrIncrement(word: String, targetMap: HashMap[CharSequence, Int]): Unit = {
    targetMap.updateEither(word, eitherFunc)
  }

  def eitherFunc: Option[Int] => Int = {
    case Some(value) => value + 1
    case None => 1
  }

}
