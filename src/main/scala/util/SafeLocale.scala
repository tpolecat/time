package util

import java.util.Locale
import scalaz.effect.IO

/**
 * The JDK's Locale API is safe except for static methods; the catalog methods return arrays (we turn them to lists
 * here and compute them lazily) and the getter/setter for the system default are impure (we lift them into IO here).
 * Locale instances themselves are safe once you get a handle to one (all method are pure).
 */
object SafeLocale {

  /** Quick access to common locales defined by country. Canada is subdivided. */
  object Country {
    
    // format: OFF
    object Canada {
      val Default        = Locale.CANADA
      val FrenchSpeaking = Locale.CANADA_FRENCH
    }
    val China   = Locale.CHINA
    val France  = Locale.FRANCE
    val Germany = Locale.GERMANY
    val Italy   = Locale.ITALY
    val Japan   = Locale.JAPAN
    val Korea   = Locale.KOREA
    val Taiwan  = Locale.TAIWAN
    val UK      = Locale.UK
    val US      = Locale.US
    // format: ON

  }

  /** Quick access to common locales defined by language. Chinese is subdivided. */
  object Language {
    
    // format: OFF
    object Chinese {
      val Default     = Locale.CHINESE
      val Simplified  = Locale.SIMPLIFIED_CHINESE
      val Traditional = Locale.TRADITIONAL_CHINESE
    }
    val English  = Locale.ENGLISH
    val French   = Locale.FRENCH
    val German   = Locale.GERMAN
    val Italian  = Locale.ITALIAN
    val Japanese = Locale.JAPANESE
    val Korean   = Locale.KOREAN
    // format: ON

  }

  lazy val getAvailableLocales: List[Locale] =
    Locale.getAvailableLocales.toList

  lazy val getISOCountries: List[String] =
    Locale.getISOCountries.toList

  lazy val getISOLanguages: List[String] =
    Locale.getISOLanguages.toList
    
  val getDefault: IO[Locale] =
    IO(Locale.getDefault)

  def setDefault(loc: Locale): IO[Unit] =
    IO(Locale.setDefault(loc))

}