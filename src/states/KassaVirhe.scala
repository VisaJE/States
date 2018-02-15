package states

final case class KassaVirhe(private val message: String = "", 
                           private val cause: Throwable = None.orNull)
                      extends Exception(message, cause)

final case class VääräTuoteVirhe(private val message: String = "", 
                           private val cause: Throwable = None.orNull)
                      extends Exception(message, cause)