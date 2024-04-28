package fprime.conversions

import scala.util.Success

given [T]: Conversion[T, Success[T]] = Success(_)
