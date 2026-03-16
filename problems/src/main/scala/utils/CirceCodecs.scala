package utils

import io.circe.{Codec, Decoder, Encoder}

import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import scala.util.Try

object CirceCodecs {
  private val base64Prefix = "𝔹"
  given byteArrayEncoder: Encoder[Array[Byte]] =
    Encoder.encodeString.contramap { bytes =>
      Try(new String(bytes, UTF_8))
        .filter(s => s.getBytes(UTF_8).sameElements(bytes) && !s.startsWith(base64Prefix))
        .getOrElse(base64Prefix + Base64.getEncoder.encodeToString(bytes))
    }
  given byteArrayDecoder: Decoder[Array[Byte]] =
    Decoder.decodeString.map { s =>
      if (s.startsWith(base64Prefix)) Base64.getDecoder.decode(s.stripPrefix(base64Prefix))
      else s.getBytes(UTF_8)
    }
  given byteArrayCodec: Codec[Array[Byte]] = Codec.implied
}
