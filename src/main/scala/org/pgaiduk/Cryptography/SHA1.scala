package org.pgaiduk.Cryptography
import java.math.BigInteger

import java.security.MessageDigest

class SHA1(str:String) {
  private val md = MessageDigest.getInstance("SHA1")
  private val digest = md.digest(str.getBytes)
  private var value = ""
  for (elem <- digest){
    value += (elem + 128).toHexString
  }

  def getSHA1str: String = {
    value
  }

  def getSHA1: BigInt = {
    new BigInteger(value, 16)
  }

}
