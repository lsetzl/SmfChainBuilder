package com.gmail.lsetzl.testsong

import com.gmail.lsetzl.smfchainbuilder.{Section, SmfChainBuilder}

object TestSong {
  def main(args: Array[String]): Unit = {
    val a = Section(0.0, 4.0)

    SmfChainBuilder()
      .channel

      .channel
      .track
      .section(a).notes("c4d4e2 c4d4e2 g4e4d4c4 d4e4d2")

      .write("test.mid")
  }
}
