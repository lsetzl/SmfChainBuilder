package com.gmail.lsetzl.testsong

import com.gmail.lsetzl.smfchainbuilder.{Section, SmfChainBuilder}

object TestSong extends App {
  SmfChainBuilder()
    .section(1, 0.0, 4.0)

    .section()
    .tempo(120)

    .allChannel
    .volume(100).pan(64).expression(100)

    .allTrack
    .velocity(100)

    .channel

    .channel
    .track
    .section(a).notes("c4d4e8 c4d4e8 g4e4d4c4 d4e4d8")

    .write("test.mid")
}
