package edu.gatech.eilab.scheherazade

import parse._
import java.io._
package data {
  class StorySet(
    val name: String,
    var storyList: List[Story]) extends XStreamable {
  }
}