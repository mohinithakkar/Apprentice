package edu.gatech.eilab.scheherazade

import java.io._
package data {
  class StorySet(
    val name: String,
    var storyList: List[Story]) extends XStreamable {
  }
}