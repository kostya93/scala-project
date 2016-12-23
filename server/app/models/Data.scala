package models

import shared.User

import scala.collection.mutable

/**
  * Created by kostya on 20.12.2016.
  */

object Data {
  def addUser(username: String, password: String) = {
    users += User(username, password)
  }

  def check(username: String, password: String): Boolean = {
    for (u <- users) {
      if (u.username == username && u.password == password) {
        return true
      }
    }
    false
  }

  def changeScore(user: User, dScore: Int): Unit = {
    for (u <- users) {
      if (u.username == user.username) {
        u.score += dScore
        return
      }
    }
  }

  def findByUserName(username: String): Option[User] = {
    users.find(user => user.username == username)
  }

  def sorted(): mutable.MutableList[User] = {
    users.sortWith(_.score > _.score)
  }

  val users: mutable.MutableList[User] = mutable.MutableList(User("test", "123"))
}
