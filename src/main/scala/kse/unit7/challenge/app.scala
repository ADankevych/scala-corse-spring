package kse.unit7.challenge

import kse.unit7.challenge.adt.*
import kse.unit7.challenge.model.*
import kse.unit7.challenge.services.*

object app:

  def getPostsViews(apiKey: ApiKey): Try[List[PostView]] = ???

  def getPostsViewDesugared(apiKey: ApiKey): Try[List[PostView]] = ???

  def getPostView(post: Post): Try[PostView] = ???

  def getPostViewDesugared(post: Post): Try[PostView] = ???
