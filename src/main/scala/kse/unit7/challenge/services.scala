package kse.unit7.challenge

import java.util.UUID
import kse.unit7.challenge.adt.Try
import kse.unit7.challenge.model.{ApiKey, Comment, Like, Post, Share, UserProfile}
import kse.unit7.challenge.model.Post.PostId
import kse.unit7.challenge.model.User.UserId

object services:

  // Do not implement the services
  def getUserProfile(apiKey: ApiKey): Try[UserProfile] = ???
  def getPosts(userId: UserId): Try[List[Post]]        = ???
  def getComments(postId: PostId): Try[List[Comment]]  = ???
  def getLikes(postId: UUID): Try[List[Like]]          = ???
  def getShares(postId: UUID): Try[List[Share]]        = ???
