package kse.unit7.challenge

import kse.unit7.challenge.adt.*
import kse.unit7.challenge.model.*
import kse.unit7.challenge.services.*

object app:

  def getPostsViews(apiKey: ApiKey): Try[List[PostView]] =
    for {
      userProfile <- services.getUserProfile(apiKey)
      posts       <- services.getPosts(userProfile.userId)
      postViews   <- posts.map(getPostView).foldLeft(Try(List.empty[PostView]))((acc, postView) => acc.flatMap(list => postView.map(_ :: list))).map(_.reverse)
    } yield postViews

  def getPostsViewDesugared(apiKey: ApiKey): Try[List[PostView]] =
    getUserProfile(apiKey)
      .flatMap(userProfile =>
        services
          .getPosts(userProfile.userId)
          .flatMap(posts =>
            posts.map(getPostView).foldLeft(Try(List.empty[PostView]))((acc, postView) => acc.flatMap(list => postView.map(_ :: list))).map(_.reverse)
          )
      )

  def getPostView(post: Post): Try[PostView] =
    for {
      comments <- services.getComments(post.postId)
      likes    <- services.getLikes(post.postId)
      shares   <- services.getShares(post.postId)
    } yield PostView(post, comments, likes, shares)

  def getPostViewDesugared(post: Post): Try[PostView] =
    services
      .getComments(post.postId)
      .flatMap(comments =>
        services.getLikes(post.postId).flatMap(likes => services.getShares(post.postId).map(shares => PostView(post, comments, likes, shares)))
      )
