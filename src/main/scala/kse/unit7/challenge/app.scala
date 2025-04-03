package kse.unit7.challenge

import kse.unit7.challenge.adt.*
import kse.unit7.challenge.model.*
import kse.unit7.challenge.services.*

object app:

  def getPostsViews(apiKey: ApiKey): Try[List[PostView]] =
    services.getUserProfile(apiKey).flatMap { userProfile =>
      services.getPosts(userProfile.userId).flatMap { posts =>
        posts
          .foldLeft(Try(List.empty[PostView])) { (acc, post) =>
            for {
              list <- acc
              view <- getPostView(post)
            } yield view :: list
          }
          .map(_.reverse)
      }
    }

  def getPostsViewDesugared(apiKey: ApiKey): Try[List[PostView]] =
    services
      .getUserProfile(apiKey)
      .flatMap(userProfile =>
        services
          .getPosts(userProfile.userId)
          .flatMap(posts => posts.foldLeft(Try(List.empty[PostView]))((acc, post) => acc.flatMap(list => getPostView(post).map(_ :: list))).map(_.reverse))
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
