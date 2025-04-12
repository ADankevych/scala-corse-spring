package kse.unit7.challenge

import java.util.UUID
import kse.unit7.challenge.model.Post.PostId
import kse.unit7.challenge.model.User.UserId

object model:

  type ApiKey = UUID

  object User:
    type UserId = UUID

  case class UserProfile(userId: UserId)

  object Post:
    type PostId = UUID

  case class Post(userId: UserId, postId: PostId)

  case class Comment(userId: UserId, postId: PostId)

  case class Like(userId: UserId, postId: PostId)

  case class Share(userId: UserId, postId: PostId)

  case class PostView(post: Post, comments: List[Comment], likes: List[Like], shares: List[Share])
