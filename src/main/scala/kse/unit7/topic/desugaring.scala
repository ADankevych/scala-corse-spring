package kse.unit7.topic

import kse.unit7.topic.adt.Option
import kse.unit7.topic.model.{ApiKey, BankAccount}
import kse.unit7.topic.model.BankAccount.BankAccountId
import kse.unit7.topic.model.Client.ClientId
import kse.unit7.topic.services.*

object desugaring:

  /*
    Only `map`-required case
    The only internal object is changed (from `Profile` to `UserId`)
   */
  def getClientId(apiKey: ApiKey): Option[ClientId] =
    for client <- getClient(apiKey)
    yield client.clientId

  def getClientIdDesugared(apiKey: ApiKey): Option[ClientId] =
    getClient(apiKey: ApiKey) map { client =>
      client.clientId
    }

  /*
   Only `flatMap`-required case
   Pipeline execution, the one service is called after another
   */
  def getClientBankAccounts(apiKey: ApiKey): Option[List[BankAccount]] =
    for
      client       <- getClient(apiKey)
      bankAccounts <- getBankAccounts(client.clientId)
    yield bankAccounts

  def getClientBankAccountsDesugared(apiKey: ApiKey): Option[List[BankAccount]] =
    getClient(apiKey) flatMap { client =>
      getBankAccounts(client.clientId)
    }

  /*
   Both `map`- and `flatMap`-required case
   Pipeline execution, the one service is called after another
   Then internal object is changed (from `List[Post]` to `Post`)
   */
  def getClientBankAccountIds(apiKey: ApiKey): Option[List[BankAccountId]] =
    for
      client       <- getClient(apiKey)
      bankAccounts <- getBankAccounts(client.clientId)
    yield bankAccounts map { _.bankAccountId }

  def getUserFirstPostsDesugared(apiKey: ApiKey): Option[List[BankAccountId]] =
    getClient(apiKey) flatMap { client =>
      getBankAccounts(client.clientId)
    } map { bankAccounts =>
      bankAccounts map { _.bankAccountId }
    }
