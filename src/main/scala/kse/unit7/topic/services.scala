package kse.unit7.topic

import kse.unit7.topic.adt.Option
import kse.unit7.topic.model.{ApiKey, BankAccount, Client, Transaction}
import kse.unit7.topic.model.BankAccount.BankAccountId
import kse.unit7.topic.model.Client.ClientId

object services:

  def getClient(apiKey: ApiKey): Option[Client]                             = ???
  def getBankAccounts(clientId: ClientId): Option[List[BankAccount]]        = ???
  def getCreditTransactions(from: BankAccountId): Option[List[Transaction]] = ???
  def getDebitTransactions(to: BankAccountId): Option[List[Transaction]]    = ???
