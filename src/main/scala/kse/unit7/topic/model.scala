package kse.unit7.topic

import java.util.UUID
import kse.unit7.topic.model.BankAccount.BankAccountId
import kse.unit7.topic.model.Client.ClientId
import kse.unit7.topic.model.TransactionId.TransactionId

object model:

  opaque type ApiKey <: UUID = UUID

  object ApiKey:
    def apply(apiKey: ApiKey): ApiKey = apiKey

    def generate: ApiKey = ApiKey(UUID.randomUUID())

  object Client:
    opaque type ClientId <: UUID = UUID

    object ClientId:
      def apply(clientId: ClientId): ClientId = clientId

      def generate: ClientId = ClientId(UUID.randomUUID())

  case class Client(clientId: ClientId)

  object BankAccount:
    opaque type BankAccountId <: UUID = UUID

    object BankAccountId:
      def apply(bankAccountId: BankAccountId): BankAccountId = bankAccountId

      def generate: BankAccountId = BankAccountId(UUID.randomUUID())

  case class BankAccount(clientId: ClientId, bankAccountId: BankAccountId)

  object TransactionId:
    opaque type TransactionId <: UUID = UUID

    object TransactionId:
      def apply(transactionId: TransactionId): TransactionId = transactionId

      def generate: TransactionId = TransactionId(UUID.randomUUID())

  case class Transaction(from: BankAccountId, to: BankAccountId)

  case class ClientTransactions(clientId: ClientId, transactions: List[Transaction])
