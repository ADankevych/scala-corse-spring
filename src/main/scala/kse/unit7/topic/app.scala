package kse.unit7.topic

import kse.unit7.topic.adt.Option
import kse.unit7.topic.model.{ApiKey, ClientTransactions, Transaction}
import kse.unit7.topic.model.BankAccount.BankAccountId
import kse.unit7.topic.services.*

object app:

  def getClientAccounts(apiKey: ApiKey): Option[ClientTransactions] =
    for
      client       <- getClient(apiKey)
      bankAccounts <- getBankAccounts(client.clientId)
      transactions <- getTransaction(bankAccounts.map(_.bankAccountId))
    yield ClientTransactions(client.clientId, transactions)

  def getTransaction(bankAccountIds: List[BankAccountId]): Option[List[Transaction]] =
    bankAccountIds.map(id => getTransactionsBy(id)).foldRight(Option(List.empty[Transaction])) {
      case (Option.Some(transactions), acc) => acc.map(_ ::: transactions)
      case (Option.None, acc)               => acc
    }

  def getTransactionsBy(bankAccountId: BankAccountId): Option[List[Transaction]] =
    for
      creditTransactions <- getCreditTransactions(bankAccountId)
      debitTransactions  <- getDebitTransactions(bankAccountId)
    yield creditTransactions ::: debitTransactions
