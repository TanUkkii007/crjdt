package eu.timepit.crjdt.circe

import eu.timepit.crjdt.core.Node.ListNode
import io.circe.Json

trait ListNodeConflictResolver {
  def duplicateIndexListToJson(node: ListNode): Json
}

object ListNodeConflictResolverStrategies {
//  implicit object LWW extends ListNodeConflictResolver {
//    override def duplicateIndexListToJson(node: ListNode): Json = {
//      node
//    }
//  }
}
