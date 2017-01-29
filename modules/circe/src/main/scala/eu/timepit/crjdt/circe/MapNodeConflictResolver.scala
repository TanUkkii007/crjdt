package eu.timepit.crjdt.circe

import eu.timepit.crjdt.core.Key.StrK
import eu.timepit.crjdt.core.{Node, TypeTag}
import eu.timepit.crjdt.core.Node.MapNode
import io.circe.Json

trait MapNodeConflictResolver {

  def duplicateKeyMapToJson(node: Node.MapNode): Json
}

object MapNodeConflictResolverStrategies {

  object Strict extends MapNodeConflictResolver {
    override def duplicateKeyMapToJson(node: MapNode): Json = {
      val message = node.keySet.groupBy {
        case TypeTag.MapT(StrK(key)) => key
        case TypeTag.ListT(StrK(key)) => key
        case TypeTag.RegT(StrK(key)) => key
        case _ => ???
      }.foldLeft(StringBuilder.newBuilder) { (builder, keys) =>
          val (key, typeTags) = keys
          if (typeTags.size > 1) {
            val cmdString = typeTags.collect {
              case t: TypeTag.RegT =>
                node
                  .getChild(t)
                  .asInstanceOf[Node.RegNode]
                  .values
                  .map(v => s"""doc["$key"] := $v""")
                  .mkString("\n")
              case _: TypeTag.ListT => s"""doc["$key"] := []"""
              case _: TypeTag.MapT => s"""doc["$key"] := {}"""
            }.mkString("\n")
            builder.append(cmdString)
          } else {
            builder
          }
        }
        .toString()
      throw new Exception(
        "concurrent modification with different type structure values is prohibited: \n" + message)
    }
  }

  implicit object LWW extends MapNodeConflictResolver with NodeToJson {
    override def duplicateKeyMapToJson(node: Node.MapNode): Json = {
      implicit val rcr = RegNodeConflictResolver.LWW
      val noConflictChildren = node.keySet.groupBy {
        case TypeTag.MapT(StrK(key)) => key
        case TypeTag.ListT(StrK(key)) => key
        case TypeTag.RegT(StrK(key)) => key
        case _ => ???
      }.map {
        case (key, keySet) =>
          keySet
            .map(typeTag => typeTag -> node.getChild(typeTag))
            .head // ToDo: use operationID
      }
      mapToJson(node.copy(children = noConflictChildren))(rcr, this)
    }
  }
}
