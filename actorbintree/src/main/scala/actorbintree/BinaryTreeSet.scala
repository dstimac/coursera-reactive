/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import scala.util.Random

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => root ! op
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC =>
    case op: Operation => stash()
//      pendingQueue = pendingQueue.enqueue(op)
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot

      unstashAll()
//      while(!pendingQueue.isEmpty) {
//        val (op, rest) = pendingQueue.dequeue
//        pendingQueue = rest
//        root ! op
//      }

      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case op: Contains =>
//      (op.elem == this.elem, this.removed) match {
//        case (x, false) => op.requester ! ContainsResult(op.id, true)
//        case _ => op.requester ! ContainsResult(op.id, false)
//      }
      if(op.elem == elem) {
          op.requester ! ContainsResult(op.id, !removed)
      } else if(op.elem > elem) {
        subtrees.get(Right) match {
          case Some(r) => r ! op
          case _ => op.requester ! ContainsResult(op.id, false)
        }
      } else if(op.elem < elem) {
        subtrees.get(Left) match {
          case Some(l) => l ! op
          case _ => op.requester ! ContainsResult(op.id, false)
        }
      }
    case op: Insert =>
      if(op.elem == elem) {
        removed = false
        op.requester ! OperationFinished(op.id)
      } else if(op.elem < elem) {
        subtrees.get(Left) match {
          case Some(l) => l ! op
          case _ =>
            subtrees = subtrees + (Left -> spawn(op.id, op.elem))
            op.requester ! OperationFinished(op.id)
        }
      } else if(op.elem > elem ) {
        subtrees.get(Right) match {
          case Some(r) => r ! op
          case _ =>
            subtrees =  subtrees + (Right -> spawn(op.id, op.elem))
            op.requester ! OperationFinished(op.id)
        }
      }
    case op: Remove =>
      if(op.elem == elem) {
        removed = true
        op.requester ! OperationFinished(op.id)
      } else if(op.elem < elem) {
        subtrees.get(Left) match {
          case Some(l) => l ! op
          case _ => op.requester ! OperationFinished(op.id)
        }
      } else if (op.elem > elem) {
        subtrees.get(Right) match {
          case Some(r) => r ! op
          case _ => op.requester ! OperationFinished(op.id)
        }
      }
    case CopyTo(dest) => copyTo(dest)
//    case res: ContainsResult => context.parent ! res
//    case res: OperationFinished => context.parent ! res
  }

  def copyTo(dest: ActorRef) = {
    val children: Set[ActorRef] = subtrees.values.toSet
    if(removed && children.isEmpty) {
      context.parent ! CopyFinished
    } else {
      if(!removed) dest ! Insert(self, -1, elem)
      children foreach(_ ! CopyTo(dest))
      context.become(copying(children, removed))
    }
    //      (removed, children) match {
    //        case (true, c) if c.isEmpty => context.parent ! CopyFinished
    //        case _ =>
    //          context.become(copying(children, removed))
    //          children foreach (_ ! CopyTo(dest))
    //          if(!removed) dest ! Insert(self, randID, elem)
    //      }
  }

  private def spawn(id: Int, elem: Int) =
    context.actorOf(BinaryTreeNode.props(elem, false))

  private def randID = (scala.math.abs(Random.nextInt(1000)) + 1000) * (-1)

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished =>
      val newSet = expected - sender
      (newSet.size, insertConfirmed) match {
//        case (1, true) => context.parent ! CopyFinished
        case (0, true) =>
          context.parent ! CopyFinished
        case _ => context.become(copying(newSet, insertConfirmed))
    }
    case OperationFinished(-1) =>
      expected.size match {
        case 0 =>
          context.parent ! CopyFinished
        case _ => context.become(copying(expected, true))
      }
  }


}
