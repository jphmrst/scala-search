// Copyright (C) 2020 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.


package org.maraist.search.graph
import org.maraist.search.SearchFailureException

object Nodes {

  class CostAndStep[S](val cost: Double, val state: S) { }

  def liftHeuristic[S, N <: SearchTreeNode[N,S]](hs: (S) => Double): N => Double =
    (node: N) => hs.apply(node.state)

  // -----------------------------------------------------------------
  // Just a simple wrapper for the state.  No cost, no history

  abstract class SimpleCoreTreeNode[
    Self <: SimpleCoreTreeNode[Self, Exp, S], Exp, S
  ](
    val expander: (S) => Iterable[Exp], val state: S)
  extends SearchTreeNode[Self, S] {
    override def toString(): String =
      "[" + state.toString() + "]"
  }

  class SimpleTreeNode[S](expander: (S) => Iterable[S], state: S)
  extends SimpleCoreTreeNode[SimpleTreeNode[S],S,S](expander, state) {
    def expand(): Iterable[SimpleTreeNode[S]] =
      new Iterable[SimpleTreeNode[S]] {
        def iterator: Iterator[SimpleTreeNode[S]] =
          new Iterator[SimpleTreeNode[S]] {
            val node: SimpleTreeNode[S] = SimpleTreeNode.this
            val dests: Iterator[S] =
              node.expander.apply(node.state).iterator;
            override def hasNext: Boolean = dests.hasNext
            override def next(): SimpleTreeNode[S] = {
              val dest: S = dests.next()
              new SimpleTreeNode[S](expander,dest)
            }
          }
      }
  }
  object SimpleTreeNode {
    def initializer[S](expander: (S) => Iterable[S]): S => SimpleTreeNode[S] =
      (state: S) => new SimpleTreeNode[S](expander, state)
  }

  // -----------------------------------------------------------------
  // The state and the cost.

  abstract class SimpleCoreTreeCostNode[
    Self <: SimpleCoreTreeCostNode[Self, Exp, S],
    Exp <: CostAndStep[S], S
  ] (
    expander: (S) => Iterable[Exp], val cost: Double, state: S
  )
  extends SimpleCoreTreeNode[Self, Exp, S](expander, state)
  with KnowsOwnCost {
    override def toString(): String =
      "[" + state.toString() + "@" + cost + "]"
  }

  class SimpleTreeCostNode[S](
    expander: (S) => Iterable[CostAndStep[S]], cost: Double, state: S
  )
  extends SimpleCoreTreeCostNode[
    SimpleTreeCostNode[S], CostAndStep[S], S
  ](expander,cost,state) {
    def expand(): Iterable[SimpleTreeCostNode[S]] =
      new Iterable[SimpleTreeCostNode[S]] {
        def iterator = new Iterator[SimpleTreeCostNode[S]] {
          val node: SimpleTreeCostNode[S] = SimpleTreeCostNode.this
          val dests: Iterator[CostAndStep[S]] =
            node.expander.apply(node.state).iterator
          override def hasNext: Boolean = dests.hasNext
          override def next(): SimpleTreeCostNode[S] = {
            val cs: CostAndStep[S] = dests.next()
            val dest: S = cs.state
            val cost: Double = cs.cost
            new SimpleTreeCostNode[S](
              expander, SimpleTreeCostNode.this.cost+cost, dest)
          }
        }
      }
  }

  object SimpleTreeCostNode {
    def initializer[S](expander: (S) => Iterable[CostAndStep[S]]): S => SimpleTreeCostNode[S] =
      (state: S) => new SimpleTreeCostNode[S](expander, 0.0, state)
  }

  // -----------------------------------------------------------------
  // The state plus a pointer to the parent node in non-root nodes.

  abstract class SimpleCoreTreePathNode[
    Self <: SimpleCoreTreePathNode[Self,Exp,S], Exp, S
  ](
    expander: (S) => Iterable[Exp], val parent: Option[Self], state: S
  ) extends SimpleCoreTreeNode[Self, Exp, S](expander,state)
  with SearchTreePathNode[Self, S] {
    def this(expander: (S) => Iterable[Exp], state: S) =
      this(expander, None, state)
    def this(parent: Self, state: S) =
      this(parent.expander, Some(parent), state)
    override def toString(): String = "[" + pathToString() + "]"
  }

  class SimpleTreePathNode[S](expander: (S) => Iterable[S],
                              parent: Option[SimpleTreePathNode[S]],
                              state: S)
  extends SimpleCoreTreePathNode[SimpleTreePathNode[S],S, S](
    expander, parent, state
  ) {
    def this(expander: (S) => Iterable[S], state: S) =
      this(expander,None,state)
    def this(parent: SimpleTreePathNode[S], state: S) =
      this(parent.expander,Some(parent),state)
    val thisSimpleTreePathNode = this
    def expand(): Iterable[SimpleTreePathNode[S]] =
      new Iterable[SimpleTreePathNode[S]] {
        def iterator: Iterator[SimpleTreePathNode[S]] =
          new Iterator[SimpleTreePathNode[S]] {
            val node: SimpleTreePathNode[S] = thisSimpleTreePathNode
            val dests: Iterator[S] =
              node.expander.apply(node.state).iterator
            override def hasNext: Boolean = dests.hasNext
            override def next(): SimpleTreePathNode[S] = {
              val dest: S = dests.next()
              new SimpleTreePathNode[S](node,dest)
            }
          }
      }


    def initializer[S](expander: (S) => Iterable[S]): S => SimpleTreePathNode[S] =
      (state: S) => new SimpleTreePathNode[S](expander, state)
  }

  // -----------------------------------------------------------------
  // The state, the cost, and a pointer to the parent node.

  abstract class SimpleCoreTreePathCostNode[
    Self <: SimpleCoreTreePathCostNode[Self,Exp,S],
    Exp <: CostAndStep[S],
    S
  ](
    expander: (S) => Iterable[Exp], parent: Option[Self],
    val cost: Double, state: S
  ) extends SimpleCoreTreePathNode[Self, Exp, S](
    expander, parent, state
  ) with KnowsOwnCost {
    def this(expander: (S) => Iterable[Exp], cost: Double, state: S) =
      this(expander, None, cost, state)
    def this(parent: Self, cost: Double, state: S) =
      this(parent.expander, Some(parent), cost, state)

    override def toString(): String =
      "[" + pathToString() + "@" + cost + "]"
  }

  class SimpleTreePathCostNode[S](
    expander: (S) => Iterable[CostAndStep[S]],
    parent: Option[SimpleTreePathCostNode[S]], cost: Double, state: S
  ) extends SimpleCoreTreePathCostNode[
    SimpleTreePathCostNode[S], CostAndStep[S], S
  ](expander, parent, cost, state) {
    def this(expander: (S) => Iterable[CostAndStep[S]],
             cost: Double, state: S) =
               this(expander,None,cost,state)
    def this(parent: SimpleTreePathCostNode[S],
             cost: Double, state: S) =
               this(parent.expander,Some(parent),cost,state)

    def expand(): Iterable[SimpleTreePathCostNode[S]] =
      new Iterable[SimpleTreePathCostNode[S]] {
        def iterator: Iterator[SimpleTreePathCostNode[S]] =
          new Iterator[SimpleTreePathCostNode[S]]() {
            val node: SimpleTreePathCostNode[S] =
              SimpleTreePathCostNode.this
            val dests: Iterator[CostAndStep[S]] =
              node.expander.apply(node.state).iterator
            override def hasNext: Boolean = dests.hasNext
            override def next(): SimpleTreePathCostNode[S] = {
              val cs: CostAndStep[S] = dests.next()
              val dest: S = cs.state
              val cost: Double = cs.cost
              new SimpleTreePathCostNode[S](
                node, SimpleTreePathCostNode.this.cost + cost,
                dest)
            }
          }
      }
  }

  object SimpleTreePathCostNode {
    def initializer[S](expander: (S) => Iterable[CostAndStep[S]]): S => SimpleTreePathCostNode[S] =
      (state: S) => new SimpleTreePathCostNode[S](expander, 0.0, state)
  }
}
