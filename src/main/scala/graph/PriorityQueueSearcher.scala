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

import java.util.Comparator

/**
 * Specialization of the generic
 * {@link org.maraist.search.graph.GraphSearcher} to use a
 * priority queue structure for its frontier.
 *
 * @tparam State Type representing elements of the search space.
 *
 * @tparam Node Type representing nodes in the search tree.  Each
 * node typically contains a reference to a State element.
 *
 * @tparam Frontier Type representing the (entire) search frontier
 * (open set).
 *
 * @param goalCheckerFactory The
 * {@link java.util.function.Supplier#get get} method of this object must
 * return a predicate on tree nodes used to tell if they are goal
 * nodes.
 *
 * @param frontierFactory The {@link java.util.function.Supplier#get
 * get} method of this object returns a new, empty Frontier
 * instance.  Note that the type bound on <tt>Factory</tt> requires
 * that the generated objects conform to
 * {@link Frontier.PriorityQueue Frontiers.PriorityQueue[Node]}.
 *
 * @param exploredSetFactory Structure used to manage adding
 * elements to the frontier, in particular for avoiing duplication.
 *
 * @param initializer Creates an initial tree node from a search
 * space element.
 */
open class PriorityQueueSearcher[
  State,
  Node <: SearchTreeNode[Node,State],
  Front <: Frontier.PriorityQueue[Node]
](
  goalCheckerFactory: () => GoalChecker[Node],
  frontierFactory: () => Front,
  exploredSetFactory: Front => ExploredSet[Node],
  initializer: State => Node
)
extends GraphSearcher[State, Node, Front](
  goalCheckerFactory, frontierFactory, exploredSetFactory, initializer
) {

  /**
   *  This constructor allows the comparison function to be specified
   *  separately from the frontier-creation process.
   *
   * @param goalCheckerFactory The
   * {@link java.util.function.Supplier#get get} method of this object must
   * return a predicate on tree nodes used to tell if they are goal
   * nodes.  Passed as-is to the primary constructor for this class,
   * and thence to the {@linkplain GraphSearcher parent} constructor.
   *
   * @param prioritizer The method by which the priority queue ranks
   * tree nodes.  In Java's {@link java.util.PriorityQueue PriorityQueue} implementation, the next element to be removed is
   * the one ranked <em>least</em> by the {@link java.util.Comparator}.
   *
   * @param frontierMetafactory This function maps a
   * {@link java.util.Comparator} for tree nodes to a
   * {@link java.util.function.Supplier Supplier}
   * of new, empty Frontier instances.
   *
   * @param exploredSetFactory Structure used to manage adding
   * elements to the frontier, in particular for avoiing duplication.
   * Passed as-is to the primary constructor for this class, and
   * thence to the {@linkplain GraphSearcher parent} constructor.
   *
   * @param initializer Creates an initial tree node from a search
   * space element.  Passed as-is to the primary constructor for this
   * class, and thence to the {@linkplain GraphSearcher parent}
   * constructor.
   */
  def this(
    goalCheckerFactory: () => GoalChecker[Node],
    prioritizer: Comparator[Node],
    frontierMetafactory: Comparator[Node] => () => Front,
    exploredSetFactory: Front => ExploredSet[Node],
    initializer: State => Node
  ) = this(goalCheckerFactory, frontierMetafactory.apply(prioritizer),
           exploredSetFactory, initializer)
}

