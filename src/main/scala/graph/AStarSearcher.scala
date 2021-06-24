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
 *  Extension of {@link AStarFrontierSearcher} to fix the frontier
 *  structure with a minimal priority queue implementation.
 *
 * @tparam State Type representing elements of the search space.
 *
 * @tparam Node Type representing nodes in the search tree.  Each
 * node typically contains a reference to a State element.
 *
 * @param goalTest A boolean-returning function checking whether a
 * tree node contains a goal state.
 *
 * @param heuristic Heuristic function for this search application.
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
class AStarSearcher[
  State, Node <: SearchTreeNode[Node,State] & KnowsOwnCost
](
  goalTest: GoalChecker[Node],
  heuristic: Node => Double,
  exploredSetFactory: Frontier.PriorityQueue[Node] => ExploredSet[Node],
  initializer: State => Node)
extends AStarFrontierSearcher[State, Node, Frontier.PriorityQueue[Node]](
  goalTest,
  heuristic,
  cmp => Frontier.priorityQueueFactory(cmp),
  exploredSetFactory,
  initializer
) {

  /**
  * Constructor for this class which does not maintain an explored
  * set.
  *
  * @param goalTest A boolean-returning function checking whether a
  * tree node contains a goal state.
  *
  * @param heuristic Heuristic function for this search application.
  *
  * @param initializer Creates an initial tree node from a search
  * space element.  Passed as-is to the primary constructor for this
  * class, and thence to superclasses.
  */
  def this(goalTest: GoalChecker[Node],
           heuristic: Node => Double,
           initializer: State => Node) =
             this(goalTest,
                  heuristic,
                  ((f : Frontier.PriorityQueue[Node])
                   => ExploredSet.doNotTrack[Frontier.PriorityQueue[Node], Node](f)),
                  initializer)
}

object AStarSearcher {

  /**
  *  A specialization of {@link AStarSearcher} to use a minimal
  *  implementation of unrelated search tree nodes (with a state and
  *  accumulated cost only), with the frontier implementation still
  *  exposed as a type parameter.
  *
  * @tparam State Type representing elements of the search space.
  *
  * @param stateTest A boolean-returning function checking whether
  * a state space element is a goal state.
  *
  * @param heuristic Heuristic function for this search application.
  *
  * @param hashArtifactBuilder Generates a hashable object from a
  * state element.
  *
  * @param expander Generates the successor states from some state,
  * each associated with a cost.
  *
  */
  class SimpleNodes[State](
    goalTest: GoalChecker[Nodes.SimpleTreeCostNode[State]],
    heuristic: Nodes.SimpleTreeCostNode[State] => Double,
    exploredSetFactory: Frontier.PriorityQueue[Nodes.SimpleTreeCostNode[State]]
                          => ExploredSet[Nodes.SimpleTreeCostNode[State]],
    initializer: State => Nodes.SimpleTreeCostNode[State])
  extends AStarSearcher[State, Nodes.SimpleTreeCostNode[State]](
    goalTest, heuristic, exploredSetFactory, initializer
  ) {

    /**
     * Constructor for this class which maintains an explored set.
     *
     * @param stateTest A boolean-returning function checking whether
     * a state space element is a goal state.
     *
     * @param heuristic Heuristic function for this search application.
     *
     * @param expander Generates the successor states from some state,
     * each associated with a cost.
     */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      hashArtifactBuilder: Nodes.SimpleTreeCostNode[State] => Object,
      expander: State => Iterable[Nodes.CostAndStep[State]]
    ) = this(
      GoalChecker.liftPredicate[State,Nodes.SimpleTreeCostNode[State]](stateTest),
      Nodes.liftHeuristic[State, Nodes.SimpleTreeCostNode[State]](heuristic),
      ((f : Frontier.PriorityQueue[Nodes.SimpleTreeCostNode[State]])
       => ExploredSet.trackGeneratedByArtifactHashSet(hashArtifactBuilder)(f)),
      Nodes.SimpleTreeCostNode.initializer[State](expander)
    )

    /**
     * Constructor for this class which does not maintain an explored
     * set.
     *
     * @param stateTest A boolean-returning function checking whether
     * a state space element is a goal state.
     *
     * @param heuristic Heuristic function for this search application.
     *
     * @param expander Generates the successor states from some state,
     * each associated with a cost.
     */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      expander: State => Iterable[Nodes.CostAndStep[State]]
    ) =
      this(GoalChecker.liftPredicate[State,Nodes.SimpleTreeCostNode[State]](stateTest),
           Nodes.liftHeuristic[State, Nodes.SimpleTreeCostNode[State]](heuristic),
           ((f : Frontier.PriorityQueue[Nodes.SimpleTreeCostNode[State]])
            => ExploredSet.doNotTrack[Frontier.PriorityQueue[Nodes.SimpleTreeCostNode[State]],
                                      Nodes.SimpleTreeCostNode[State]](f)),
           Nodes.SimpleTreeCostNode.initializer[State](expander))

  }

  /**
   *  A specialization of {@link AStarSearcher} to use a minimal
   *  implementation of hierarchical search tree nodes (with a state,
   *  accumulated cost, and pointer to a parent tree node), with the
   *  frontier implementation still exposed as a type parameter.
   *
   * @tparam State Type representing elements of the search space.
   */
  class PathNodes[State](
    goalTest: GoalChecker[Nodes.SimpleTreePathCostNode[State]],
    heuristic: Nodes.SimpleTreePathCostNode[State] => Double,
    exploredSetFactory: (Frontier.PriorityQueue[Nodes.SimpleTreePathCostNode[State]])
                          => ExploredSet[Nodes.SimpleTreePathCostNode[State]],
    initializer: State => Nodes.SimpleTreePathCostNode[State])
  extends AStarSearcher[State, Nodes.SimpleTreePathCostNode[State]](
    goalTest, heuristic, exploredSetFactory, initializer
  ) {

    /**
     * Constructor for this class which maintains an explored set.
     *
     * @param stateTest A boolean-returning function checking whether
     * a state space element is a goal state.
     *
     * @param heuristic Heuristic function for this search application.
     *
     * @param hashArtifactBuilder
     * Generates an artifact which can be used in a
     * hashtable to recall storage of a node.
     *
     * @param expander Generates the successor states from some state,
     * each associated with a cost.
     */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      hashArtifactBuilder: Nodes.SimpleTreePathCostNode[State] => Object,
      expander: State => Iterable[Nodes.CostAndStep[State]]
    ) = this(
      GoalChecker.liftPredicate[State,Nodes.SimpleTreePathCostNode[State]](stateTest),
      Nodes.liftHeuristic[State, Nodes.SimpleTreePathCostNode[State]](heuristic),
      ((f : Frontier.PriorityQueue[Nodes.SimpleTreePathCostNode[State]])
       => ExploredSet.trackGeneratedByArtifactHashSet(hashArtifactBuilder)(f)),
      Nodes.SimpleTreePathCostNode.initializer[State](expander)
    )

    /**
    * Constructor for this class which does not maintain an explored
    * set.
    *
    * @param stateTest A boolean-returning function checking whether
    * a state space element is a goal state.
    *
    * @param heuristic Heuristic function for this search application.
    *
    * @param expander Generates the successor states from some state,
    * each associated with a cost.
    */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      expander: State => Iterable[Nodes.CostAndStep[State]]
    ) = this(
      GoalChecker.liftPredicate[State,Nodes.SimpleTreePathCostNode[State]](stateTest),
      Nodes.liftHeuristic[State, Nodes.SimpleTreePathCostNode[State]](heuristic),
      ((f : Frontier.PriorityQueue[Nodes.SimpleTreePathCostNode[State]])
       => ExploredSet.doNotTrack[Frontier.PriorityQueue[Nodes.SimpleTreePathCostNode[State]],
                                 Nodes.SimpleTreePathCostNode[State]](f)),
      Nodes.SimpleTreePathCostNode.initializer[State](expander))
  }
}

