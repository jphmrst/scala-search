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
 *  Extension of {@link org.maraist.search.graph.PriorityQueueSearcher} with A*'s
 *  prioritization formula <i>f(n) = g(n)+h(n)</i>, still leaving the
 *  exact structure of the frontier as a configurable option.
 *
 * @tparam State Type representing elements of the search space.
 *
 * @tparam Node Type representing nodes in the search tree.  Each
 * node typically contains a reference to a State element.
 *
 * @tparam Frontier Type representing the (entire) search frontier
 * (open set).
 *
 * @param goalTest A boolean-returning function checking whether a
 * tree node contains a goal state.
 *
 * @param heuristic Heuristic function for this search application.
 *
 * @param frontierMetafactory This function maps a
 * {@link java.util.Comparator} for tree nodes to a
 * {@link java.util.function.Supplier Supplier} of new, empty Frontier
 * instances.
 *
 * @param exploredSetFactory Structure used to manage adding
 * elements to the frontier, in particular for avoiing duplication.
 * Passed as-is to the primary constructor for this class, and
 * thence to the {@linkplain org.maraist.search.graph.GraphSearcher parent} constructor.
 *
 * @param initializer Creates an initial tree node from a search
 * space element.  Passed as-is to the primary constructor for this
 * class, and thence to the {@linkplain org.maraist.search.graph.GraphSearcher parent}
 * constructor.
 */
open class AStarFrontierSearcher[
  State,
  Node <: SearchTreeNode[Node,State] & KnowsOwnCost,
  Front <: Frontier.PriorityQueue[Node]
](goalTest: GoalChecker[Node],
  val heuristic: Node => Double,
  frontierMetafactory: Comparator[Node] => () => Front,
  exploredSetFactory: Front => ExploredSet[Node],
  initializer: State => Node
)
extends PriorityQueueSearcher[State, Node, Front](
  () => GoalChecker.firstGoal(goalTest),
  new Comparator[Node]() {
    override def compare(n1: Node, n2: Node): Int = {
      val diff: Double = (n2.cost + heuristic(n2)
                          - n1.cost - heuristic(n1))
      if (diff<0) {
        return 1
      } else if (diff>0) {
        return -1
      } else {
        return 0
      }
    }
  },
  frontierMetafactory, exploredSetFactory, initializer
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
   * @param frontierMetafactory This function maps a
   * {@link java.util.Comparator} for tree nodes to a
   * {@link java.util.function.Supplier Supplier} of new, empty Frontier
   * instances.
   *
   * @param initializer Creates an initial tree node from a search
   * space element.  Passed as-is to the primary constructor for this
   * class, and thence to the
   * {@linkplain org.maraist.search.graph.GraphSearcher parent}
   * constructor.
   */
  def this(
    goalTest: GoalChecker[Node],
    heuristic: Node => Double,
    frontierMetafactory: Comparator[Node] => () => Front,
    initializer: State => Node
  ) =
    this(goalTest, heuristic, frontierMetafactory,
         ExploredSet.doNotTrack[Front, Node], initializer)

  /** {@inheritDoc} */
  override def debugFrontierRemoval(node: Node): Unit =
    println("Popped node " + node  + " h=" + heuristic(node))

  /** {@inheritDoc} */
  override def debugFrontierAddition(node: Node): Unit =
    println("  - Adding with h=" + heuristic(node))
}

object AStarFrontierSearcher {

  /**
   *  A specialization of {@link org.maraist.search.graph.AStarFrontierSearcher} to use a
   *  minimal implementation of unrelated search tree nodes (with a
   *  state and accumulated cost only), with the frontier
   *  implementation still exposed as a type parameter.
   *
   * @tparam State Type representing elements of the search space.
   *
   * @tparam Frontier Type representing the (entire) search frontier
   * (open set).
   */
  class SimpleNodes[
    State,
    Front <: Frontier.PriorityQueue[Nodes.SimpleTreeCostNode[State]]
  ](
    goalTest: GoalChecker[Nodes.SimpleTreeCostNode[State]],
    heuristic: Nodes.SimpleTreeCostNode[State] => Double,
    frontierMetafactory: Comparator[Nodes.SimpleTreeCostNode[State]] => () => Front,
    exploredSetFactory: Front => ExploredSet[Nodes.SimpleTreeCostNode[State]],
    initializer: State => Nodes.SimpleTreeCostNode[State]
  )
  extends AStarFrontierSearcher[State,
                                Nodes.SimpleTreeCostNode[State],
                                Front](
                                  goalTest, heuristic,
                                  frontierMetafactory,
                                  exploredSetFactory,
                                  initializer)
  {


    /**
     * Constructor for this class which does not maintain an explored
     * set.
     *
     * @param stateTest A boolean-returning function checking whether
     * a state space element is a goal state.
     *
     * @param heuristic Heuristic function for this search application.
     *
     * @param frontierMetafactory This function maps a
     * {@link java.util.Comparator} for tree nodes to a {@link java.util.function.Supplier Supplier} of new, empty Front
     * instances.
     *
     * @param expander Generates the successor states from some state,
     * each associated with a cost.
     */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      frontierMetafactory: Comparator[Nodes.SimpleTreeCostNode[State]] => () => Front,
      expander: State => Iterable[Nodes.CostAndStep[State]]
    ) =
      this(GoalChecker.liftPredicate[State,Nodes.SimpleTreeCostNode[State]](stateTest),
           Nodes.liftHeuristic[State,
                               Nodes.SimpleTreeCostNode[State]](heuristic),
           frontierMetafactory,
           f => ExploredSet.doNotTrack[Front,Nodes.SimpleTreeCostNode[State]](f),
           Nodes.SimpleTreeCostNode.initializer[State](expander))


    /**
    * Constructor for this class which maintains an explored
    * set using a hashing of the state representations.
    *
    * @param stateTest A boolean-returning function checking whether
    * a state space element is a goal state.
    *
    * @param heuristic Heuristic function for this search application.
    *
    * @param frontierMetafactory This function maps a
    * {@link java.util.Comparator} for tree nodes to a
    * {@link java.util.function.Supplier Supplier} of new, empty Front
    * instances.
    *
    * @param hashArtifactBuilder Generates a hashable object from a
    * state element.
    *
    * @param expander Generates the successor states from some state,
    * each associated with a cost.
    */
    def this(stateTest: State => Boolean,
             heuristic: State => Double,
             frontierMetafactory: Comparator[Nodes.SimpleTreeCostNode[State]] => () => Front,
             hashArtifactBuilder: Nodes.SimpleTreeCostNode[State] => Object,
             expander: State => Iterable[Nodes.CostAndStep[State]]) =
               this(GoalChecker.liftPredicate[State,Nodes.SimpleTreeCostNode[State]](stateTest),
                    Nodes.liftHeuristic[State, Nodes.SimpleTreeCostNode[State]](heuristic),
                    frontierMetafactory,
                    f => ExploredSet.trackGeneratedByArtifactHashSet[
                      Front, Nodes.SimpleTreeCostNode[State], Object
                    ](hashArtifactBuilder)(f),
                    Nodes.SimpleTreeCostNode.initializer[State](expander))
  }

  /**
   *  A specialization of {@link org.maraist.search.graph.AStarFrontierSearcher} to use a
   *  minimal implementation of hierarchical search tree nodes (with a
   *  state, accumulated cost, and pointer to a parent tree node),
   *  with the frontier implementation still exposed as a type
   *  parameter.
   *
   * @param [State] Type representing elements of the search space.
   * @param <Frontier> Type representing the (entire) search frontier
   * (open set).
   */
  class PathNodes[
    State,
    Front <: Frontier.PriorityQueue[Nodes.SimpleTreePathCostNode[State]]
  ](
    goalTest: GoalChecker[Nodes.SimpleTreePathCostNode[State]],
    heuristic: Nodes.SimpleTreePathCostNode[State] => Double,
    frontierMetafactory: Comparator[Nodes.SimpleTreePathCostNode[State]] => () => Front,
    exploredSetFactory: Front => ExploredSet[Nodes.SimpleTreePathCostNode[State]],
    initializer: State => Nodes.SimpleTreePathCostNode[State]
  )
  extends AStarFrontierSearcher[
    State, Nodes.SimpleTreePathCostNode[State], Front
  ](goalTest, heuristic, frontierMetafactory, exploredSetFactory, initializer
  ) {

    /**
    * Constructor for this class which maintains an explored
    * set using a hashing of the state representations.
    *
    * @param stateTest A boolean-returning function checking whether
    * a state space element is a goal state.
    *
    * @param heuristic Heuristic function for this search application.
    *
    * @param frontierMetafactory This function maps a
    * {@link java.util.Comparator} for tree nodes to a
    * {@link java.util.function.Supplier Supplier} of new, empty Front
    * instances.
    *
    * @param hashArtifactBuilder Generates a hashable object from a
    * state element.
    *
    * @param expander Generates the successor states from some state,
    * each associated with a cost.
    */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      frontierMetafactory: Comparator[Nodes.SimpleTreePathCostNode[State]] => () => Front,
      hashArtifactBuilder: Nodes.SimpleTreePathCostNode[State] => Object,
      expander: State => Iterable[Nodes.CostAndStep[State]]
    ) =
      this(
        GoalChecker.liftPredicate[State,Nodes.SimpleTreePathCostNode[State]](stateTest),
        Nodes.liftHeuristic[State, Nodes.SimpleTreePathCostNode[State]](heuristic),
        frontierMetafactory,
        (f => ExploredSet.trackGeneratedByArtifactHashSet[
            Front, Nodes.SimpleTreePathCostNode[State], Object
          ](hashArtifactBuilder)(f)),
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
    * @param frontierMetafactory This function maps a
    * {@link java.util.Comparator} for tree nodes to a
    * {@link java.util.function.Supplier Supplier} of new, empty Front
    * instances.
    *
    * @param expander Generates the successor states from some state,
    * each associated with a cost.
    */
    def this(
      stateTest: State => Boolean,
      heuristic: State => Double,
      frontierMetafactory: Comparator[Nodes.SimpleTreePathCostNode[State]] => () => Front,
      expander: State => Iterable[Nodes.CostAndStep[State]]) =
        this(GoalChecker.liftPredicate[State,Nodes.SimpleTreePathCostNode[State]](stateTest),
             Nodes.liftHeuristic[State, Nodes.SimpleTreePathCostNode[State]](heuristic),
             frontierMetafactory,
             ExploredSet.doNotTrack[Front, Nodes.SimpleTreePathCostNode[State]](_),
             Nodes.SimpleTreePathCostNode.initializer[State](expander))
  }
}
