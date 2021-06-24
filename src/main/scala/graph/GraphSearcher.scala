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
import org.maraist.search.Searcher
import org.maraist.search.SearchFailureException

/**
 *  Topmost class encapsulating graph search.  The
 *  {@link org.maraist.search.graph.GraphSearcher#search search} method
 *  implements the Graph-Search algorithm of Russell and Norvig (2nd
 *  ed., Figure 3.7, p. 77), with customizable behavior provided as
 *  constructor arguments.
 *
 * @tparam State Type representing elements of the search space.
 *
 * @tparam Node Type representing nodes in the search tree.  Each
 * node typically contains a reference to a State element.
 *
 * @tparam F Type representing the (entire) search frontier
 * (open set).
 *
 * @param goalCheckerFactory The
 * {@link java.util.function.Supplier#get get}
 * method of this object must
 * return a predicate on tree nodes used to tell if they are goal
 * nodes.
 *
 * @param frontierFactory The {@link java.util.function.Supplier#get
 * get} method of this object returns a new, empty F (Frontier)
 * instance.
 *
 * @param exploredSetFactory Structure used to manage adding
 * elements to the frontier, in particular for avoiing duplication.
 *
 * @param initializer Creates an initial tree node from a search
 * space element.
 */
open class GraphSearcher[State,
                         Node <: SearchTreeNode[Node,State],
                         F <: Frontier[Node]](
  val goalCheckerFactory: () => GoalChecker[Node],
  val frontierFactory: () => F,
  val exploredSetFactory: F => ExploredSet[Node],
  val initializer: State => Node
) extends Searcher[State, Node] {
  private var addedToFrontier: Long = -1
  private var notAddedToFrontier: Long = -1
  private var expandedFromFrontier: Long = -1
  private var unexpandedInFrontier: Long = -1

  /**
   *  Executes a search beginning from a particular search space
   *  element.
   *
   * @param initial The starting element
   * @return The tree node corresponding to a goal element of the
   * search space which is returned from this search
   */
  def search(initial: State): Node = {

    // Initialize the frontier and the root node of the search tree.
    val frontier: F = frontierFactory()
    val initialNode: Node = initializer.apply(initial)
    if (getDebug()) { debugInitialNode(initialNode) }
    frontier.add(initialNode)
    addedToFrontier = 1
    notAddedToFrontier = 0
    expandedFromFrontier = 0

    // Create a new, empty existing set.  In this framework the
    // existing set also checks memberhsip in the frontier.
    val exploredSet: ExploredSet[Node] = exploredSetFactory(frontier)
    exploredSet.noteInitial(initialNode)

    // Initialize the manager for reacting to expanding a node
    // containing a goal state.
    val goalChecker: GoalChecker[Node] = goalCheckerFactory()

    // While the frontier is not empty, choose a leaf node and remove
    // it from the frontier.
    if (getDebug()) { debugFrontier(frontier) }
    while (!frontier.isEmpty()) {
      val node: Node = frontier.pop()
      val state: State = node.state
      if (getDebug()) { debugFrontierRemoval(node) }

      // If the node contains a goal state (the goalChecker decided to
      // tell us so), then return the result.
      if (goalChecker.test(node)) {
        if (getDebug()) { debugGoalFound(node) }
        unexpandedInFrontier = frontier.countOpen()
        return node;
      }

      // Add this node to the explored set, as we are exploring it
      // right now.
      exploredSet.noteExplored(node);

      // Expand the node, and add the resulting nodes to the
      // frontier if they are not already in either the frontier or
      // the explored set.
      expandedFromFrontier = expandedFromFrontier + 1
      for(childNode <- node.expand()) {
        if (getDebug()) { debugExpansion(childNode) }

        if (exploredSet.shouldAddToFrontier(childNode)) {
          if (getDebug()) { debugFrontierAddition(childNode) }
          addedToFrontier = addedToFrontier + 1;
          frontier.add(childNode)
        } else {
          if (getDebug()) { debugFrontierNonaddition(childNode) }
          notAddedToFrontier = notAddedToFrontier + 1
        }
      }

      if (getDebug()) { debugFrontier(frontier) }
    }

    // If the goalChecker has been hoarding or otherwise processing
    // goals, let it give a final answer --- otherwise it can throw
    // the exception.
    if (getDebug()) { debugFrontierExhausted(goalChecker) }
    unexpandedInFrontier = frontier.countOpen()
    return goalChecker.get()
  }

  /**
   *  This method prints a debugging message about the initial tree
   *  node of a search.
   *
   * @param node The tree node in question.
   */
  override def debugInitialNode(node: Node): Unit =
    println("Initial node: " + node)

  /**
   *  This method prints a debugging message when a tree node is
   *  removed from the frontier for expansion.
   *
   * @param node The tree node in question.
   */
  def debugFrontierRemoval(node: Node): Unit =
    println("Popped node " + node)

  /**
   *  This method prints a debugging message when a tree node is
   *  generated from another node extracted from the frontier for
   *  expansion.
   *
   * @param node The tree node in question.
   */
  def debugExpansion(node: Node): Unit =
    println("- Expanded to " + node)

  /**
   *  This method prints a debugging message when a tree node is added
   *  to the frontier.
   *
   * @param node The tree node in question.  The default message
   * printed in the version of this method defined in this class does
   * not actually use this argument, since the tree node is already
   * printed in the default version of the {@link #debugExpansion}
   * method.
   */
  def debugFrontierAddition(node: Node) = println("  - Added")

  /**
   *  This method prints a debugging message when a tree node is
   *  <em>not</em> added to the frontier.
   *
   * @param node The tree node in question
   */
  def debugFrontierNonaddition(node: Node) = println("  - Not added")

  /**
   *  This method prints a debugging message when the frontier is
   *  emptied (which is usually an error situation).
   *
   * @param checker The goal checker function used in this
   * search.  This instance may be storing expanded nodes (to avoid
   * revisiting them), and so may have additional information we wish
   * to view.  However the default implementation of this method in
   * this parent class does not use this argument.
   */
  def debugFrontierExhausted(checker: GoalChecker[Node]) =
    System.out.println("Frontier exhausted")

  /**
   *
   */
  override def debugGoalFound(node: Node) = println("- Node is goal")

  /**
   *  Print debugging information about the frontier.  By default,
   *  invokes {@link Frontier#debugDisplayFrontier
   *  debugDisplayFrontier}.
   */
  def debugFrontier(f: F) = f.debugDisplayFrontier()

  /**
   *  Returns the number of frontier nodes which were added to the
   *  last search.
   * @return -1 if no search has been executed
   */
  def getLastAddedToFrontier(): Long = addedToFrontier

  /**
   *  Returns the number of frontier nodes which were generated from
   *  an expanded node, but <em>not</em> added to the last search.
   * @return -1 if no search has been executed
   */
  def getLastNotAddedToFrontier(): Long = notAddedToFrontier

  /**
   *  Returns the number of frontier nodes which were expanded in the
   *  last search.
   * @return -1 if no search has been executed
   */
  def getLastExpandedFromFrontier(): Long = expandedFromFrontier

  /**
   *  Returns the number of frontier nodes which were expanded in the
   *  last search.
   * @return -1 if no search has been executed
   */
  def getLastUnexpandedInFrontier(): Long = unexpandedInFrontier

  private var debug: Boolean = false

  /**
   *  Controls whether execution of the {@link #search} method should
   *  print debugging messages.
   */
  def setDebug(debug: Boolean): Unit = {
    this.debug = debug;
  }

  /**
   *  Returns whether execution of the {@link #search} method is
   *  printing debugging messages.
   */
  def getDebug(): Boolean = debug
}
