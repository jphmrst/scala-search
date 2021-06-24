// Copyright (C) 2020 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.search

/**
 *  General trait describing a search algorithm.
 *
 * @tparam State Type representing elements of the search space.
 *
 * @tparam Node Type representing nodes in the search space.  Each
 * node typically contains a reference to a State element.
 */
trait Searcher[State, Node] {

  /**
   *  Executes a search beginning from a particular search space
   *  element.
   *
   * @param initial The starting element
   * @return The tree node corresponding to a goal element of the
   * search space which is returned from this search
   */
  def search(initial: State): Node

  /**
   *  Convenience method for when we care only about whether a
   *  solution exists, and not what it is.
   *
   * @param initial The starting element
   * @return <tt>true</tt> if {@link #search} would return a final
   * node with the same initial state, otherwise <tt>false</tt>
   */
  def solvable(initial: State): Boolean = {
    try {
      val result: Node = search(initial)
      return true;
    } catch {
      case (e: SearchFailureException) => return false;
    }
  }

  /**
   *  This method prints a debugging message about the initial tree
   *  node of a search.
   *
   * @param node The tree node in question.
   */
  def debugInitialNode(node: Node): Unit

  /**
   *  This method prints a debugging message about finding a goal
   *  node.
   *
   * @param node The tree node in question.
   */
  def debugGoalFound(node: Node): Unit
}
