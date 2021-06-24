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
import java.util.function.Predicate

/**
 *  Methods required of objects which check that a tree node
 *  corresponds to a search goal.
 *
 *  However these object can also allow the search process to delay
 *  selecting the found node until the end of a complete search; in
 *  this usage the {@link #get get} method should return the desired
 *  search result.
 *
 * @param <Node> Type representing nodes in the search tree.
 */
trait GoalChecker[Node] {

  /**
   *  For checking each node as it is removed from the frontier for
   *  expansion.
   */
  def test(n: Node): Boolean

  /**
   * When a search exhausts the frontier, this method can return the
   * result, or else should throw a
   * {@link org.maraist.search.SearchFailureException
   * SearchFailureException}.
   */
  def get(): Node
}

object GoalChecker {
  def firstGoal[Node](checker: GoalChecker[Node]): GoalChecker[Node] =
    new GoalChecker[Node] {
      override def get(): Node = throw new SearchFailureException()
      override def test(n: Node): Boolean = checker.test(n)
    }

  def goalCheckerFactory[S, N <: SearchTreeNode[N,S]](
    pred: (S) => Boolean
  ): () => GoalChecker[N] =
    () => new GoalChecker[N]() {
      override def test(n: N): Boolean = pred(n.state)
      override def get(): N = throw new SearchFailureException()
    }

  def liftPredicate[S, N <: SearchTreeNode[N,S]](pred: (S) => Boolean): GoalChecker[N] =
    new GoalChecker[N] {
      override def test(n: N): Boolean = pred(n.state)
      override def get(): N = throw new SearchFailureException()
    }
}

