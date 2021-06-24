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
import scala.collection.mutable.HashSet;

/**
 *  Methods required of a representation of an explored set.
 *
 *  The {@link org.maraist.search.graph.ExploredSet}
 *  companion object contains some standard
 *  implementations and utilities of this interface.
 *
 * @tparam Node The type of tree nodes stored in the explored set.
 *
 * @see GraphSearcher#search
 */
trait ExploredSet[Node] {

  /**
   * Called by the {@link GraphSearcher#search search} method with the
   * initial tree node.
   *
   * @param n The tree node to be noted
   */
  def noteInitial(n: Node): Unit

  /**
   * Called by the {@link GraphSearcher#search search} method with a
   * node when it is removed from the frontier for exploration.
   *
   * @param n The tree node to be noted
   */
  def noteExplored(n: Node): Unit

  /**
   * Called by the {@link GraphSearcher#search search} method to
   * determine whether a node should be added to the frontier.
   *
   * @param n The tree node to be tested
   */
  def shouldAddToFrontier(n: Node): Boolean
}

object ExploredSet {

  def doNotTrack[F,N](frontier: F): ExploredSet[N] =
    new ExploredSet[N] {
      override def noteExplored(n: N): Unit = { }
      override def noteInitial(n: N): Unit = { }
      override def shouldAddToFrontier(n: N): Boolean = true
    }

  def trackGeneratedByArtifactHashSet[F,N,A](artifactBuilder: (N) => A)(frontier: F): ExploredSet[N] =
    new ExploredSet[N] {
      val tracker: HashSet[A] = new HashSet[A]
      override def noteExplored(n: N): Unit = { }
      override def noteInitial(n: N): Unit = {
        val artifact: A = artifactBuilder(n)
        tracker.add(artifact)
      }
      override def shouldAddToFrontier(n: N): Boolean = {
        val artifact: A = artifactBuilder(n)
        val result: Boolean = !tracker.contains(artifact)
        if (result) { tracker.add(artifact) }
        result
      }
    }

  def trackStateByHashSet[F,S,N <: SearchTreeNode[N,S]]: (F) => ExploredSet[N] =
    trackGeneratedByArtifactHashSet((x) => x.state)
}
