// Copyright (C) 2020 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.search.local
import org.maraist.search.Searcher
import org.maraist.search.SearchFailureException
import org.maraist.search.Debug.debugOn

/**
 *  Methods associated with the set of candidate solutions
 *  passed from one cycle of a beam search to the next.
 *
 * @tparam S Type of elements generated in the search.
 */
trait Beam[S] extends Iterable[S] {

  /**
   *  Return a particular element of the beam by its numeric
   *  index.
   */
  def apply(i: Int): S

  /**
   *  Return the number of elements in this beam.
   */
  def length: Int

  /**
   *  Return the number of passes up to this beam.
   */
  def generation: Int
}

/**
 *  Methods associated with constructing a set of candidate
 *  solutions to possibly pass to the next cycle of a beam
 *  search.
 *
 * @tparam S Elements generated in the search.
 *
 * @tparam B Beam passed from one cycle of search to the next.
 */
trait BeamBuilder[S, B <: Beam[S]] {

  /**
   *  Add an element to the beam.
   */
  def add(state: S): Unit

  /**
   *  Returns the number of elements which have been added to
   *  the beam.
   */
  def length: Int

  /**
   *  Completes and returns the under-construction beam.
   */
  def toBeam: B
}

/**
 *  Topmost class encapsulating beam search.
 *
 * @tparam S Elements generated in the search.
 *
 * @tparam B Beam passed from one cycle of search to the next.
 *
 * @param firstBeam Function which produces an initial beam from a
 * search element.
 *
 * @param nextBeam Function which either initializes a beam builder
 * from the result of the previous cycle, or instead indicates that
 * search could conclude.
 *
 * @param successors Function returning the successors of some search
 * element.
 *
 * @param extractor Function extracting the result of the search from
 * the final beam.
 */
open class BeamSearcher[State, B <: Beam[State]](
  val successors: State => Iterator[State],
  val firstBeam: State => B,
  val nextBeam: B => Option[BeamBuilder[State, B]],
  val extractor: B => State
) {

  /**
   * Perform beam search from the given starting state.
   *
   * @return The final state extracted from end of the search.
   */
  def search(initial: State): State = {
    val finalBeam = search(firstBeam(initial))
    if (finalBeam.length == 0)  throw new SearchFailureException
    extractor(finalBeam)
  }

  /**
   * Perform beam search from the given initial beam, and
   * return the last beam produced.
   */
  def search(srcBeam: B): B = {
    nextBeam(srcBeam) match {
      case None => srcBeam
      case Some(beam) => {
        for (st <- srcBeam)
          for (succ <- successors(st)) beam.add(succ)
        if (beam.length == 0)
          srcBeam
        else
          search(beam.toBeam)
      }
    }
  }
}

/**
 *  Type synonyms for the function types used in the {@link
 *  org.maraist.search.local.BeamSearcher} constructor arguments.
 */
object BeamSearcher {

  /**
   * Functions which produces an initial beam from a search element.
   */
  type FirstBeamFn[S,B <: Beam[S]] = S => B

  /**
   * Functions which either initializes a beam builder from the result
   * of the previous cycle, or instead indicates that search could
   * conclude.
   */
  type NextBeamFn[S,B <: Beam[S], BB <: BeamBuilder[S,B]] = B => Option[BB]

  /**
   * Functions which return the successors of some search element.
   */
  type SuccessorsFn[S] = S => Iterator[S]

  /**
   * Functions which extract the result of a search from the final
   * beam.
   */
  type ExtractorFn[S,B <: Beam[S]] = B => S
}

object BeamSearchConverters {
  given intToSBB[S]: Conversion[Int, StochasticBeamBuilder[S] => Int] with
    def apply(i: Int): StochasticBeamBuilder[S] => Int = (_) => i
  given scala.util.Random = new scala.util.Random
}
