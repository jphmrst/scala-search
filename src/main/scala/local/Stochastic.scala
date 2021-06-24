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
import scala.collection.immutable.HashMap
import scala.collection.mutable.TreeSet
import scala.util.Random
import scala.math.{log,exp}
import org.maraist.search.Searcher
import org.maraist.search.SearchFailureException
import org.maraist.search.Debug.debugOn

/**
 * Beam builder based on an objective function, but stochasticly keeping
 * certain non-optimized elements in the beam.
 *
 * @tparam S Type of elements of the search space.
 *
 * @param generation The generation number of this builder.
 *
 * @param stateCompare Imposes an order on search space elements,
 * where a more preferred element is ranked higher.
 *
 * @param retainProbability Function giving the relative probability
 * of retaining a search space element in the beam.
 *
 * @param beamLength Function mapping a beam builder to the length
 * of the next beam.
 *
 * @param retainOnOrder Function mapping a beam builder to the number
 * of elements of a beam which should be retained based on the
 * quality ordering alone, not subject to probabilistic retention.
 *
 * @param randoms A random number generator for this search.
 */
class StochasticBeamBuilder[S](
  val generation: Int,
  val stateCompare: Ordering[S],
  val retainProbability: S => Double,
  val beamLength: StochasticBeamBuilder[S] => Int,
  val retainOnOrder: StochasticBeamBuilder[S] => Int,
  val randoms: Random
)
extends BeamBuilder[S, StochasticBeam[S]] {
  /**
   *  Alternative constructor receiving only a
   *  {{org.maraist.search.local.StochasticBeam}}.
   *
   * @param sb The preceeding beam.
   */
  def this(sb: StochasticBeam[S]) =
    this(1 + sb.generation, sb.stateCompare, sb.retainProbability,
         sb.beamLength, sb.retainOnOrder, sb.randoms)

  /** Store for the items in this beam. */
  val store = TreeSet[S]()(stateCompare)
  /** Number of items stored in this beam. */
  var size: Int = 0
  /** Number of items stored in this beam. */
  def length: Int = size
  /** Add a search space element to this beam. */
  def add(state: S): Unit = {
    store += state
    size = size + 1
  }
  /** Return the beam we are building. */
  def toBeam = new StochasticBeam[S](this)
}

/**
 * Beam for search based on an objective function, but stochasticly
 * keeping certain non-optimized elements in the beam.
 *
 * @tparam S Type of elements of the search space.
 *
 * @param generation The genertion number of this builder.
 *
 * @param stateCompare Imposes an order on search space elements,
 * where a more preferred element is ranked higher.
 *
 * @param retainProbability Function giving the relative probability
 * of retaining a search space element in the beam.
 *
 * @param beamLength Function mapping a beam builder to the length
 * of the next beam.
 *
 * @param retainOnOrder Function mapping a beam builder to the number
 * of elements of a beam which should be retained based on the
 * quality ordering alone, not subject to probabilistic retention.
 *
 * @param randoms A random number generator for this search.
 *
 * @param store Elements to be stored in the beam, ordered from
 * least to most preferred.
 */
class StochasticBeam[S](val generation: Int,
                        val stateCompare: Ordering[S],
                        val retainProbability: S => Double,
                        val beamLength: StochasticBeamBuilder[S] => Int,
                        val retainOnOrder: StochasticBeamBuilder[S] => Int,
                        val randoms: Random,
                        val store: Seq[S])
extends Beam[S] {
  val length: Int = store.size

  def this(src: StochasticBeamBuilder[S]) =
    this(src.generation, src.stateCompare, src.retainProbability,
         src.beamLength, src.retainOnOrder, src.randoms, {
           val storeBuilder = Seq.newBuilder[S]

           val keepTop: Int = src.retainOnOrder(src)
           storeBuilder ++= src.store.takeRight(keepTop)

           val chooseLen: Int = src.size - keepTop
           val others = src.store.take(chooseLen)
           val scores: HashMap[S, Double] = {
             val builder = HashMap.newBuilder[S, Double]
             for (other <- others)
               builder += ((other,
                            src.retainProbability(other)
                             * src.randoms.nextDouble()))
             builder.result()
           }
           val othersSorted = new TreeSet[S]()(new SBOrder[S](scores))
           othersSorted ++= others
           storeBuilder ++= othersSorted.takeRight(
             src.beamLength(src) - keepTop
           )
           storeBuilder.result().sorted(src.stateCompare)
         })

  override def apply(i: Int): S = store(i)
  override def iterator: Iterator[S] = store.iterator
}

/**
  * Temporary workaround for scalac 3.0.0 bug preventing inlining this
  * class above --- see https://github.com/lampepfl/dotty/issues/12632
  */
private[local] class SBOrder[S](val scores: HashMap[S, Double])
    extends Ordering[S] {
  override def compare(x: S, y: S): Int = scores(x).compare(scores(y))
}

/**
 *  Generators for the function arguments to
 *  {@link org.maraist.search.local.BeamSearcher}
 *  needed for stochastic beam search.
 */
object StochasticBeam {
  import BeamSearcher.*

  def extractor[S]: ExtractorFn[S, StochasticBeam[S]] = (b => b.store.last)

  def firstBeam[S](
    stateCompare: Ordering[S],
    retainProbability: S => Double,
    beamLength: StochasticBeamBuilder[S] => Int,
    retainOnOrder: StochasticBeamBuilder[S] => Int,
    randoms: Random
  ): FirstBeamFn[S, StochasticBeam[S]] =
    state =>
      new StochasticBeam[S](0, stateCompare, retainProbability, beamLength,
                            retainOnOrder, randoms, Seq[S](state))

  type StocNextBeamFn[S] = NextBeamFn[S, StochasticBeam[S],
                                      StochasticBeamBuilder[S]]

  def quitAfterGenerations[S](limit: Int): StocNextBeamFn[S] =
    sb => if (sb.generation > limit) None
          else Some(new StochasticBeamBuilder[S](sb))

  def quitAtGoal[S](isGoal: S => Boolean): StocNextBeamFn[S] =
    sb => if (isGoal(sb.store.last)) None
          else Some(new StochasticBeamBuilder[S](sb))
}

/**
 *  Specialization of a
 *  {@linkplain org.maraist.search.local.BeamSearcher beam searcher}
 *  to a stochastic stochastic beam search, where some or all of the
 *  beam is drawn randomly from the possible successors.
 *
 * @tparam S Elements generated in the search.
 *
 * @param successors Function returning the successors of some
 * search element.
 *
 * @param stateCompare Determines the ordering between two states,
 * where a more preferred state is ordered greater than a less
 * preferred state.
 *
 * @param retainProbability Function calculating the retention
 * probability of a state.  It should be the case that if the
 * {@code stateCompare} ranks state {@code x} as greater than
 * {@code y}, then this function should return a greater value
 * for {@code x} than for {@code y} --- but this is not checked.
 *
 * @param nextBeam Function which either initializes a beam
 * builder from the result of the previous cycle, or instead
 * indicates that search could conclude.
 *
 * @param beamLength Function calculating the total size of the
 * next beam.
 *
 * @param retainOnOrder Function calculating the number of elements
 * of the next beam which should be taken directly from the best
 * scoring successors.
 *
 * @param randoms Random number generator for choosing from
 * successor beams.
 */
class StochasticBeamSearcher[S](
  successors: S => Iterator[S],
  stateCompare: Ordering[S],
  retainProbability: S => Double,
  nextBeam: StochasticBeam[S] => Option[StochasticBeamBuilder[S]],
  beamLength: StochasticBeamBuilder[S] => Int,
  retainOnOrder: StochasticBeamBuilder[S] => Int,
  randoms: Random
) extends BeamSearcher[S, StochasticBeam[S]](
  successors,
  StochasticBeam.firstBeam[S](stateCompare, retainProbability, beamLength,
                              retainOnOrder, randoms),
  nextBeam, StochasticBeam.extractor[S]) {

  /**
   *  Alternative constructor specifying an objective function whose
   *  value should be either maximized or minimized.  The ordering
   *  on states is constructed from these two arguments.
   *
   * @param evaluator Objective or penalty function mapping each
   * state to a real number.
   *
   * @param maximize {@code true} if search should maximize the
   * objective function; {@code false} if search should minimize
   * it.
   *
   * @param successors Function returning the successors of some
   * search element.
   *
   * @param nextBeam Function which either initializes a beam
   * builder from the result of the previous cycle, or instead
   * indicates that search could conclude.
   *
   * @param retainProbability Function calculating the retention
   * probability of a state.  It should be the case that if the
   * {@code stateCompare} ranks state {@code x} as greater than
   * {@code y}, then this function should return a greater value
   * for {@code x} than for {@code y} --- but this is not checked.
   *
   * @param beamLength Function calculating the total size of the
   * next beam.
   *
   * @param retainOnOrder Function calculating the number of elements
   * of the next beam which should be taken directly from the best
   * scoring successors.
   *
   * @param randoms Random number generator for choosing from
   * successor beams.
   */
  def this(
    evaluator: S => Double,
    maximize: Boolean,
    successors: S => Iterator[S],
    nextBeam: StochasticBeam[S] => Option[StochasticBeamBuilder[S]],
    retainProbability: S => Double,
    beamLength: StochasticBeamBuilder[S] => Int,
    retainOnOrder: StochasticBeamBuilder[S] => Int,
    randoms: Random
  ) = this(successors,
           if (maximize)
             new Ordering[S] {
               override def compare(x: S, y: S): Int =
                 evaluator(x).compare(evaluator(y))
             }
           else
             new Ordering[S] {
               override def compare(x: S, y: S): Int =
                 evaluator(y).compare(evaluator(x))
             },
           retainProbability, nextBeam, beamLength, retainOnOrder, randoms)

  /**
   *  Alternative constructor specifying an objective function whose
   *  value should be either maximized or minimized.  The ordering
   *  on states and the calculator for the relative probability of
   *  selecting a successor state are both constructed from this
   *  these two arguments.
   *
   * @param evaluator Objective or penalty function mapping each
   * state to a real number.  This function should never return
   * a negative number: this constructor uses
   * logarithmic/exponential functions to calculate the probability
   * that a successor state is retained in the beam, and negative
   * evaluations may raise exceptions.
   *
   * @param maximize {@code true} if search should maximize the
   * objective function; {@code false} if search should minimize
   * it.
   *
   * @param successors Function returning the successors of some
   * search element.
   *
   * @param nextBeam Function which either initializes a beam
   * builder from the result of the previous cycle, or instead
   * indicates that search could conclude.
   *
   * @param beamLength Function calculating the total size of the
   * next beam.
   *
   * @param retainOnOrder Function calculating the number of elements
   * of the next beam which should be taken directly from the best
   * scoring successors.
   *
   * @param randoms Random number generator for choosing from
   * successor beams.
   */
  def this(
    evaluator: S => Double,
    maximize: Boolean,
    successors: S => Iterator[S],
    nextBeam: StochasticBeam[S] => Option[StochasticBeamBuilder[S]],
    beamLength: StochasticBeamBuilder[S] => Int,
    retainOnOrder: StochasticBeamBuilder[S] => Int,
    randoms: Random
  ) = this(successors,
           if (maximize)
             new Ordering[S] {
               override def compare(x: S, y: S): Int =
                 evaluator(x).compare(evaluator(y))
             }
           else
             new Ordering[S] {
               override def compare(x: S, y: S): Int =
                 evaluator(y).compare(evaluator(x))
             },
           if (maximize)
             (s: S) => log(evaluator(s))
           else
             (s: S) => exp(- evaluator(s)),
           nextBeam, beamLength, retainOnOrder, randoms)
}
