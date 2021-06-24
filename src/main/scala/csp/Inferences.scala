// Copyright (C) 2020 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.search.csp
import org.maraist.search.Searcher
import org.maraist.search.SearchFailureException

/**
 * Representation of inference drawn from a recent binding of assignments,
 * but not necessarily yet deployed into the
 * {@link org.maraist.search.csp.AssignmentSet}
 * from which it was derived.
 */
trait Inferences[Var, Val, AS <: AssignmentSet[Var, Val, AS]] {
  def isFailure: Boolean
}

class NoInference[Var, Val, AS <: AssignmentSet[Var, Val, AS]]
extends Inferences[Var, Val, AS] {
  override def isFailure: Boolean = false
}

object NoInference {
  def apply[Var, Val, AS <: AssignmentSet[Var, Val, AS]] =
    new NoInference[Var, Val, AS]
}

