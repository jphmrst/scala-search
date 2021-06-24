// Copyright (C) 2020, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.search.csp
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import org.maraist.search.SearchFailureException
import org.maraist.search.Debug.debugOn

/**
 * Mutable hashtable-based partial implementation of variable-value
 * bindings associated with a particular CSP solution.
 *
 * @tparam Var Type representing variables in the CSP.
 *
 * @tparam Val Type representing values in the CSP.
 *
 * @tparam AS Actual implementation type for this trait.
 */
abstract class AbstractHashAssignmentSet[
  Var, Val, AS <: AbstractHashAssignmentSet[Var, Val, AS]
] (
  val csp: Problem[Var, Val],
  private var failed: Boolean = false
) extends AssignmentSet[Var, Val, AS] {

  private val bindings = new HashMap[Var, Val]
  private var unset = HashSet.from(csp.variables)

  /**
   *  Returns {@code true} if this set corresponds to search failure.
   */
  def unsetVariables: Set[Var] = unset.toSet

  /**
   *  Returns {@code true} if this set corresponds to search failure.
   */
  def isFailure: Boolean = failed

  /**
   *  Returns {@code true} if all variables are assigned.
   */
  def isComplete: Boolean = unset.isEmpty

  /**
   * Returns the (immutable) collection of variables which have been
   * bound to a value.
   */
  def boundSet: Set[Var] = bindings.keySet.toSet

  import scala.util.control.NonLocalReturns.*
  /**
   * Returns {@code true} if the given variable-value assignment
   * would be consistent.
   */
  def isConsistent(variable: Var, value: Val): Boolean = returning {
    if (debugOn)
      println("- Checking addition " ++ variable.toString()
              ++ " <- " ++ value.toString())
    for (constraint <- csp.constraints) {
      val checkResult = constraint.checkWith(this, variable, value)
      if (debugOn)
        println("  + Checking " ++ constraint.toString()
                ++ ": " ++ checkResult.toString())
      if (!checkResult) {
        throwReturn(false)
      }
    }

    // Result if we have not returned false
    true
  }

  /**
   * Returns {@code true} if the given variable-value assignment
   * would be consistent.
   */
  def setFailed(flag: Boolean) = { failed = flag }

  /**
   *  Returns the {@code AssignmentSet} obtained by making the given
   *  variable-value binding to this set.
   */
  def add(variable: Var, value: Val): AS = {
    if (debugOn)
      printf("  Binding %s to %s\n", variable.toString(), value.toString())
    bindings += ((variable, value))
    unset -= variable
    self
  }

  /**
   *  Returns the {@code AssignmentSet} obtained by removing the given
   *  variable-value binding from this set.
   */
  def remove(variable: Var, value: Val): Unit = {
    if (debugOn)
      printf("  Revoking binding of %s to %s\n",
             variable.toString(), value.toString())
    bindings -= variable
    unset += variable
  }

  /**
   *  Retrieve the value bound to the given variable, if one exists.
   */
  def apply(variable: Var): Option[Val] = bindings.get(variable)
}

/**
 * Simple implementation of hashmap-based
 * {@link org.maraist.search.csp.AssignmentSet}s
 */
class HashAssignmentSet[Var, Val] (csp: Problem[Var, Val],
                                   failed: Boolean = false)
extends AbstractHashAssignmentSet[Var, Val, HashAssignmentSet[Var, Val]
                                ](csp, failed)
{
  override def self: HashAssignmentSet[Var, Val] = this
}

object HashAssignmentSet {
  def failure[Var, Val](csp: Problem[Var, Val]): HashAssignmentSet[Var, Val] =
    new HashAssignmentSet[Var, Val](csp, true)
}
