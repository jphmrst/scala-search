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
 *  One constraint from a CSP problem.
 */
trait Constraint[Var, Val] {
  def check[AS <: AssignmentSet[Var, Val, AS]](
    assignments: AssignmentSet[Var, Val, AS]
  ): Boolean
  def checkWith[AS <: AssignmentSet[Var, Val, AS]](
    assignments: AssignmentSet[Var, Val, AS], variable: Var, value: Val
  ): Boolean
}

object Constraint {

  class Different[Var, Val](val var1: Var, val var2: Var)
  extends Constraint[Var, Val] {
    def check[AS <: AssignmentSet[Var, Val, AS]](
      assignments: AssignmentSet[Var, Val, AS]
    ): Boolean = !assignments(var1).equals(assignments(var2))

    def checkWith[AS <: AssignmentSet[Var, Val, AS]](
      assignments: AssignmentSet[Var, Val, AS], variable: Var, value: Val
    ): Boolean =
      // First see if the var1 is the test variable
      (var1 == variable) match {
        case true => assignments(var2) match {
          case Some(val2) => !(value == val2)
            case None => true
        }
        case false => (var2 == variable) match {
          case true => assignments(var1) match {
            case Some(val1) => !(val1 == value)
              case None => true
          }
          case false => assignments(var1) match {
            case Some(val1) => assignments(var2) match {
              case Some(val2) => !(val1 == val2)
                case None => true
            }
            case None => true
          }
        }
      }

    override def toString(): String =
      var1.toString() ++ " != " ++ var2.toString()
  }

  object Different {
    def apply[Var, Val](v1: Var, v2: Var) = new Different[Var, Val](v1, v2)
  }
}
