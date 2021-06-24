// Copyright (C) 2020 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.search.csp.examples
import org.maraist.search.csp.*
import org.maraist.search.csp.Problem.HashSetProblem
import org.maraist.search.csp.Constraint.Different
import org.maraist.search.csp.NoInference

object Color3 extends Enumeration {
  type Color3 = Value
  val Red, Green, Blue = Value
}
import Color3.*

object SimpleAustraliaColor3
extends Problem.HashSetProblem[String, Color3](
  Seq[String]("WA", "NT", "SA", "QLD", "NSW", "VIC", "ACT", "TAS"),
  Seq[Constraint[String, Color3]](
    Different("WA", "NT"), Different("WA", "SA"),
    Different("NT", "SA"), Different("VIC", "SA"),
    Different("QLD", "NT"), Different("QLD", "SA"),
    Different("QLD", "NSW"), Different("VIC", "NSW"),
    Different("ACT", "NSW"))
){
  type NoInf = NoInference[String, Color3, HashAssignmentSet[String, Color3]]
  private val noInference = NoInference[String, Color3,
                                        HashAssignmentSet[String, Color3]]

  def solver() = new Backtrack[HashAssignmentSet[String, Color3],
                               String, Color3, NoInf,
                               Problem.HashSetProblem[String, Color3]
                             ](new HashAssignmentSet[String, Color3](_),
                               _.isComplete,
                               _.unsetVariables.iterator.next(),
                               (_, _, _) => Seq[Color3](Red, Green,Blue),
                               (_,_) => noInference,
                               (_,_,_,_) => (),
                               (map,_) => map,
                               () => HashAssignmentSet.failure(this))

  def run: Unit = System.out.println(solver().search(SimpleAustraliaColor3))
}


