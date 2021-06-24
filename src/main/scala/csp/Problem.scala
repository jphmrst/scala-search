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
 *  Elements of a CSP problem.
 */
trait Problem[Var, Val] {
  def variables: Seq[Var]
  def constraints: Seq[Constraint[Var, Val]]
}

object Problem {

  open class HashSetProblem[Var, Val](
    val variables: Seq[Var], val constraints: Seq[Constraint[Var, Val]])
  extends Problem[Var, Val]

}
