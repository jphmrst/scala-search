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
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.maraist.search.csp.*
import org.maraist.search.csp.examples.Color3.*
import scala.language.adhocExtensions

class Tests extends AnyFlatSpec {
  val solution = SimpleAustraliaColor3.solver().search(SimpleAustraliaColor3)

  "3-coloring Australia with simple hash backtracker" `should` "find correct colors" in {
    assert(solution("VIC").get === Red)
    assert(solution("ACT").get === Red)
    assert(solution("NSW").get === Green)
    assert(solution("NT").get === Red)
    assert(solution("TAS").get === Red)
    assert(solution("WA").get === Blue)
    assert(solution("QLD").get === Blue)
    assert(solution("SA").get === Green)
  }
}
