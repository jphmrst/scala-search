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
import java.util.ArrayList

trait SearchTreePathNode[This <: SearchTreePathNode[This,S], S]
extends SearchTreeNode[This, S] {

  val parent: Option[This]

  def statePath(): ArrayList[S] = statePath(new ArrayList[S]())
  def statePath(states: ArrayList[S]): ArrayList[S] = {
    parent match {
      case None => { }
      case Some(p) => {
        p.statePath(states)
      }
    }
    states.add(state)
    states
  }

  def pathToString(): String = {
    parent match {
      case None => state.toString()
      case Some(p) => p.pathToString() + " >> " + state.toString()
    }
  }
}

