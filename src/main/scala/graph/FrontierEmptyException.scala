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

/**
 *  Thrown when a frontier structure is empty.  Generally this
 *  exception indicates something wrong in the implementation of the
 *  frontier, or else something very wrong in the
 *  {@link GraphSearcher#search search} method: this method arises from the
 *  {@link Frontier#pop pop} method of the frontier
 *  representation, but {@link Frontier#pop pop} is only
 *  called after verifying that {@link Frontier#isEmpty isEmpty}
 *  is false.
 */
class FrontierEmptyException(cause: Throwable)
extends IllegalStateException(FrontierEmptyException.MESSAGE, cause) {
  /**
   *  Used when the underlying data structure of the frontier throws
   *  an exception.
   *
   * @param cause Exeception caught from a method call on the
   * underlying structure on behalf of the {@link
   * Frontier#pop pop} method.
   */
  def this() = this(null)
}

object FrontierEmptyException {
  val MESSAGE = "Frontier (unexpectedly) empty"
}
