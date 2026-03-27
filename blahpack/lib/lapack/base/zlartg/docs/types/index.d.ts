/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

// TypeScript declarations for @stdlib/lapack/base/zlartg

import { Complex128Array } from '@stdlib/types/array';

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate a complex Givens plane rotation.
	*
	* @param f - first component (input only)
	* @param offsetF - index offset for f (in complex elements)
	* @param g - second component (input only)
	* @param offsetG - index offset for g (in complex elements)
	* @param c - on exit, the real cosine of the rotation
	* @param offsetC - index offset for c
	* @param s - on exit, the complex sine of the rotation
	* @param offsetS - index offset for s (in complex elements)
	* @param r - on exit, the complex result
	* @param offsetR - index offset for r (in complex elements)
	*/
	(
		f: Complex128Array,
		offsetF: number,
		g: Complex128Array,
		offsetG: number,
		c: Float64Array,
		offsetC: number,
		s: Complex128Array,
		offsetS: number,
		r: Complex128Array,
		offsetR: number
	): void;
}

/**
* Generate a complex Givens plane rotation.
*/
declare var zlartg: Routine;

export = zlartg;
