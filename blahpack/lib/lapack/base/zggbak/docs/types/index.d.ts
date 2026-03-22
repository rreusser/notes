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

// TypeScript declarations for @stdlib/lapack/base/zggbak

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Back-transform eigenvectors of a balanced pair of matrices.
	*/
	(
		job: string,
		side: string,
		N: number,
		ilo: number,
		ihi: number,
		LSCALE: Float64Array,
		strideLSCALE: number,
		offsetLSCALE: number,
		RSCALE: Float64Array,
		strideRSCALE: number,
		offsetRSCALE: number,
		M: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number
	): Float64Array;
}

/**
* Back-transform eigenvectors of a balanced pair of matrices.
*/
declare var zggbak: Routine;

export = zggbak;
