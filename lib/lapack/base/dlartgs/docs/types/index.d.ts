/*
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

// TypeScript Version: 4.1

/// <reference types="@stdlib/types"/>



/**
* Interface describing `dlartgs`.
*/
interface Routine {
	/**
	* Generates a plane rotation for the bidiagonal SVD implicit QR iteration
	*
	* @param x - x
	* @param y - y
	* @param sigma - sigma
	* @param cs - cs
	* @param sn - sn
	*/
	( x: number, y: number, sigma: number, cs: number, sn: number ): void;

	/**
	* Generates a plane rotation for the bidiagonal SVD implicit QR iteration, using alternative indexing semantics.
	*
	* @param x - x
	* @param y - y
	* @param sigma - sigma
	* @param cs - cs
	* @param sn - sn
	*/
	ndarray( x: number, y: number, sigma: number, cs: number, sn: number ): void;
}

/**
* Generates a plane rotation for the bidiagonal SVD implicit QR iteration
*/
declare var dlartgs: Routine;


// EXPORTS //

export = dlartgs;
