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

/**
* Interchange two complex double-precision vectors.
*
* @module @stdlib/blas/base/zswap
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zswap = require( '@stdlib/blas/base/zswap' );
* 
* var zx = new Complex128Array( [ 1.0, 2.0 ] );
* var zy = new Complex128Array( [ 1.0, 2.0 ] );
* 
* zswap( 2, zx, 1, zy, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zswap = require( '@stdlib/blas/base/zswap' );
* 
* var zx = new Float64Array( [ 1.0, 2.0 ] );
* var zy = new Float64Array( [ 1.0, 2.0 ] );
* 
* zswap.ndarray( 2, zx, 1, 0, zy, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zswap.ndarray" }
