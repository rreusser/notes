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
* Back-transform eigenvectors of a balanced pair of matrices.
*
* @module @stdlib/lapack/base/zggbak
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zggbak = require( '@stdlib/lapack/base/zggbak' );
*
* var LSCALE = new Float64Array( [ 1.0, 2.0 ] );
* var RSCALE = new Float64Array( [ 1.0, 2.0 ] );
* var V = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zggbak( 'row-major', 'none', 'left', 2, 1, 2, LSCALE, 1, RSCALE, 1, 2, V, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zggbak = require( '@stdlib/lapack/base/zggbak' );
*
* var LSCALE = new Float64Array( [ 1.0, 2.0 ] );
* var RSCALE = new Float64Array( [ 1.0, 2.0 ] );
* var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zggbak.ndarray( 'none', 'left', 2, 1, 2, LSCALE, 1, 0, RSCALE, 1, 0, 2, V, 1, 2, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zggbak.ndarray" }
